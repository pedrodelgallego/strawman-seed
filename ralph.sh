#!/usr/bin/env bash
# Ralph Loop — autonomous TDD driver for Strawman
# Continuously reads plan.md, finds the next unchecked item, and invokes
# Claude Code to execute one RED→GREEN TDD cycle per iteration.
#
# Usage:
#   ./ralph.sh            # run the loop
#   ./ralph.sh --dry-run  # show what would be done without invoking claude

set -euo pipefail

# ── Colors ──────────────────────────────────────────────────────────────

if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    CYAN='\033[0;36m'
    BOLD='\033[1m'
    DIM='\033[2m'
    RESET='\033[0m'
else
    RED='' GREEN='' YELLOW='' BLUE='' CYAN='' BOLD='' DIM='' RESET=''
fi

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
PLAN="$PROJECT_DIR/plan.md"
SPEC="$PROJECT_DIR/spec.md"
LOG="$PROJECT_DIR/ralph.log"
LOCK_FILE="$PROJECT_DIR/.ralph.lock"
MAX_RETRIES=3
MAX_LOG_BYTES=$((10 * 1024 * 1024))  # 10 MB
CLAUDE_TIMEOUT=600                    # 10 minutes per task
MAX_COST_USD=50                       # Session cost cap ($)
MAX_ITERATIONS=200                    # Safety cap on loop iterations
DRY_RUN=false
CONFIG="$PROJECT_DIR/config.json"
CONTEXT_FILE="$PROJECT_DIR/context.md"

# Session-level counters for exit summary
TASKS_COMPLETED=0
TASKS_FAILED=0
TOTAL_COST_USD="0"

if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
fi

# ── Lock file ───────────────────────────────────────────────────────────

acquire_lock() {
    if [[ -f "$LOCK_FILE" ]]; then
        local lock_pid
        lock_pid=$(<"$LOCK_FILE")
        if kill -0 "$lock_pid" 2>/dev/null; then
            echo -e "${RED}ERROR: Another ralph instance is running (PID $lock_pid).${RESET}" >&2
            echo -e "${DIM}Remove $LOCK_FILE if this is stale.${RESET}" >&2
            exit 1
        fi
        echo -e "${YELLOW}WARNING: Stale lock file found (PID $lock_pid dead). Removing.${RESET}" >&2
    fi
    echo $$ > "$LOCK_FILE"
}

release_lock() {
    rm -f "$LOCK_FILE" 2>/dev/null || true
}

# ── Cleanup trap ────────────────────────────────────────────────────────

cleanup() {
    local exit_code=$?
    # Print session summary if we ran long enough to have stats
    if [[ -n "${RALPH_START_TIME:-}" ]]; then
        local now elapsed_s elapsed_m
        now=$(date +%s)
        elapsed_s=$((now - RALPH_START_TIME))
        elapsed_m=$((elapsed_s / 60))

        echo ""
        echo -e "${BOLD}${BLUE}═══════════════════════════════════════════${RESET}"
        echo -e "  ${BOLD}SESSION SUMMARY${RESET}"
        echo -e "  ${GREEN}Completed:${RESET} $TASKS_COMPLETED  ${RED}Failed:${RESET} $TASKS_FAILED  ${DIM}Time:${RESET} ${elapsed_m}m"
        if [[ "$TOTAL_COST_USD" != "0" ]]; then
            echo -e "  ${YELLOW}Cost:${RESET} \$${TOTAL_COST_USD}"
        fi
        echo -e "${BOLD}${BLUE}═══════════════════════════════════════════${RESET}"
        echo ""
    fi

    # Clean up temp files
    rm -f "$PROJECT_DIR"/logs/.snapshot-$$.* 2>/dev/null || true
    rm -f "$PROJECT_DIR"/logs/.snapshot-$$ 2>/dev/null || true
    rm -f /tmp/.ralph-cost-$$ 2>/dev/null || true
    rm -f "${PLAN}.tmp.$$" "${PLAN}.tmp.$$.new" 2>/dev/null || true
    release_lock
    exit "$exit_code"
}

trap cleanup EXIT INT TERM

# ── Portable timeout ─────────────────────────────────────────────────────

# Use GNU timeout if available; otherwise fall back to a background-job
# approach that works on stock macOS.
if command -v timeout &>/dev/null; then
    ralph_timeout() { timeout "$@"; }
elif command -v gtimeout &>/dev/null; then
    ralph_timeout() { gtimeout "$@"; }
else
    ralph_timeout() {
        local secs="$1"; shift
        "$@" &
        local pid=$!
        ( sleep "$secs" && kill -TERM "$pid" 2>/dev/null ) &
        local watchdog=$!
        wait "$pid" 2>/dev/null
        local ret=$?
        kill "$watchdog" 2>/dev/null
        wait "$watchdog" 2>/dev/null || true
        # If the process was killed by our watchdog, mimic timeout exit code
        if [[ $ret -eq 143 ]]; then  # 128+15 = SIGTERM
            return 124
        fi
        return "$ret"
    }
fi

# ── Log rotation ────────────────────────────────────────────────────────

rotate_log() {
    [[ ! -f "$LOG" ]] && return 0
    local size
    size=$(wc -c < "$LOG" 2>/dev/null || echo 0)
    if [[ $size -gt $MAX_LOG_BYTES ]]; then
        mv "$LOG" "${LOG}.1"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Log rotated (previous log: ${LOG}.1)" > "$LOG"
    fi
}

# ── Disk space check ─────────────────────────────────────────────────────

check_disk_space() {
    local min_mb=100
    local avail_kb
    avail_kb=$(df -k "$PROJECT_DIR" | awk 'NR==2 {print $4}')
    if [[ -n "$avail_kb" && "$avail_kb" -lt $((min_mb * 1024)) ]]; then
        log "${RED}ERROR:${RESET} Less than ${min_mb}MB disk space remaining (${avail_kb}KB). Stopping to prevent corruption."
        exit 1
    fi
}

# ── Configuration ────────────────────────────────────────────────────────

load_config() {
    if ! command -v jq &>/dev/null; then
        echo -e "${RED}ERROR: jq is required but not installed.${RESET}" >&2
        echo -e "  macOS:  ${CYAN}brew install jq${RESET}" >&2
        echo -e "  Linux:  ${CYAN}apt install jq${RESET}" >&2
        exit 1
    fi

    if [[ ! -f "$CONFIG" ]]; then
        echo -e "${RED}ERROR: config.json not found at $CONFIG${RESET}" >&2
        exit 1
    fi

    if ! jq empty "$CONFIG" 2>/dev/null; then
        echo -e "${RED}ERROR: config.json is not valid JSON${RESET}" >&2
        exit 1
    fi

    LANG_NAME=$(jq -r '.language' "$CONFIG")
    FILE_EXT=$(jq -r '.file_extension' "$CONFIG")
    RUN_CMD=$(jq -r '.run_command' "$CONFIG")
    TEST_CMD=$(jq -r '.test_command' "$CONFIG")
    TEST_DIR_CMD=$(jq -r '.test_dir_command' "$CONFIG")
    TEST_FRAMEWORK=$(jq -r '.test_framework' "$CONFIG")
    SOURCE_DIR=$(jq -r '.source_dir // "src"' "$CONFIG")
    TEST_DIR=$(jq -r '.test_dir // "tests"' "$CONFIG")
    ENTRY_POINT=$(jq -r '.entry_point' "$CONFIG")
    TEST_FILE_PATTERN=$(jq -r '.test_file_pattern // "test-{module}"' "$CONFIG")
    SOURCE_EXT=$(jq -r '.source_extension // ".straw"' "$CONFIG")
    MODULE_INSTRUCTIONS=$(jq -r '.module_instructions' "$CONFIG")
    FILE_PREAMBLE=$(jq -r '.file_preamble // ""' "$CONFIG")

    # Optional quality gates
    LINT_CMD=$(jq -r '.lint_command // ""' "$CONFIG")
    COVERAGE_CMD=$(jq -r '.coverage_command // ""' "$CONFIG")
    COVERAGE_THRESHOLD=$(jq -r '.coverage_threshold // 0' "$CONFIG")
    INTEGRATION_TEST_CMD=$(jq -r '.integration_test_command // ""' "$CONFIG")

    local required_fields=("language" "file_extension" "run_command" "test_command" "test_dir_command" "test_framework" "entry_point" "module_instructions")
    for field in "${required_fields[@]}"; do
        local val
        val=$(jq -r ".$field // empty" "$CONFIG")
        if [[ -z "$val" ]]; then
            echo -e "${RED}ERROR: Required config field '$field' is missing in config.json${RESET}" >&2
            exit 1
        fi
    done
}

# ── Pre-flight checks ────────────────────────────────────────────────────

preflight_checks() {
    local missing=0
    for cmd in claude jq git; do
        if ! command -v "$cmd" &>/dev/null; then
            echo -e "${RED}ERROR: '$cmd' is required but not found in PATH${RESET}" >&2
            missing=1
        fi
    done
    if [[ $missing -ne 0 ]]; then exit 1; fi
}

check_git_clean() {
    local dirty
    dirty=$(cd "$PROJECT_DIR" && git diff --name-only -- "$SOURCE_DIR/" "$TEST_DIR/" 2>/dev/null || true)
    local untracked
    untracked=$(cd "$PROJECT_DIR" && git ls-files --others --exclude-standard -- "$SOURCE_DIR/" "$TEST_DIR/" 2>/dev/null || true)

    if [[ -n "$dirty" || -n "$untracked" ]]; then
        log "${YELLOW}WARNING:${RESET} Uncommitted changes in $SOURCE_DIR/ or $TEST_DIR/:"
        [[ -n "$dirty" ]] && echo "$dirty" | while IFS= read -r f; do log "  ${YELLOW}modified:${RESET} $f"; done
        [[ -n "$untracked" ]] && echo "$untracked" | while IFS= read -r f; do log "  ${YELLOW}untracked:${RESET} $f"; done
        log ""
        log "${RED}Ralph resets the working tree between retries and on failure.${RESET}"
        log "${RED}These changes WILL be lost. Commit or stash them first.${RESET}"
        exit 1
    fi
}

show_plan_summary() {
    local total checked remaining
    total=$(grep -c '^ *- \[.\]' "$PLAN" || true)
    checked=$(grep -c '^ *- \[x\]' "$PLAN" || true)
    [[ -z "$total" ]] && total=0
    [[ -z "$checked" ]] && checked=0
    remaining=$((total - checked))

    local claude_ver
    claude_ver=$(claude --version 2>/dev/null) || claude_ver="unknown"

    log "${DIM}Language:${RESET} $LANG_NAME ${DIM}|${RESET} ${DIM}Test:${RESET} $TEST_DIR_CMD ${DIM}|${RESET} ${DIM}Framework:${RESET} $TEST_FRAMEWORK"
    log "${DIM}Claude CLI:${RESET} $claude_ver"
    log "${CYAN}Plan:${RESET} ${BOLD}$checked/$total${RESET} tasks done, ${BOLD}$remaining${RESET} remaining"

    # Verify test command binary is available
    local test_bin
    test_bin=$(echo "$TEST_DIR_CMD" | awk '{print $1}')
    if ! command -v "$test_bin" &>/dev/null; then
        echo -e "${RED}ERROR: Test command '$test_bin' not found in PATH${RESET}" >&2
        exit 1
    fi
}

# ── Logging ──────────────────────────────────────────────────────────────

log() {
    local ts
    ts=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${DIM}[$ts]${RESET} $1"
    # Strip color codes for the log file
    local plain
    plain=$(echo -e "[$ts] $1" | sed $'s/\033\\[[0-9;]*m//g')
    echo "$plain" >> "$LOG"
}

# ── Progress dashboard ───────────────────────────────────────────────────

show_progress() {
    local task_text="$1"
    local attempt="$2"

    local total checked remaining pct
    total=$(grep -c '^ *- \[.\]' "$PLAN" || true)
    checked=$(grep -c '^ *- \[x\]' "$PLAN" || true)
    [[ -z "$total" ]] && total=0
    [[ -z "$checked" ]] && checked=0
    remaining=$((total - checked))
    if [[ $total -gt 0 ]]; then
        pct=$((checked * 100 / total))
    else
        pct=0
    fi

    local now elapsed_s elapsed_m
    now=$(date +%s)
    elapsed_s=$((now - RALPH_START_TIME))
    elapsed_m=$((elapsed_s / 60))

    local bar_width=30
    local filled=$((pct * bar_width / 100))
    local empty=$((bar_width - filled))
    local bar=""
    local i
    for ((i=0; i<filled; i++)); do bar+="█"; done
    for ((i=0; i<empty; i++)); do bar+="░"; done

    echo ""
    echo -e "${BOLD}${BLUE}════════════════════════════════════════════════════════════${RESET}"
    echo -e "  ${BOLD}RALPH LOOP${RESET} ${DIM}|${RESET} ${GREEN}${checked}${RESET}/${total} tasks ${DIM}(${pct}%)${RESET} ${DIM}|${RESET} ${elapsed_m}m elapsed"
    echo -e "  ${GREEN}${bar:0:$filled}${DIM}${bar:$filled}${RESET}"
    echo -e "  ${CYAN}Next:${RESET} ${task_text}"
    if [[ "$attempt" -gt 1 ]]; then
        echo -e "  ${YELLOW}Retry:${RESET} #${attempt} of ${MAX_RETRIES}"
    fi
    echo -e "${BOLD}${BLUE}════════════════════════════════════════════════════════════${RESET}"
    echo ""
}

# ── Context extraction ───────────────────────────────────────────────────

# Given a line number in plan.md, walk backwards to find the nearest
# Phase header (## Phase ...) and Story header (**E...).
get_context() {
    local line_num="$1"
    local phase_header=""
    local story_header=""
    local stories_line=""

    # Read lines above the checkbox, bottom-up
    local i=$((line_num - 1))
    while [[ $i -ge 1 ]]; do
        local line
        line=$(sed -n "${i}p" "$PLAN")

        # Story header: **E1.4 — ... or **E2.1 — ... (bold story labels)
        if [[ -z "$story_header" && "$line" =~ ^\*\*E[0-9] ]]; then
            story_header="$line"
        fi

        # Stories line: *Stories: E1.1* (italic, under phase header)
        if [[ -z "$stories_line" && "$line" =~ ^\*Stories: ]]; then
            stories_line="$line"
        fi

        # Phase header: ## Phase N — ...
        if [[ -z "$phase_header" && "$line" =~ ^##\  ]]; then
            phase_header="$line"
            # If no story header found, use the stories line as context
            if [[ -z "$story_header" && -n "$stories_line" ]]; then
                story_header="$stories_line"
            fi
            break
        fi

        i=$((i - 1))
    done

    # Fallback: if still no story header, use phase header
    if [[ -z "$story_header" ]]; then
        story_header="$phase_header"
    fi

    echo "$phase_header"
    echo "$story_header"
}

# ── Extract story ID ────────────────────────────────────────────────────

# Extracts a clean story ID (e.g., "E1.1") from story header and checkbox text.
# Tries checkbox text first (Phase 8+ embeds story ID), then story header.
extract_story_id() {
    local story_header="$1"
    local checkbox_text="${2:-}"

    # Try checkbox text first (Phase 8+ format: **E4.1** — ...)
    if [[ -n "$checkbox_text" && "$checkbox_text" =~ E([0-9]+\.[0-9]+) ]]; then
        echo "E${BASH_REMATCH[1]}"
        return
    fi

    # Then try story header (**E1.4 — ... or *Stories: E1.1*)
    if [[ "$story_header" =~ E([0-9]+\.[0-9]+) ]]; then
        echo "E${BASH_REMATCH[1]}"
        return
    fi

    echo ""
}

# ── Extract Test Matrix from spec.md ─────────────────────────────────────

# Given a story ID (e.g., "E1.1"), extracts the Test Matrix table from spec.md.
extract_test_matrix() {
    local story_id="$1"
    local spec="$PROJECT_DIR/spec.md"

    # Find story header line (### E1.1 — ...)
    local header_line
    header_line=$(grep -n "^### ${story_id} " "$spec" | head -1 | cut -d: -f1)

    if [[ -z "$header_line" ]]; then
        echo "(Test Matrix not found for $story_id in spec.md)"
        return
    fi

    # Find "#### Test Matrix" within the next 60 lines
    local matrix_offset
    matrix_offset=$(tail -n "+$header_line" "$spec" | head -60 | grep -n '^#### Test Matrix' | head -1 | cut -d: -f1)

    if [[ -z "$matrix_offset" ]]; then
        echo "(No Test Matrix section for $story_id)"
        return
    fi

    local matrix_start=$((header_line + matrix_offset - 1))

    # Find next section boundary after the Test Matrix
    local end_offset
    end_offset=$(tail -n "+$((matrix_start + 1))" "$spec" | grep -n '^####\|^###\|^---' | head -1 | cut -d: -f1)

    if [[ -z "$end_offset" ]]; then
        # No end marker found; take 30 lines as fallback
        tail -n "+$matrix_start" "$spec" | head -30
    else
        tail -n "+$matrix_start" "$spec" | head -n "$end_offset"
    fi
}

# ── Extract Acceptance Criteria from spec.md ──────────────────────────────

# Given a story ID (e.g., "E1.1"), extracts the Acceptance Criteria section.
extract_acceptance_criteria() {
    local story_id="$1"
    local spec="$PROJECT_DIR/spec.md"

    # Find story header line (### E1.1 — ...)
    local header_line
    header_line=$(grep -n "^### ${story_id} " "$spec" | head -1 | cut -d: -f1)
    [[ -z "$header_line" ]] && return

    # Find "#### Acceptance Criteria" within the next 80 lines
    local ac_offset
    ac_offset=$(tail -n "+$header_line" "$spec" | head -80 | grep -n '^#### Acceptance Criteria' | head -1 | cut -d: -f1)
    [[ -z "$ac_offset" ]] && return

    local ac_start=$((header_line + ac_offset - 1))

    # Find next section boundary
    local end_offset
    end_offset=$(tail -n "+$((ac_start + 1))" "$spec" | grep -n '^####\|^###\|^---' | head -1 | cut -d: -f1)

    if [[ -z "$end_offset" ]]; then
        tail -n "+$ac_start" "$spec" | head -20
    else
        tail -n "+$ac_start" "$spec" | head -n "$end_offset"
    fi
}

# ── Validate context.md ──────────────────────────────────────────────────

# Verify context.md has the expected structure and is within size limits.
validate_context() {
    [[ ! -f "$CONTEXT_FILE" ]] && return 0

    local lines
    lines=$(wc -l < "$CONTEXT_FILE")
    if [[ $lines -gt 100 ]]; then
        log "${YELLOW}WARNING:${RESET} context.md is $lines lines (limit: 100). It may get truncated in prompts."
    fi

    # Verify required sections exist
    for section in "## Current State" "## Conventions & Decisions" "## Gotchas"; do
        if ! grep -q "$section" "$CONTEXT_FILE"; then
            log "${YELLOW}WARNING:${RESET} context.md is missing section: $section"
        fi
    done
}

# ── Find next unchecked item ─────────────────────────────────────────────

# Returns line number and text of the first `- [ ]` in plan.md.
# Returns empty if plan is complete.
find_next_item() {
    # Find first unchecked `- [ ]` line, whether indented or not
    local match
    match=$(grep -n '^ *- \[ \]' "$PLAN" | head -1) || true
    echo "$match"
}

# ── Mark item as done ────────────────────────────────────────────────────

mark_done() {
    local line_num="$1"
    # Backup plan.md before modifying (atomic: write temp, then move)
    local tmp="${PLAN}.tmp.$$"
    cp "$PLAN" "$tmp"
    if [[ "$(uname)" == "Darwin" ]]; then
        sed "${line_num}s/- \[ \]/- [x]/" "$tmp" > "${tmp}.new"
    else
        sed "${line_num}s/- \[ \]/- [x]/" "$tmp" > "${tmp}.new"
    fi
    mv "${tmp}.new" "$PLAN"
    rm -f "$tmp"
}

# ── Check if a story block is fully done ─────────────────────────────────

# Look ahead from current position: if no more `- [ ]` before the next
# **REFACTOR** or **COMMIT** or next story header, the story is complete.
check_story_complete() {
    local line_num="$1"
    local remaining
    remaining=$(tail -n "+$line_num" "$PLAN" | grep -c '^ *- \[ \]' || true)

    # Check only until the next phase header
    local next_phase_line
    next_phase_line=$(tail -n "+$((line_num + 1))" "$PLAN" | grep -n '^## Phase' | head -1 | cut -d: -f1 || true)

    if [[ -n "$next_phase_line" ]]; then
        remaining=$(tail -n "+$line_num" "$PLAN" | head -n "$next_phase_line" | grep -c '^ *- \[ \]' || true)
    fi

    [[ "$remaining" -eq 0 ]]
}

# ── Check if a phase (epic) is fully done ────────────────────────────────

check_phase_complete() {
    local line_num="$1"

    # Find the start of the current phase
    local phase_start
    phase_start=$(head -n "$line_num" "$PLAN" | grep -n '^## Phase' | tail -1 | cut -d: -f1 || true)
    [[ -z "$phase_start" ]] && return 1

    # Find the next phase header after current one
    local next_phase_offset
    next_phase_offset=$(tail -n "+$((phase_start + 1))" "$PLAN" | grep -n '^## Phase' | head -1 | cut -d: -f1 || true)

    local unchecked
    if [[ -n "$next_phase_offset" ]]; then
        local phase_end=$((phase_start + next_phase_offset))
        unchecked=$(sed -n "${phase_start},${phase_end}p" "$PLAN" | grep -c '^ *- \[ \]' || true)
    else
        unchecked=$(tail -n "+$phase_start" "$PLAN" | grep -c '^ *- \[ \]' || true)
    fi

    [[ "$unchecked" -eq 0 ]]
}

# ── Stream filter ────────────────────────────────────────────────────────

# Reads NDJSON from claude --output-format stream-json on stdin and prints
# a compact, human-readable activity feed to stdout.
ralph_stream_filter() {
    local turn_count=0
    local parse_errors=0
    local total_lines=0

    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        total_lines=$((total_lines + 1))

        local msg_type
        msg_type=$(echo "$line" | jq -r '.type // empty' 2>/dev/null)
        if [[ $? -ne 0 || -z "$msg_type" ]]; then
            parse_errors=$((parse_errors + 1))
            if [[ $parse_errors -ge 10 && $total_lines -gt 0 ]]; then
                local err_pct=$((parse_errors * 100 / total_lines))
                if [[ $err_pct -ge 50 ]]; then
                    echo -e "  ${RED}[error]${RESET} Too many stream parse failures ($parse_errors/$total_lines) — aborting filter" >&2
                    return 1
                fi
            fi
            continue
        fi

        case "$msg_type" in
            system)
                local subtype model
                subtype=$(echo "$line" | jq -r '.subtype // empty' 2>/dev/null)
                if [[ "$subtype" == "init" ]]; then
                    model=$(echo "$line" | jq -r '.model // "unknown"' 2>/dev/null)
                    echo -e "  ${DIM}[init]${RESET} model=${CYAN}$model${RESET}"
                fi
                ;;
            assistant)
                turn_count=$((turn_count + 1))
                # Show tool_use calls with key input
                local tools
                tools=$(echo "$line" | jq -r '
                    .message.content[]?
                    | select(.type == "tool_use")
                    | "\(.name): \(
                        if .name == "Read" or .name == "Write" or .name == "Edit" then (.input.file_path // "?")
                        elif .name == "Bash" then (.input.command // "?" | .[0:100])
                        elif .name == "Glob" then (.input.pattern // "?")
                        elif .name == "Grep" then (.input.pattern // "?")
                        else (.input | keys | join(", "))
                        end
                    )"' 2>/dev/null)
                if [[ -n "$tools" ]]; then
                    echo "$tools" | while IFS= read -r tline; do
                        local tool_name="${tline%%:*}"
                        local tool_arg="${tline#*: }"
                        echo -e "  ${DIM}[turn $turn_count]${RESET} ${BLUE}${tool_name}${RESET}: ${DIM}${tool_arg}${RESET}"
                    done
                fi

                # Show first 120 chars of text responses
                local text
                text=$(echo "$line" | jq -r '
                    .message.content[]?
                    | select(.type == "text")
                    | .text' 2>/dev/null | head -c 120)
                if [[ -n "$text" ]]; then
                    echo -e "  ${DIM}[turn $turn_count]${RESET} $text"
                fi
                ;;
            result)
                local num_turns total_cost duration_ms subtype
                num_turns=$(echo "$line" | jq -r '.num_turns // "?"' 2>/dev/null)
                total_cost=$(echo "$line" | jq -r '.total_cost_usd // "?"' 2>/dev/null)
                duration_ms=$(echo "$line" | jq -r '.duration_ms // "?"' 2>/dev/null)
                subtype=$(echo "$line" | jq -r '.subtype // "?"' 2>/dev/null)
                local status_color="$GREEN"
                [[ "$subtype" == "error"* ]] && status_color="$RED"
                echo -e "  ${BOLD}[done]${RESET} turns=${num_turns} cost=${YELLOW}\$${total_cost}${RESET} time=${duration_ms}ms status=${status_color}${subtype}${RESET}"
                # Export cost for session tracking
                if [[ "$total_cost" != "?" ]]; then
                    echo "$total_cost" > /tmp/.ralph-cost-$$
                fi
                ;;
        esac
    done

    if [[ $parse_errors -gt 0 ]]; then
        echo -e "  ${YELLOW}[warn]${RESET} ${parse_errors}/${total_lines} stream lines failed to parse"
    fi
}

# ── Test failure diagnostics ─────────────────────────────────────────────

parse_test_failures() {
    local output="$1"

    # Extract summary lines (e.g., "N tests failed", "N passed")
    local summary
    summary=$(echo "$output" | grep -iE '[0-9]+ (tests?|checks?) (failed|passed|run)' | tail -3)
    if [[ -n "$summary" ]]; then
        log "${BOLD}Test summary:${RESET}"
        echo "$summary" | while IFS= read -r sline; do
            log "  ${DIM}$sline${RESET}"
        done
    fi

    # Extract FAILURE blocks (rackunit style)
    local failures
    failures=$(echo "$output" | grep -A 4 'FAILURE' | grep -E '(name:|actual:|expected:)' | head -20)
    if [[ -n "$failures" ]]; then
        log "${RED}Failing tests:${RESET}"
        echo "$failures" | while IFS= read -r fline; do
            log "  ${RED}$fline${RESET}"
        done
    fi

    # Extract ERROR lines
    local errors
    errors=$(echo "$output" | grep -iE '^(ERROR|error:|Exception)' | head -5)
    if [[ -n "$errors" ]]; then
        log "${RED}Errors:${RESET}"
        echo "$errors" | while IFS= read -r eline; do
            log "  ${RED}$eline${RESET}"
        done
    fi
}

# ── TDD discipline verification ──────────────────────────────────────────

# Capture file hashes and baseline test exit code before Claude runs.
snapshot_test_state() {
    local snapshot_file="$1"
    # Save hashes of all test and source files
    (cd "$PROJECT_DIR" && {
        find "$SOURCE_DIR" "$TEST_DIR" -name "*$FILE_EXT" -exec md5 -q {} \; 2>/dev/null || \
        find "$SOURCE_DIR" "$TEST_DIR" -name "*$FILE_EXT" -exec md5sum {} \; 2>/dev/null
    }) > "$snapshot_file" 2>/dev/null || true

    # Also capture list of files for diffing
    (cd "$PROJECT_DIR" && {
        find "$SOURCE_DIR" -name "*$FILE_EXT" 2>/dev/null | sort
    }) > "${snapshot_file}.src_files" || true
    (cd "$PROJECT_DIR" && {
        find "$TEST_DIR" -name "*$FILE_EXT" 2>/dev/null | sort
    }) > "${snapshot_file}.test_files" || true

    # Capture baseline test exit code
    local baseline_exit=0
    if ls "$PROJECT_DIR"/$TEST_DIR/*$FILE_EXT 1>/dev/null 2>&1; then
        (cd "$PROJECT_DIR" && eval "$TEST_DIR_CMD") > /dev/null 2>&1 || baseline_exit=$?
    fi
    echo "BASELINE_EXIT=$baseline_exit" >> "$snapshot_file"
}

# Post-Claude check: verify test files were modified (RED happened),
# source files were modified (GREEN happened), and no regressions in
# unmodified test files.
verify_tdd_discipline() {
    local snapshot_file="$1"
    local tdd_issues=""

    # a. Check test files were modified or added (RED phase)
    local old_test_files new_test_files
    old_test_files="${snapshot_file}.test_files"
    new_test_files="${snapshot_file}.test_files_after"
    (cd "$PROJECT_DIR" && find "$TEST_DIR" -name "*$FILE_EXT" 2>/dev/null | sort) > "$new_test_files" || true

    local test_diff
    test_diff=$(diff "$old_test_files" "$new_test_files" 2>/dev/null || true)
    local test_modified
    test_modified=$(cd "$PROJECT_DIR" && git diff --name-only -- "$TEST_DIR/" 2>/dev/null || true)
    local test_new
    test_new=$(cd "$PROJECT_DIR" && git ls-files --others --exclude-standard -- "$TEST_DIR/" 2>/dev/null || true)

    if [[ -z "$test_diff" && -z "$test_modified" && -z "$test_new" ]]; then
        tdd_issues="${tdd_issues}No test files were written or modified (RED phase missing). "
        log "${YELLOW}TDD WARNING:${RESET} No test files changed — RED phase may have been skipped"
    fi

    # b. Check source files were modified or added (GREEN phase)
    local old_src_files new_src_files
    old_src_files="${snapshot_file}.src_files"
    new_src_files="${snapshot_file}.src_files_after"
    (cd "$PROJECT_DIR" && find "$SOURCE_DIR" -name "*$FILE_EXT" 2>/dev/null | sort) > "$new_src_files" || true

    local src_diff
    src_diff=$(diff "$old_src_files" "$new_src_files" 2>/dev/null || true)
    local src_modified
    src_modified=$(cd "$PROJECT_DIR" && git diff --name-only -- "$SOURCE_DIR/" 2>/dev/null || true)
    local src_new
    src_new=$(cd "$PROJECT_DIR" && git ls-files --others --exclude-standard -- "$SOURCE_DIR/" 2>/dev/null || true)

    if [[ -z "$src_diff" && -z "$src_modified" && -z "$src_new" ]]; then
        log "${YELLOW}TDD WARNING:${RESET} No source files changed — tests may pass without new code"
    fi

    # c. Regression detection: run only unmodified test files
    local baseline_exit
    baseline_exit=$(grep 'BASELINE_EXIT=' "$snapshot_file" 2>/dev/null | cut -d= -f2 || echo "1")

    if [[ "$baseline_exit" == "0" ]]; then
        local changed_tests="${test_modified}"$'\n'"${test_new}"
        local all_tests
        all_tests=$(find "$PROJECT_DIR/$TEST_DIR" -name "*$FILE_EXT" 2>/dev/null)

        local regression_found=false
        while IFS= read -r test_file; do
            [[ -z "$test_file" ]] && continue
            local relative
            relative=$(echo "$test_file" | sed "s|$PROJECT_DIR/||")
            if ! echo "$changed_tests" | grep -q "$relative"; then
                # This test file was NOT modified by Claude — run it
                local reg_exit=0
                (cd "$PROJECT_DIR" && eval "$TEST_CMD $relative") > /dev/null 2>&1 || reg_exit=$?
                if [[ $reg_exit -ne 0 ]]; then
                    log "${RED}REGRESSION:${RESET} $relative fails but was not modified by Claude"
                    tdd_issues="${tdd_issues}Regression in $relative. "
                    regression_found=true
                fi
            fi
        done <<< "$all_tests"
    fi

    # Return result
    if [[ -n "$tdd_issues" ]]; then
        log "${YELLOW}TDD DISCIPLINE ISSUES:${RESET} $tdd_issues"
        echo "$tdd_issues"
        return 1
    fi
    return 0
}

# ── Git state management ─────────────────────────────────────────────────

# Reset src/ and tests/ to the last committed state. Used between retries
# so each attempt starts clean, and on final failure so the human sees a
# clean working tree instead of a tangle of failed attempts.
reset_working_tree() {
    (cd "$PROJECT_DIR" && {
        git checkout -- "$SOURCE_DIR/" "$TEST_DIR/" 2>/dev/null || true
        git clean -fd "$SOURCE_DIR/" "$TEST_DIR/" 2>/dev/null || true
    })
    log "${YELLOW}Working tree reset to last committed state${RESET}"
}

# ── Scope enforcement ────────────────────────────────────────────────────

# Reverts changes Claude made outside the expected SOURCE_DIR/ and TEST_DIR/.
check_scope() {
    local changed_files
    changed_files=$(cd "$PROJECT_DIR" && {
        git diff --name-only 2>/dev/null
        git ls-files --others --exclude-standard 2>/dev/null
    })
    [[ -z "$changed_files" ]] && return 0

    local reverted=false
    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        if [[ "$file" != "$SOURCE_DIR/"* && "$file" != "$TEST_DIR/"* && "$file" != "context.md" && "$file" != "suggestions.md" ]]; then
            log "${RED}SCOPE VIOLATION:${RESET} Reverting unauthorized change to ${BOLD}$file${RESET}"
            (cd "$PROJECT_DIR" && {
                # Revert tracked files; delete untracked ones
                git checkout -- "$file" 2>/dev/null || rm -f "$file" 2>/dev/null || true
            })
            reverted=true
        fi
    done <<< "$changed_files"

    if [[ "$reverted" == true ]]; then
        log "${YELLOW}WARNING:${RESET} Unauthorized file changes were reverted. Only $SOURCE_DIR/, $TEST_DIR/, context.md, suggestions.md are allowed."
    fi
}

# ── Build the prompt ─────────────────────────────────────────────────────

build_prompt() {
    local checkbox_text="$1"
    local phase_header="$2"
    local story_header="$3"
    local story_id="${4:-}"
    local failure_context="${5:-}"

    cat <<PROMPT
You are working on the Strawman Lisp interpreter in $LANG_NAME. Follow strict TDD.

PROJECT DIRECTORY: $PROJECT_DIR

CURRENT TASK: $checkbox_text
PHASE: $phase_header
STORY: $story_header

$(if [[ -n "$story_id" ]]; then
    echo "TEST MATRIX (from spec.md for $story_id):"
    extract_test_matrix "$story_id"
    echo ""
    echo "Your current task is the row(s) matching: $checkbox_text"
    echo "Use the exact Input/Expected values from the table above."
fi)

$(if [[ -n "$story_id" ]]; then
    local ac
    ac=$(extract_acceptance_criteria "$story_id")
    if [[ -n "$ac" ]]; then
        echo "ACCEPTANCE CRITERIA (from spec.md for $story_id):"
        echo "$ac"
        echo ""
        echo "Your implementation must satisfy these criteria in addition to the Test Matrix."
        echo ""
    fi
fi)

$(if [[ -f "$CONTEXT_FILE" ]]; then
    echo "CONTEXT FROM PREVIOUS TASKS:"
    cat "$CONTEXT_FILE"
    echo ""
fi)

INSTRUCTIONS — follow this exact sequence:

0. VALIDATE CONTEXT — Read context.md. If any information in it contradicts
   what you see in the actual source files, fix context.md to match reality
   before proceeding. Stale context leads to bugs.
1. Read the current source files and test files to understand what exists.
2. RED: Write a failing test for this specific task in the appropriate test file
   under $TEST_DIR/. If the test file doesn't exist, create it. Use $TEST_FRAMEWORK.
3. RED: Run \`$TEST_DIR_CMD\` — confirm the new test FAILS.
   If it passes already, the test is wrong — make it actually test the behavior.
4. GREEN: Write the MINIMUM production code in the appropriate $SOURCE_DIR/ file to make
   the test pass. If the file doesn't exist, create it with proper
   $FILE_PREAMBLE and appropriate module setup.
5. GREEN: Run \`$TEST_DIR_CMD\` — confirm ALL tests pass (new + existing).
6. If any test fails, fix the code (not the test) until all tests pass.
7. Do a final \`$TEST_DIR_CMD\` to confirm everything is green.
8. UPDATE \`context.md\` — After all tests pass, update context.md in the project root.
   It has three sections — update each as needed:
   - **Current State**: What modules exist, their public APIs, how they connect.
     Update this to reflect what you just built or changed.
   - **Conventions & Decisions**: Patterns, struct choices, naming, error formats,
     test organization. Add new conventions you established. Keep existing ones
     that are still relevant.
   - **Gotchas & Notes for Next Task**: Edge cases discovered, things that almost
     broke, what the next task should watch out for. This section is volatile —
     rewrite it each time with what matters now.
   Keep the whole file under 100 lines. Replace outdated notes, don't just append.

RULES:
- Create $SOURCE_DIR/ and $TEST_DIR/ directories if they don't exist.
- You may only modify files in $SOURCE_DIR/, $TEST_DIR/, and context.md.
- Do NOT modify: plan.md, spec.md, ralph.sh, config.json, CLAUDE.md, README.md
- Do NOT commit — the driver script handles commits.
- Do NOT add features beyond what the current task requires.
- $MODULE_INSTRUCTIONS
PROMPT

    # Append failure context from previous attempt if retrying
    if [[ -n "$failure_context" ]]; then
        cat <<RETRY

PREVIOUS ATTEMPT FAILED. The test runner independently verified your code and it did not pass:

$(echo "$failure_context" | head -80)

Analyze the failure above. Focus on fixing the production code to match the expected
behavior. Do NOT rewrite tests unless they are genuinely wrong.
RETRY
    fi
}

# ── Code quality gates ───────────────────────────────────────────────────

# Optional lint step — advisory only (warn but don't fail).
run_lint() {
    [[ -z "$LINT_CMD" ]] && return 0
    log "${DIM}Linting:${RESET} $LINT_CMD..."
    local lint_output lint_exit=0
    lint_output=$( (cd "$PROJECT_DIR" && eval "$LINT_CMD") 2>&1) || lint_exit=$?
    if [[ $lint_exit -ne 0 ]]; then
        log "${YELLOW}LINT WARNING:${RESET}"
        echo "$lint_output" | head -20 | while IFS= read -r l; do log "  $l"; done
    fi
    echo "$lint_output" >> "${CURRENT_TASK_LOG:-$LOG}"
    return 0
}

# Optional coverage step — advisory only (warn but don't fail).
run_coverage() {
    [[ -z "$COVERAGE_CMD" ]] && return 0
    log "${DIM}Coverage:${RESET} $COVERAGE_CMD..."
    local cov_output cov_exit=0
    cov_output=$( (cd "$PROJECT_DIR" && eval "$COVERAGE_CMD") 2>&1) || cov_exit=$?
    echo "$cov_output" >> "${CURRENT_TASK_LOG:-$LOG}"

    if [[ $COVERAGE_THRESHOLD -gt 0 ]]; then
        local pct
        pct=$(echo "$cov_output" | grep -oE '[0-9]+%' | tail -1 | tr -d '%')
        if [[ -n "$pct" && "$pct" -lt "$COVERAGE_THRESHOLD" ]]; then
            log "${YELLOW}COVERAGE WARNING:${RESET} ${pct}% is below threshold (${COVERAGE_THRESHOLD}%)"
        else
            log "Coverage: ${pct:-?}%"
        fi
    fi
    return 0
}

# ── Integration tests ────────────────────────────────────────────────────

# Optional integration test step — run at phase boundaries.
run_integration_tests() {
    [[ -z "$INTEGRATION_TEST_CMD" ]] && return 0
    local phase_header="$1"

    log "${BLUE}INTEGRATION:${RESET} Running integration tests for $phase_header..."
    local int_output int_exit=0
    int_output=$( (cd "$PROJECT_DIR" && eval "$INTEGRATION_TEST_CMD") 2>&1) || int_exit=$?
    echo "$int_output" >> "${CURRENT_TASK_LOG:-$LOG}"

    if [[ $int_exit -ne 0 ]]; then
        log "${RED}INTEGRATION FAIL:${RESET} Integration tests did not pass after $phase_header"
        log "Output:"
        echo "$int_output" | head -30 | while IFS= read -r l; do log "  $l"; done
        return 1
    fi
    log "${GREEN}INTEGRATION PASS:${RESET} All integration tests green"
    return 0
}

# ── Auto-commit when story is complete ───────────────────────────────────

auto_commit() {
    local story_header="$1"
    local phase_header="$2"

    log "${GREEN}COMMIT:${RESET} All checkboxes green for ${BOLD}$story_header${RESET}"

    if [[ "$DRY_RUN" == true ]]; then
        log "DRY-RUN: Would commit for $story_header"
        return
    fi

    local commit_prompt
    commit_prompt=$(cat <<CPROMPT
Create a git commit for the Strawman project.

Context: All tests are passing for: $story_header ($phase_header)

Steps:
1. Run \`git status\` to see what changed.
2. Run \`git diff\` to review changes.
3. Stage all relevant source and test files (NOT plan.md, NOT ralph.log).
4. Commit with a descriptive message summarizing what was implemented.
   End the message with: Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
CPROMPT
    )

    claude -p "$commit_prompt" \
        --verbose \
        --dangerously-skip-permissions \
        --max-turns 10 \
        --output-format stream-json \
        2>> "${CURRENT_TASK_LOG:-$LOG}" \
        | tee "$LOGS_DIR/commit-$(date '+%Y%m%d-%H%M%S').jsonl" \
        | ralph_stream_filter \
        || log "${YELLOW}WARNING:${RESET} Auto-commit failed"
}

# ── Suggest improvements ─────────────────────────────────────────────────

# Called after story or epic completion. Asks Claude to review what was built
# and update suggestions.md with improvements, features, and deprecations.
suggest_improvements() {
    local story_header="$1"
    local phase_header="$2"
    local milestone="$3"  # "story" or "epic"

    log "${BLUE}REFLECT:${RESET} Generating suggestions after $milestone completion (${BOLD}$story_header${RESET})"

    if [[ "$DRY_RUN" == true ]]; then
        log "DRY-RUN: Would generate suggestions for $story_header"
        return
    fi

    local suggest_prompt
    suggest_prompt="You just completed a $milestone for the Strawman Lisp interpreter in $LANG_NAME.

Completed: $story_header ($phase_header)
Project directory: $PROJECT_DIR

Review the current codebase (source in $SOURCE_DIR/, tests in $TEST_DIR/) and
update suggestions.md in the project root.

The file has two sections:

## Active
Add new suggestions here. Each entry should be a single line:
- [type] Description — why it matters (surfaced after $story_header)

Types: improvement (refactor/quality), feature (new capability), direction (architectural)

Only suggest things that are non-obvious and actionable. Focus on:
- Patterns in the code that could be generalized
- Edge cases the spec doesn't cover but a real Lisp would need
- Architectural decisions that should be revisited before the next epic
- Performance or correctness issues spotted during implementation

Quality over quantity — 2-3 thoughtful suggestions beat 10 generic ones.

## Deprecated
Review existing Active suggestions. If any are now irrelevant, already done,
or would be harmful given what you have learned, move them here with a reason:
- [~~type~~] Description — reason it is deprecated (deprecated after $story_header)

Do NOT modify any source code, test files, or other project files.
Only modify suggestions.md."

    claude -p "$suggest_prompt" \
        --verbose \
        --dangerously-skip-permissions \
        --max-turns 10 \
        --output-format stream-json \
        2>> "${CURRENT_TASK_LOG:-$LOG}" \
        | tee "$LOGS_DIR/suggest-$(date '+%Y%m%d-%H%M%S').jsonl" \
        | ralph_stream_filter \
        || log "${YELLOW}WARNING:${RESET} Suggestion generation failed"
}

# ── Main loop ────────────────────────────────────────────────────────────

main() {
    preflight_checks
    acquire_lock
    load_config
    check_git_clean
    check_disk_space

    RALPH_START_TIME=$(date +%s)
    ITERATION=0
    LOGS_DIR="$PROJECT_DIR/logs"
    mkdir -p "$LOGS_DIR"
    rotate_log

    log "${BOLD}${BLUE}═══════════════════════════════════════════${RESET}"
    log "${BOLD}Ralph Loop starting${RESET} in $PROJECT_DIR (${CYAN}$LANG_NAME${RESET})"
    log "${BOLD}${BLUE}═══════════════════════════════════════════${RESET}"
    show_plan_summary

    local consecutive_failures=0
    local last_item=""
    local LAST_FAILURE_OUTPUT=""

    while true; do
        # 0. GUARDS — cost, disk, iteration limits
        ITERATION=$((ITERATION + 1))
        if [[ $ITERATION -gt $MAX_ITERATIONS ]]; then
            log "${RED}${BOLD}SAFETY:${RESET}${RED} Reached $MAX_ITERATIONS iterations. Stopping to prevent runaway loop.${RESET}"
            exit 1
        fi

        local over_budget
        over_budget=$(awk "BEGIN{print ($TOTAL_COST_USD >= $MAX_COST_USD) ? 1 : 0}")
        if [[ "$over_budget" -eq 1 ]]; then
            log "${RED}${BOLD}BUDGET:${RESET}${RED} Session cost \$${TOTAL_COST_USD} reached cap \$${MAX_COST_USD}. Stopping.${RESET}"
            exit 1
        fi

        check_disk_space

        # 1. FIND — next unchecked item
        local match
        match=$(find_next_item)

        if [[ -z "$match" ]]; then
            log "${GREEN}${BOLD}══ Plan complete! All items checked. ══${RESET}"
            exit 0
        fi

        local line_num
        line_num=$(echo "$match" | cut -d: -f1)
        local checkbox_text
        checkbox_text=$(echo "$match" | cut -d: -f2- | sed 's/^ *- \[ \] //')

        # 2. CONTEXT — extract phase and story headers
        local context
        context=$(get_context "$line_num")
        local phase_header
        phase_header=$(echo "$context" | head -1)
        local story_header
        story_header=$(echo "$context" | tail -1)

        log "${DIM}──────────────────────────────────────────${RESET}"
        log "${BOLD}TASK:${RESET}  ${CYAN}$checkbox_text${RESET}"
        log "${BOLD}PHASE:${RESET} $phase_header"
        log "${BOLD}STORY:${RESET} $story_header"
        log "${DIM}LINE:  $line_num${RESET}"

        show_progress "$checkbox_text" "$((consecutive_failures + 1))"

        # 3. FAILURE TRACKING — same item failing repeatedly?
        if [[ "$checkbox_text" == "$last_item" ]]; then
            consecutive_failures=$((consecutive_failures + 1))
            log "${YELLOW}RETRY #$consecutive_failures${RESET} for: $checkbox_text"

            # Reset working tree so retry starts from last known-good state
            reset_working_tree

            if [[ $consecutive_failures -ge $MAX_RETRIES ]]; then
                log "${RED}${BOLD}══ STUCK: $MAX_RETRIES consecutive failures on: $checkbox_text ══${RESET}"
                log "${RED}${BOLD}══ Pausing for human intervention. Fix the issue and re-run. ══${RESET}"
                reset_working_tree
                exit 1
            fi
        else
            consecutive_failures=0
            last_item="$checkbox_text"
            LAST_FAILURE_OUTPUT=""
        fi

        # 4. BUILD — construct the prompt
        local story_id
        story_id=$(extract_story_id "$story_header" "$checkbox_text")
        local prompt
        prompt=$(build_prompt "$checkbox_text" "$phase_header" "$story_header" "$story_id" "$LAST_FAILURE_OUTPUT")

        # Per-task log file
        local task_slug
        task_slug=$(echo "$checkbox_text" | LC_ALL=C tr -cs 'a-zA-Z0-9._-' '-' | sed 's/^-//;s/-$//' | head -c 60)
        local attempt=$((consecutive_failures + 1))
        CURRENT_TASK_LOG="$LOGS_DIR/$(date '+%Y%m%d-%H%M%S')-${task_slug}-attempt${attempt}.log"

        if [[ "$DRY_RUN" == true ]]; then
            log "DRY-RUN: Would invoke claude with prompt:"
            echo "--- PROMPT BEGIN ---"
            echo "$prompt"
            echo "--- PROMPT END ---"
            mark_done "$line_num"
            sleep 1
            continue
        fi

        # Log the prompt to the task log
        {
            echo "=== PROMPT SENT ==="
            echo "$prompt"
            echo "=== END PROMPT ==="
            echo ""
        } >> "$CURRENT_TASK_LOG"

        # 4b. SNAPSHOT — capture baseline state before Claude runs
        local snapshot_file="$LOGS_DIR/.snapshot-$$"
        snapshot_test_state "$snapshot_file"

        # 5. RUN — invoke Claude Code with streaming
        log "${BLUE}Invoking Claude Code...${RESET}"
        local task_start claude_exit
        task_start=$(date +%s)
        claude_exit=0
        local raw_stream="$LOGS_DIR/$(date '+%Y%m%d-%H%M%S')-${task_slug}-stream.jsonl"

        ralph_timeout "$CLAUDE_TIMEOUT" \
            claude -p "$prompt" \
            --verbose \
            --dangerously-skip-permissions \
            --max-turns 50 \
            --output-format stream-json \
            --append-system-prompt "You are executing a TDD cycle for the Strawman Lisp interpreter in $LANG_NAME. Read spec.md and plan.md for context. Write tests first, then minimal code. Always run $TEST_DIR_CMD to verify." \
            2>> "$CURRENT_TASK_LOG" \
            | tee "$raw_stream" \
            | ralph_stream_filter \
            || claude_exit=$?

        local task_end task_duration
        task_end=$(date +%s)
        task_duration=$((task_end - task_start))

        if [[ $claude_exit -eq 124 ]]; then
            log "${RED}TIMEOUT:${RESET} Claude exceeded ${CLAUDE_TIMEOUT}s limit"
        else
            log "Claude finished in ${BOLD}${task_duration}s${RESET} (exit code $claude_exit)"
        fi

        # Accumulate cost from stream filter
        if [[ -f /tmp/.ralph-cost-$$ ]]; then
            local task_cost
            task_cost=$(<"/tmp/.ralph-cost-$$")
            TOTAL_COST_USD=$(awk "BEGIN{printf \"%.4f\", $TOTAL_COST_USD + $task_cost}")
            rm -f "/tmp/.ralph-cost-$$"
        fi

        # Parse the result event from the stream
        if [[ -f "$raw_stream" ]]; then
            local result_line result_subtype
            result_line=$(tail -1 "$raw_stream")
            result_subtype=$(echo "$result_line" | jq -r '.subtype // empty' 2>/dev/null)
            if [[ "$result_subtype" == "error_max_turns" ]]; then
                log "${YELLOW}WARNING:${RESET} Claude hit max turns limit (50)"
            fi

            # Extract final response to task log
            local result_text
            result_text=$(echo "$result_line" | jq -r '.result // empty' 2>/dev/null)
            if [[ -n "$result_text" ]]; then
                {
                    echo ""
                    echo "=== CLAUDE FINAL RESPONSE ==="
                    echo "$result_text"
                    echo "=== END RESPONSE ==="
                } >> "$CURRENT_TASK_LOG"
            fi
        fi

        if [[ $claude_exit -ne 0 ]]; then
            log "${YELLOW}WARNING:${RESET} Claude exited with code $claude_exit"
        fi

        # 5b. SCOPE CHECK — warn if files modified outside expected directories
        check_scope || true

        # 5c. TDD DISCIPLINE — verify RED and GREEN phases happened
        local tdd_issues=""
        tdd_issues=$(verify_tdd_discipline "$snapshot_file" 2>&1) || {
            log "${YELLOW}TDD discipline check failed${RESET}"
            LAST_FAILURE_OUTPUT="Your previous attempt did not follow TDD discipline: $tdd_issues"
            log "${YELLOW}Will retry with TDD feedback${RESET}"
            # Keep snapshots for debugging — only clean up on success
            sleep 2
            continue
        }
        # Clean up snapshots after successful verification
        rm -f "$snapshot_file" "${snapshot_file}".* 2>/dev/null || true

        # 6. VERIFY — independently run the test suite
        log "${BLUE}Verifying:${RESET} $TEST_DIR_CMD..."
        local test_exit=0
        local test_output=""

        # Only run tests if the tests directory has source files
        if ls "$PROJECT_DIR"/$TEST_DIR/*$FILE_EXT 1>/dev/null 2>&1; then
            test_output=$( (cd "$PROJECT_DIR" && eval "$TEST_DIR_CMD") 2>&1) || test_exit=$?
            echo "$test_output" >> "$CURRENT_TASK_LOG"
        else
            log "${DIM}No test files yet — skipping verification (first item bootstrap)${RESET}"
            test_exit=0
        fi

        # 7. MARK or RETRY
        if [[ $test_exit -eq 0 ]]; then
            log "${GREEN}${BOLD}PASS${RESET}${GREEN} — all tests green${RESET} ${DIM}(${task_duration}s)${RESET}"
            TASKS_COMPLETED=$((TASKS_COMPLETED + 1))
            run_lint
            run_coverage
            validate_context
            mark_done "$line_num"
            consecutive_failures=0
            LAST_FAILURE_OUTPUT=""
            log "${DIM}Marked line $line_num as done${RESET}"

            # 8. COMMIT & REFLECT — if story is complete
            if check_story_complete "$line_num"; then
                auto_commit "$story_header" "$phase_header"

                # Check if the entire phase (epic) is also complete
                if check_phase_complete "$line_num"; then
                    run_integration_tests "$phase_header" || {
                        log "${YELLOW}WARNING:${RESET} Integration tests failed — continuing but review needed"
                    }
                    suggest_improvements "$story_header" "$phase_header" "epic"
                else
                    suggest_improvements "$story_header" "$phase_header" "story"
                fi
            fi
        else
            log "${RED}${BOLD}FAIL${RESET}${RED} — tests did not pass (exit code $test_exit)${RESET} ${DIM}(${task_duration}s)${RESET}"
            TASKS_FAILED=$((TASKS_FAILED + 1))
            parse_test_failures "$test_output"
            LAST_FAILURE_OUTPUT="$test_output"
            log "${DIM}Full output in: $CURRENT_TASK_LOG${RESET}"
            log "${YELLOW}Will retry this item on next iteration${RESET}"
        fi

        # Brief pause between iterations
        sleep 2
    done
}

main "$@"
