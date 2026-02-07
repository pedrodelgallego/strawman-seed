#!/usr/bin/env bash
# Ralph Loop — autonomous TDD driver for Strawman
# Continuously reads plan.md, finds the next unchecked item, and invokes
# Claude Code to execute one RED→GREEN TDD cycle per iteration.
#
# Usage:
#   ./ralph.sh            # run the loop
#   ./ralph.sh --dry-run  # show what would be done without invoking claude

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
PLAN="$PROJECT_DIR/plan.md"
SPEC="$PROJECT_DIR/spec.md"
LOG="$PROJECT_DIR/ralph.log"
MAX_RETRIES=3
DRY_RUN=false
CONFIG="$PROJECT_DIR/config.json"

if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
fi

# ── Configuration ────────────────────────────────────────────────────────

load_config() {
    if ! command -v jq &>/dev/null; then
        echo "ERROR: jq is required but not installed." >&2
        echo "  macOS:  brew install jq" >&2
        echo "  Linux:  apt install jq" >&2
        exit 1
    fi

    if [[ ! -f "$CONFIG" ]]; then
        echo "ERROR: config.json not found at $CONFIG" >&2
        exit 1
    fi

    if ! jq empty "$CONFIG" 2>/dev/null; then
        echo "ERROR: config.json is not valid JSON" >&2
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

    local required_fields=("language" "file_extension" "run_command" "test_command" "test_dir_command" "test_framework" "entry_point" "module_instructions")
    for field in "${required_fields[@]}"; do
        local val
        val=$(jq -r ".$field // empty" "$CONFIG")
        if [[ -z "$val" ]]; then
            echo "ERROR: Required config field '$field' is missing in config.json" >&2
            exit 1
        fi
    done
}

# ── Logging ──────────────────────────────────────────────────────────────

log() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg"
    echo "$msg" >> "$LOG"
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
    # Replace `- [ ]` with `- [x]` on the specific line
    if [[ "$(uname)" == "Darwin" ]]; then
        sed -i '' "${line_num}s/- \[ \]/- [x]/" "$PLAN"
    else
        sed -i "${line_num}s/- \[ \]/- [x]/" "$PLAN"
    fi
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

# ── Build the prompt ─────────────────────────────────────────────────────

build_prompt() {
    local checkbox_text="$1"
    local phase_header="$2"
    local story_header="$3"

    cat <<PROMPT
You are working on the Strawman Lisp interpreter in $LANG_NAME. Follow strict TDD.

PROJECT DIRECTORY: $PROJECT_DIR

CURRENT TASK: $checkbox_text
PHASE: $phase_header
STORY: $story_header

INSTRUCTIONS — follow this exact sequence:

1. Read spec.md to find the Test Matrix row matching this task.
2. Read the current source files and test files to understand what exists.
3. RED: Write a failing test for this specific task in the appropriate test file
   under $TEST_DIR/. If the test file doesn't exist, create it. Use $TEST_FRAMEWORK.
4. RED: Run \`$TEST_DIR_CMD\` — confirm the new test FAILS.
   If it passes already, the test is wrong — make it actually test the behavior.
5. GREEN: Write the MINIMUM production code in the appropriate $SOURCE_DIR/ file to make
   the test pass. If the file doesn't exist, create it with proper
   $FILE_PREAMBLE and appropriate module setup.
6. GREEN: Run \`$TEST_DIR_CMD\` — confirm ALL tests pass (new + existing).
7. If any test fails, fix the code (not the test) until all tests pass.
8. Do a final \`$TEST_DIR_CMD\` to confirm everything is green.

RULES:
- Create $SOURCE_DIR/ and $TEST_DIR/ directories if they don't exist.
- Do NOT modify plan.md — the driver script handles checkboxes.
- Do NOT commit — the driver script handles commits.
- Do NOT add features beyond what the current task requires.
- $MODULE_INSTRUCTIONS
PROMPT
}

# ── Auto-commit when story is complete ───────────────────────────────────

auto_commit() {
    local story_header="$1"
    local phase_header="$2"

    log "COMMIT: All checkboxes green for $story_header"

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
        --dangerously-skip-permissions \
        --max-turns 10 \
        --output-format text \
        >> "$LOG" 2>&1 || log "WARNING: Auto-commit failed"
}

# ── Main loop ────────────────────────────────────────────────────────────

main() {
    load_config

    log "═══════════════════════════════════════════"
    log "Ralph Loop starting in $PROJECT_DIR ($LANG_NAME)"
    log "═══════════════════════════════════════════"

    local consecutive_failures=0
    local last_item=""

    while true; do
        # 1. FIND — next unchecked item
        local match
        match=$(find_next_item)

        if [[ -z "$match" ]]; then
            log "══ Plan complete! All items checked. ══"
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

        log "──────────────────────────────────────────"
        log "TASK: $checkbox_text"
        log "PHASE: $phase_header"
        log "STORY: $story_header"
        log "LINE: $line_num"

        # 3. FAILURE TRACKING — same item failing repeatedly?
        if [[ "$checkbox_text" == "$last_item" ]]; then
            consecutive_failures=$((consecutive_failures + 1))
            log "RETRY #$consecutive_failures for: $checkbox_text"

            if [[ $consecutive_failures -ge $MAX_RETRIES ]]; then
                log "══ STUCK: $MAX_RETRIES consecutive failures on: $checkbox_text ══"
                log "══ Pausing for human intervention. Fix the issue and re-run. ══"
                exit 1
            fi
        else
            consecutive_failures=0
            last_item="$checkbox_text"
        fi

        # 4. BUILD — construct the prompt
        local prompt
        prompt=$(build_prompt "$checkbox_text" "$phase_header" "$story_header")

        if [[ "$DRY_RUN" == true ]]; then
            log "DRY-RUN: Would invoke claude with:"
            log "  Task: $checkbox_text"
            log "  Phase: $phase_header"
            log "  Story: $story_header"
            log "DRY-RUN: Marking done and continuing..."
            mark_done "$line_num"
            sleep 1
            continue
        fi

        # 5. RUN — invoke Claude Code
        log "Invoking Claude Code..."
        local claude_exit=0
        claude -p "$prompt" \
            --dangerously-skip-permissions \
            --max-turns 50 \
            --output-format text \
            --append-system-prompt "You are executing a TDD cycle for the Strawman Lisp interpreter in $LANG_NAME. Read spec.md and plan.md for context. Write tests first, then minimal code. Always run $TEST_DIR_CMD to verify." \
            >> "$LOG" 2>&1 || claude_exit=$?

        if [[ $claude_exit -ne 0 ]]; then
            log "WARNING: Claude exited with code $claude_exit"
        fi

        # 6. VERIFY — independently run the test suite
        log "Verifying: $TEST_DIR_CMD..."
        local test_exit=0

        # Only run tests if the tests directory has source files
        if ls "$PROJECT_DIR"/$TEST_DIR/*$FILE_EXT 1>/dev/null 2>&1; then
            (cd "$PROJECT_DIR" && eval "$TEST_DIR_CMD") >> "$LOG" 2>&1 || test_exit=$?
        else
            log "No test files yet — skipping verification (first item bootstrap)"
            test_exit=0
        fi

        # 7. MARK or RETRY
        if [[ $test_exit -eq 0 ]]; then
            log "PASS — all tests green"
            mark_done "$line_num"
            consecutive_failures=0
            log "Marked line $line_num as done"

            # 8. COMMIT — if story is complete
            if check_story_complete "$line_num"; then
                auto_commit "$story_header" "$phase_header"
            fi
        else
            log "FAIL — tests did not pass (exit code $test_exit)"
            log "Will retry this item on next iteration"
        fi

        # Brief pause between iterations
        sleep 2
    done
}

main "$@"
