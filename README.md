# Strawman Seed

A seed repository for building AI-coded Lisp interpreters in any language.

Clone this repo, pick your language, and let Claude Code write every line
of code — one failing test at a time.

---

## The Idea

Strawman Seed is a starter kit for an experiment:

1. **A Lisp interpreter** structured around *Lisp in Small Pieces* by
   Christian Queinnec — starting from a basic eval/apply core and growing
   toward continuations, macros, a bytecode compiler, and an object system.

2. **Any implementation language.** The repo ships with config examples for
   Python, TypeScript, Rust, Go, Ruby, Java, C, Elixir, Haskell, and Racket.
   Pick one, copy its config, and the entire toolchain adapts.

3. **Zero human-written code.** The spec, the plan, the architecture — those
   are human decisions. Every line of implementation code, every test, every
   commit message is produced by Claude Code following a strict TDD workflow.

We call this the **Ralph approach**: direct the intent, let the machine
write the implementation, and trust nothing that isn't verified by tests.

## How It Works

An autonomous shell script (`ralph.sh`) drives the entire build:

```
                     ┌─────────────┐
                     │  PREFLIGHT   │  verify claude, jq, git
                     └──────┬──────┘
                            │
                     ┌──────▼──────┐
                     │ LOAD CONFIG │  config.json → language, test cmd
                     └──────┬──────┘
                            │
              ┌─────────────▼─────────────┐
              │       MAIN LOOP           │
              │                           │
              │  1. FIND next - [ ] item  │  plan.md
              │  2. CONTEXT phase + story │  extract story ID
              │  3. BUILD prompt          │  + Test Matrix from spec.md
              │     + context.md          │  + failure context (retries)
              │  4. RUN claude -p         │  Claude does RED→GREEN TDD
              │     (stream-json output)  │
              │  5. SCOPE CHECK           │  warn on out-of-scope changes
              │  6. VERIFY tests          │  independent test run
              │          │                │
              │       ┌──▼──┐             │
              │       │PASS?│             │
              │       └──┬──┘             │
              │      yes │ no             │
              │    ┌─────┴──────┐         │
              │    ▼            ▼         │
              │  MARK [x]   SAVE failure  │  failure context → retry
              │    │         → LOOP       │
              │    ▼                      │
              │  STORY COMPLETE?          │
              │    yes → COMMIT           │  auto-commit via claude
              │        → SUGGEST          │  update suggestions.md
              │  LOOP                     │
              └───────────────────────────┘
```

**Key behaviors:**
- The test suite is the only authority — Claude's claim is never trusted
- On failure, the test output is fed into the next retry prompt
- After 3 consecutive failures on the same task, the loop pauses for a human
- At story/epic milestones, Claude reviews the codebase and suggests improvements
- A rolling `context.md` carries learnings between tasks

## What Strawman Will Support

The roadmap follows 10 epics mapped to the book's chapters:

| Priority | Epics | What you get |
|----------|-------|-------------|
| **MVP** | 1–2 | Lexer, parser, eval, closures, recursion, `let` forms, REPL |
| **Depth** | 3–4 | `call/cc`, continuations, `catch`/`throw`, mutation, vectors |
| **Performance** | 5–6 | Denotational semantics, lexical addressing, benchmarks |
| **Compilation** | 7 | Bytecode compiler + virtual machine |
| **Power** | 8–9 | `eval`, reflection, macros, quasiquote |
| **OOP** | 10 | Classes, generic functions, inheritance |

## Getting Started

```bash
# 1. Clone the seed
git clone <this-repo> strawman-python
cd strawman-python

# 2. Pick your language (see config-examples/ for all options)
cp config-examples/config.python.json config.json

# 3. Let Claude build it
./ralph.sh

# Or preview what would happen first (shows the full prompt)
./ralph.sh --dry-run
```

The default `config.json` targets Racket. Config examples are provided for:
**Python, JavaScript, TypeScript, Rust, Go, Ruby, Java, C, Elixir, Haskell.**

## Example

```lisp
(define (square x) (* x x))
(square 5)
; => 25

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 10)
; => 3628800

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))
(map square '(1 2 3 4 5))
; => (1 4 9 16 25)
```

## Running the Ralph Loop

```bash
# Autonomous mode — writes tests, writes code, verifies, commits
./ralph.sh

# Preview — shows the full prompt without invoking Claude
./ralph.sh --dry-run
```

Progress is tracked as checkboxes in `plan.md`. Per-task logs go to `logs/`.

## Project Layout

```
strawman-seed/
  config.json          Language & toolchain settings (edit this)
  config-examples/     Pre-made configs for 11 languages
  ralph.sh             Autonomous TDD loop driver
  spec.md              10 epics, 37 stories, 150+ test matrix rows
  plan.md              TDD build order with per-task checkboxes
  context.md           Rolling working memory across tasks
  suggestions.md       Improvements surfaced at milestones
  CLAUDE.md            Conventions for Claude Code
  examples/            Example .straw programs
  src/                 Production code (created by Claude)
  tests/               Test suite (created by Claude)
  logs/                Per-task logs and stream recordings
```

## The Prompt

Each task prompt includes:

| Section | Source | Purpose |
|---------|--------|---------|
| Task & context | plan.md | What to build now |
| Test Matrix | spec.md | Exact input/expected pairs |
| Previous learnings | context.md | Decisions, patterns, gotchas |
| Failure context | test output | What went wrong (retries only) |
| Scope rules | ralph.sh | Which files can be modified |

Claude is instructed to follow strict TDD (RED → GREEN), update `context.md`
with learnings after each task, and stay within scope.

## Requirements

- [Claude Code](https://claude.ai/code) (drives the autonomous loop)
- [jq](https://jqlang.github.io/jq/) (for config parsing in `ralph.sh`)
- The toolchain for your chosen language (e.g., Racket, Python, Rust, etc.)
