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
 plan.md          spec.md          ralph.sh
 ┌──────┐        ┌──────┐        ┌────────────────────────┐
 │- [ ] │───────>│ Test │───────>│ 1. Find next task       │
 │- [ ] │        │Matrix│        │ 2. Claude: write test   │
 │- [x] │        │      │        │ 3. Claude: write code   │
 │- [x] │        │      │        │ 4. Run test suite (gate)│
 └──────┘        └──────┘        │ 5. Mark [x] or retry    │
                                 │ 6. Auto-commit           │
                                 │ 7. Loop                  │
                                 └────────────────────────┘
```

The test suite is the only authority. If tests fail, the checkbox stays
unchecked and Claude gets another attempt. After 3 failures, the loop
pauses for a human to look.

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

# Or preview what would happen first
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

# Preview — shows what would happen without invoking Claude
./ralph.sh --dry-run
```

Progress is tracked as checkboxes in `plan.md`. Logs go to `ralph.log`.

## Project Layout

```
strawman-seed/
  config.json          Language & toolchain settings (edit this)
  config-examples/     Pre-made configs for 11 languages
  ralph.sh             Autonomous TDD loop driver
  spec.md              10 epics, 37 stories, 150+ test matrix rows
  plan.md              TDD build order with per-task checkboxes
  CLAUDE.md            Conventions for Claude Code
  examples/            Example .straw programs
  src/                 Production code (created by Claude)
  tests/               Test suite (created by Claude)
```

## Requirements

- [Claude Code](https://claude.ai/code) (drives the autonomous loop)
- [jq](https://jqlang.github.io/jq/) (for config parsing in `ralph.sh`)
- The toolchain for your chosen language (e.g., Racket, Python, Rust, etc.)
