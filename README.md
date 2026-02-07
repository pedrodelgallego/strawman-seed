# Strawman

A minimal Lisp interpreter written in Racket — built entirely by AI,
one failing test at a time.

---

## The Idea

Strawman is two experiments in one:

1. **A Lisp interpreter** structured around *Lisp in Small Pieces* by
   Christian Queinnec — starting from a basic eval/apply core and growing
   toward continuations, macros, a bytecode compiler, and an object system.

2. **An AI-authoring experiment** where a human writes zero code. The spec,
   the plan, the architecture — those are human decisions. Every line of
   Racket, every test, every commit message is produced by Claude Code
   following a strict TDD workflow.

We call this the **Ralph approach**: direct the intent, let the machine
write the implementation, and trust nothing that isn't verified by tests.

## How It Works

An autonomous shell script (`ralph.sh`) drives the entire build:

```
 plan.md          spec.md          ralph.sh
 ┌──────┐        ┌──────┐        ┌────────────────────┐
 │- [ ] │───────>│ Test │───────>│ 1. Find next task   │
 │- [ ] │        │Matrix│        │ 2. Claude: write    │
 │- [x] │        │      │        │    failing test     │
 │- [x] │        │      │        │ 3. Claude: write    │
 └──────┘        └──────┘        │    minimum code     │
                                 │ 4. raco test (gate) │
                                 │ 5. Mark [x] or retry│
                                 │ 6. Auto-commit      │
                                 │ 7. Loop             │
                                 └────────────────────┘
```

The test suite is the only authority. If `raco test tests/` fails, the
checkbox stays unchecked and Claude gets another attempt. After 3 failures,
the loop pauses for a human to look.

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

## Quick Start

```bash
# Start the REPL
racket strawman.rkt

# Run a script
racket strawman.rkt examples/hello.straw

# Run the test suite
raco test tests/
```

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
strawman/
  strawman.rkt       Entry point
  ralph.sh           Autonomous TDD loop driver
  src/
    lexer.rkt        Tokenizer
    parser.rkt        S-expression parser
    env.rkt          Environments (lexical scope)
    eval.rkt         Evaluator
    builtins.rkt     Built-in primitives
    repl.rkt         Interactive REPL
  tests/             Test suite (rackunit, one file per module)
  examples/          Example .straw programs
  spec.md            10 epics, 37 stories, 150+ test matrix rows
  plan.md            TDD build order with per-task checkboxes
  CLAUDE.md          Conventions for Claude Code
```

## Configuration

The implementation language is configured in `config.json`. The default
configuration targets Racket. To build the interpreter in a different language,
edit `config.json` with the appropriate commands, file extensions, and test
framework. `ralph.sh` reads all language-specific settings from this file.

## Requirements

- The implementation language toolchain (default: [Racket](https://racket-lang.org/) v9.0+)
- [jq](https://jqlang.github.io/jq/) (for config parsing in `ralph.sh`)
- [Claude Code](https://claude.ai/code) (for running `ralph.sh`)
