# Strawman Seed

Seed repo for building AI-coded Lisp interpreters in any language.
Clone it, pick a language via `config.json`, and run `./ralph.sh` —
Claude Code writes all implementation code and tests.
Structured around *Lisp in Small Pieces* (Queinnec).

## Configuration

All language-specific settings live in `config.json` at the project root.
Read it before starting work to know the implementation language, test command,
file extensions, and module conventions. Key fields:

| Field | Purpose |
|-------|---------|
| `language` | Implementation language name (e.g. `racket`, `python`) |
| `file_extension` | Source file extension (e.g. `.rkt`, `.py`) |
| `run_command` | Command to run the entry point (e.g. `racket`) |
| `test_command` | Command to run a single test file (e.g. `raco test`) |
| `test_dir_command` | Command to run the full test suite (e.g. `raco test tests/`) |
| `test_framework` | Test framework name (e.g. `rackunit`, `pytest`) |
| `source_dir` | Production source directory (default: `src`) |
| `test_dir` | Test directory (default: `tests`) |
| `entry_point` | Main file to run the interpreter |
| `test_file_pattern` | Naming pattern for test files (e.g. `test-{module}`) |
| `module_instructions` | Language-specific module/import setup instructions |
| `file_preamble` | Required preamble in every source file (e.g. `#lang racket`) |

## Commands

Commands use values from `config.json`. Defaults shown for Racket:

```bash
# Run interpreter (config: run_command + entry_point)
racket strawman.rkt              # Start the REPL
racket strawman.rkt file.straw   # Run a script

# Run tests (config: test_dir_command / test_command)
raco test tests/                 # Run full test suite
raco test tests/test-lexer.rkt   # Run a single test file

# Autonomous TDD loop (reads config.json automatically)
./ralph.sh                       # Autonomous TDD loop
./ralph.sh --dry-run             # Preview without invoking Claude
```

## Architecture

```
Source string → Lexer → Tokens → Parser → AST → Evaluator → Value
                                                    ↑
                                              Environment
                                              (lexical scope)
```

| File | Role |
|------|------|
| `{entry_point}` | Entry point: REPL or file execution |
| `{source_dir}/lexer.*` | `tokenize : string -> (listof token)` |
| `{source_dir}/parser.*` | `parse : (listof token) -> s-expr` |
| `{source_dir}/env.*` | Environment: `make-env`, `env-lookup`, `env-set!`, `env-update!`, `env-extend` |
| `{source_dir}/eval.*` | `straw-eval : s-expr × env -> value` |
| `{source_dir}/builtins.*` | `default-env` with arithmetic, comparison, list ops, I/O |
| `{source_dir}/repl.*` | `run-repl` — read/eval/print loop with error recovery |

File extensions and module conventions are set in `config.json`.
Each source module has a matching test file in `{test_dir}/`.

## Specification & Plan

| File | What it contains |
|------|-----------------|
| `spec.md` | 10 epics, 37 user stories, each with narrative, requirements, test matrix, acceptance criteria, definition of done |
| `plan.md` | 15 phases with per-task `- [ ]` checkboxes tracking TDD progress |
| `ralph.sh` | Autonomous driver that reads plan.md and invokes Claude Code |

**Epics map to *Lisp in Small Pieces* chapters:**

1. Basic Interpreter (lexer, parser, eval, closures, builtins, REPL)
2. Namespaces & Recursion (`let`, `let*`, `letrec`, define shorthand)
3. Escape & Continuations (CPS evaluator, `call/cc`, `catch`/`throw`)
4. Assignment & Side Effects (box model, `set-car!`/`set-cdr!`, vectors)
5. Denotational Semantics (formal semantics document, semantics-driven tests)
6. Fast Interpretation (pretreatment, lexical addressing, benchmarks)
7. Bytecode Compilation (instruction set, compiler, VM)
8. Eval & Reflection (`eval`, first-class environments)
9. Macros (`define-macro`, quasiquote, `cond`/`when`/`unless`)
10. Object System (classes, generics, inheritance)

Epics 1–2 are the MVP. Later epics build on top.

## TDD Discipline

Every task follows this exact cycle:

```
1. RED      Write a failing test for one Test Matrix row from spec.md
2. RED      Run the test suite (test_dir_command from config.json) — confirm it fails
3. GREEN    Write the minimum production code to pass
4. GREEN    Run the test suite — confirm ALL tests pass (not just the new one)
5. FIX      If anything fails, fix code until green
6. REFACTOR Clean up while keeping all tests green
7. REPEAT   Next Test Matrix row
```

**Hard rules:**
- Never write production code without a failing test first
- Never skip step 2 — seeing the failure confirms the test tests what you think
- One Test Matrix row per RED→GREEN cycle (keep cycles small)
- Run the full suite after every GREEN
- Commit after completing each story (all its Test Matrix rows green)
- Do not modify `plan.md` checkboxes — `ralph.sh` handles that
- Do not add features beyond what the current task requires

## Coding Conventions

**Language style** (see `config.json` for active language):
- Follow the `file_preamble` and `module_instructions` from config.json
- Prefer idiomatic patterns for the configured language
- Prefer pure functions; mutation only where necessary (environments)
- Use language-idiomatic data structures for closures and tokens

**Testing:**
- Every module in `{source_dir}/` gets a corresponding test in `{test_dir}/`
- Use the configured `test_framework` (see config.json)
- Test names should describe the behavior, not the implementation
- Error tests should assert on expected error messages

**Naming:**
- The language is called Strawman Lisp
- Source files use the `.straw` extension
- The evaluator function is `straw-eval` (not `eval`, to avoid shadowing the host language)

## Ralph Loop (`ralph.sh`)

The autonomous driver that builds the interpreter without human coding:

1. Reads `plan.md`, finds the first unchecked `- [ ]` item
2. Extracts phase header (`## Phase N — ...`) and story header (`**E1.4 — ...`)
3. Builds a prompt telling Claude Code to execute one TDD cycle for that task
4. Invokes `claude -p` with `--dangerously-skip-permissions --max-turns 50`
5. **Independently** runs the test suite (`test_dir_command` from config.json) to verify — Claude's claim is not trusted
6. If tests pass: marks `- [x]` in plan.md
7. If tests fail: leaves unchecked, retries (up to 3 times, then stops for human)
8. When all checkboxes in a story are `[x]`: auto-commits
9. Logs everything to `ralph.log`

## Value Representation

> The host-language column below assumes the default Racket configuration.
> Adapt representations when using a different language (see `config.json`).

| Strawman type | Host-language representation |
|--------------|----------------------------|
| Number | Native number type |
| String | Native string type |
| Boolean | `#t` / `#f` (true/false in the host language) |
| Symbol | Interned symbol or string |
| Pair / List | Cons cells or equivalent linked list |
| Empty list | Nil / empty list sentinel |
| Closure | Struct: `(closure params body env)` |
| Builtin | Struct wrapping a host-language procedure |
| Void | Host-language void — not printed in REPL |

## Special Forms vs Builtins

**Special forms** (handled in the evaluator — custom evaluation rules):
`quote`, `if`, `begin`, `define`, `set!`, `lambda`, `and`, `or`,
`let`, `let*`, `letrec`

**Builtins** (host-language procedures in the default environment — normal eval):
`+`, `-`, `*`, `/`, `mod`, `<`, `>`, `<=`, `>=`, `=`, `equal?`,
`cons`, `car`, `cdr`, `list`, `null?`, `pair?`, `not`,
`number?`, `string?`, `symbol?`, `boolean?`, `procedure?`,
`display`, `newline`

## Error Handling

- Unbound variable: `"unbound variable: <name>"`
- Cannot set! unbound: `"cannot set! unbound variable: <name>"`
- Not a procedure: `"not a procedure: <value>"`
- Arity mismatch: `"arity mismatch: expected <n>, got <m>"`
- Division by zero: `"division by zero"`
- Car/cdr on non-pair: `"car: expected pair"` / `"cdr: expected pair"`
- Type errors: `"expected number"`, `"expected pair"`, etc.
- Special form errors: `"quote: expected exactly one argument"`, `"if: expected 2 or 3 arguments"`, `"lambda: expected parameter list"`, `"define: expected at least 2 arguments"`
- The REPL catches all errors, prints `Error: <message>`, and continues
