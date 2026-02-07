# Strawman Implementation Plan

Step-by-step build order using strict TDD. Each phase produces a working,
testable system. Phases map to user stories in `spec.md`.

---

## TDD Cycle

Every task within every phase follows this loop:

```
1. RED    — Write a failing test for the next behavior
2. RED    — Run the test, confirm it fails (for the right reason)
3. GREEN  — Write the minimum code to make the test pass
4. GREEN  — Run the test suite, confirm it passes
5. FIX    — If any test fails, fix code until all tests pass
6. REFACTOR — Clean up code while keeping all tests green
7. REPEAT — Pick the next test case from the Test Matrix and go to step 1
```

**Rules:**
- Never write production code without a failing test first
- Never skip step 2 — seeing the failure confirms the test tests what you think
- Each RED→GREEN cycle should be small (one row from the Test Matrix)
- Run the *full* test suite (`test_dir_command` from config.json) after each GREEN, not just the new test
- Commit after completing each story (all its Test Matrix rows are green)

---

## Phase 1 — Lexer
*Stories: E1.1*

### TDD Steps
1. **RED** — Create `tests/test-lexer.rkt`, write test: `(tokenize "") → '()`
2. **RED** — Run `raco test tests/test-lexer.rkt` → fails (no `tokenize`)
3. **GREEN** — Create `src/lexer.rkt` with minimal `tokenize` that returns `'()`
4. **GREEN** — Run tests → passes
5. **RED** — Write test: `(tokenize "42")` → `(list (token 'NUMBER 42))`
6. **RED** — Run → fails (no token struct, no number parsing)
7. **GREEN** — Define `token` struct, add number lexing
8. **GREEN** — Run tests → both pass
9. Continue for each row in E1.1 Test Matrix:
   - [x] Empty input
   - [x] Single number / negative / float
   - [x] String literal / string with escape
   - [x] Symbol / operator symbol
   - [x] Boolean `#t` / `#f`
   - [x] Parentheses
   - [x] Mixed expression `(+ 1 2)`
   - [x] Comment skipping
   - [x] Whitespace only
   - [x] Unterminated string → error
   - [x] Nested parens
10. **REFACTOR** — Clean up lexer code, ensure no duplication
11. **COMMIT** — All E1.1 tests green

## Phase 2 — Parser
*Stories: E1.2*

### TDD Steps
For each row in E1.2 Test Matrix:
1. **RED** — Write test in `tests/test-parser.rkt`
2. **RED** — Run → fails
3. **GREEN** — Implement just enough in `src/parser.rkt`
4. **GREEN** — Run full suite → all pass

Rows to cover (one RED→GREEN cycle each):
- [ ] Single atom: number
- [ ] Single atom: symbol
- [ ] Single atom: string
- [ ] Single atom: boolean
- [ ] Simple list `(+ 1 2)`
- [ ] Nested list `(+ (* 2 3) 4)`
- [ ] Empty list `()`
- [ ] Deeply nested `(a (b (c d)))`
- [ ] Quote sugar `'x` → `(quote x)`
- [ ] Quote list `'(1 2 3)`
- [ ] Multiple expressions via `read-all-from-string`
- [ ] Error: unmatched open paren
- [ ] Error: unmatched close paren
- [ ] Error: empty input
- **REFACTOR** then **COMMIT**

## Phase 3 — Environment
*Stories: E1.3*

### TDD Steps
For each row in E1.3 Test Matrix:
- [ ] Set and lookup
- [ ] Unbound lookup → error
- [ ] Parent chain lookup
- [ ] Shadowing
- [ ] Update existing binding
- [ ] Update unbound → error
- [ ] Extend with params/args
- [ ] Extend arity mismatch → error
- [ ] Parent not mutated by child
- **REFACTOR** then **COMMIT**

## Phase 4 — Core Evaluator
*Stories: E1.4, E1.5, E1.6, E1.7, E1.8, E1.9, E1.10*

### TDD Steps
Work through stories in order. For each story, work through its Test Matrix row by row:

**E1.4 — Self-evaluating & symbol lookup:**
- [ ] Integer → itself
- [ ] Float → itself
- [ ] Negative → itself
- [ ] String → itself
- [ ] Boolean true/false → itself
- [ ] Bound symbol → value
- [ ] Unbound symbol → error

**E1.5 — Quote:**
- [ ] `(quote foo)` → symbol
- [ ] `(quote 42)` → number
- [ ] `(quote (1 2 3))` → list
- [ ] `(quote (a (b c)))` → nested
- [ ] `(quote ())` → empty list
- [ ] `(quote)` → arity error
- [ ] `(quote a b)` → arity error

**E1.6 — If:**
- [ ] True branch
- [ ] False branch
- [ ] Truthy zero
- [ ] Truthy empty list
- [ ] No alternative (true)
- [ ] No alternative (false) → void
- [ ] Non-taken branch not evaluated
- [ ] No args → error

**E1.7 — Begin:**
- [ ] Single expr
- [ ] Two/three exprs → last
- [ ] Empty begin → void
- [ ] Side effect ordering
- [ ] Nested begin

**E1.8 — Define & Set!:**
- [ ] Simple define
- [ ] Define overwrites
- [ ] Define with expression
- [ ] Set! existing
- [ ] Set! parent binding
- [ ] Set! unbound → error
- [ ] Define returns void

**E1.9 — Lambda & closures:**
- [ ] Identity function
- [ ] Multi param
- [ ] Closure captures env
- [ ] Closure over closure (make-adder)
- [ ] Implicit begin in body
- [ ] No params
- [ ] Wrong arity → error
- [ ] Bad param list → error

**E1.10 — Function application:**
- [ ] Builtin call
- [ ] Closure call
- [ ] Nested calls
- [ ] Higher-order function
- [ ] Non-procedure in operator → error

**REFACTOR** then **COMMIT** after each story.

## Phase 5 — Builtins
*Stories: E1.11, E1.12, E1.13, E1.14, E1.15*

### TDD Steps
For each story, row-by-row through Test Matrix:

**E1.11 — Arithmetic:**
- [ ] `(+)` → 0
- [ ] `(+ 5)` → 5
- [ ] `(+ 1 2 3 4)` → 10
- [ ] `(+ 1.5 2.5)` → 4.0
- [ ] `(- 5)` → -5
- [ ] `(- 10 3)` → 7
- [ ] `(- 10 3 2)` → 5
- [ ] `(*)` → 1
- [ ] `(* 3 4)` → 12
- [ ] `(/ 10 2)` → 5
- [ ] `(/ 7 2)` → 3.5
- [ ] `(/ 1 0)` → error
- [ ] `(mod 10 3)` → 1
- [ ] `(mod 10 0)` → error
- [ ] `(+ 1 "a")` → error

**E1.12 — Comparison & equality:**
- [ ] `<`, `>`, `<=`, `>=` — true/false cases
- [ ] `=` — numeric equality
- [ ] `equal?` — atoms, strings, lists, nested, different
- [ ] Non-number arg → error

**E1.13 — List operations:**
- [ ] `cons` onto list / dotted
- [ ] `car` / `cdr`
- [ ] `list` empty / many
- [ ] `null?` / `pair?` — various types
- [ ] `car` of empty → error
- [ ] `cdr` of atom → error

**E1.14 — and/or/not:**
- [ ] `and` — all true, short-circuit, empty, one false
- [ ] `or` — first true, all false, short-circuit, empty
- [ ] `not` — true, false, truthy

**E1.15 — Type predicates & I/O:**
- [ ] Each predicate: positive and negative case
- [ ] `display` string / number
- [ ] `newline`

**REFACTOR** then **COMMIT** after each story.

## Phase 6 — REPL & Entry Point
*Stories: E1.16*

### TDD Steps
- [ ] Simple expression → prints result
- [ ] Computation → prints result
- [ ] Define then use across inputs
- [ ] Multi-line input (balanced parens)
- [ ] Error recovery (error then normal expr)
- [ ] Unbound variable error
- [ ] `(exit)` terminates
- [ ] `(quit)` terminates
- [ ] EOF terminates
- [ ] Wire `strawman.rkt`: no args → REPL, file arg → execute
- **REFACTOR** then **COMMIT**

## Phase 7 — Namespaces & Recursion
*Stories: E2.1 through E2.5*

### TDD Steps

**E2.1 — let:**
- [ ] Simple / two bindings / parallel semantics
- [ ] Shadowing / outer unchanged
- [ ] Body implicit begin / nested let / empty bindings
- [ ] Malformed → error

**E2.2 — let*:**
- [ ] Sequential deps / three deps / shadow across / empty

**E2.3 — letrec:**
- [ ] Self-recursive (factorial)
- [ ] Mutual recursion (even?/odd?)
- [ ] Non-lambda value

**E2.4 — Define shorthand:**
- [ ] `(define (f x) body)` — simple, multi-param, with body, recursive, no params

**E2.5 — Integration:**
- [ ] Factorial 0 / factorial 10
- [ ] Fibonacci 10
- [ ] Map with lambda
- [ ] Filter with lambda
- [ ] Closure scope (lexical vs dynamic)
- [ ] Accumulator (mutable closure)

**REFACTOR** then **COMMIT** after each story.

---

*Phases 8–15 below are deferred until the MVP (Phases 1–7) is solid.
Same TDD cycle applies: RED → RED → GREEN → GREEN → FIX → REFACTOR → REPEAT.*

---

## Phase 8 — Continuations
*Stories: E3.1 through E3.5*

### TDD Steps
- [ ] **E3.1** — Run all existing tests against CPS evaluator → must pass unchanged
- [ ] **E3.2** — `call/cc`: normal return, early exit, in expression, saved continuation, non-procedure error
- [ ] **E3.3** — `catch`/`throw`: no throw, simple throw, nested, wrong tag, throw in function
- [ ] **E3.4** — `block`/`return-from`: no return, early return, nested, inner, unknown block
- [ ] **E3.5** — `unwind-protect`: normal, with throw, cleanup order

## Phase 9 — Side Effects & Mutation
*Stories: E4.1 through E4.4*

### TDD Steps
- [ ] **E4.1** — Shared mutation between closures, lambda captures box
- [ ] **E4.2** — `set-car!`, `set-cdr!`, non-pair error
- [ ] **E4.3** — `eq?` vs `equal?`: same symbol, same number, different lists, same binding, structural equality
- [ ] **E4.4** — Vectors: make/ref, set/ref, length, type pred, out of bounds

## Phase 10 — Denotational Semantics
*Stories: E5.1, E5.2*

### TDD Steps
- [ ] **E5.1** — Write `docs/semantics.md`
- [ ] **E5.2** — Derive one test per valuation clause, run full suite

## Phase 11 — Fast Interpretation
*Stories: E6.1 through E6.3*

### TDD Steps
- [ ] **E6.1** — Run all existing tests through pretreat + fast-eval → identical results
- [ ] **E6.2** — Lexical addressing: local var, free var one level, multiple params
- [ ] **E6.3** — Benchmark harness: factorial, fibonacci, ackermann, map

## Phase 12 — Bytecode Compilation
*Stories: E7.1 through E7.4*

### TDD Steps
- [ ] **E7.1** — Document instruction set in `docs/bytecode.md`
- [ ] **E7.2** — Compile each core form, test bytecode output
- [ ] **E7.3** — VM execute: all existing tests via compile + VM → identical results
- [ ] **E7.4** — REPL with `--compiled` flag, all REPL tests pass in both modes

## Phase 13 — Eval & Reflection
*Stories: E8.1 through E8.2*

### TDD Steps
- [ ] **E8.1** — `eval`: simple, constructed, with define, nested, non-list
- [ ] **E8.2** — First-class envs: capture, isolation, type predicate

## Phase 14 — Macros
*Stories: E9.1 through E9.4*

### TDD Steps
- [ ] **E9.1** — `define-macro`: simple macro, swap macro, expansion
- [ ] **E9.2** — `macroexpand`: expand once, non-macro unchanged
- [ ] **E9.3** — Quasiquote: simple unquote, splicing, nested, no unquote
- [ ] **E9.4** — Standard macros: `cond` (first/second/else/none), `when`, `unless`

## Phase 15 — Object System
*Stories: E10.1 through E10.4*

### TDD Steps
- [ ] **E10.1** — `define-class`: simple, subclass, duplicate field error, unknown parent error
- [ ] **E10.2** — Instantiation: construct, access, mutate, predicate, wrong arity, wrong type
- [ ] **E10.3** — Generics: single method, inherited, no method, override
- [ ] **E10.4** — Inheritance: inherited field, own field, `is-a?` direct/parent/unrelated

---

## File Structure

```
strawman/
  strawman.rkt           # Entry point (Phase 6)
  src/
    lexer.rkt            # Phase 1
    parser.rkt           # Phase 2
    env.rkt              # Phase 3
    eval.rkt             # Phase 4
    builtins.rkt         # Phase 5
    repl.rkt             # Phase 6
  tests/
    test-lexer.rkt       # Phase 1
    test-parser.rkt      # Phase 2
    test-env.rkt         # Phase 3
    test-eval.rkt        # Phase 4
    test-builtins.rkt    # Phase 5
    test-repl.rkt        # Phase 6
    test-integration.rkt # Phase 7
  examples/
    hello.straw
    factorial.straw
    fibonacci.straw
  docs/
    semantics.md         # Phase 10
    bytecode.md          # Phase 12
  bench/
    run.rkt              # Phase 11
  spec.md
  plan.md
  CLAUDE.md
  README.md
```

## Definition of Done (per phase)

1. Every Test Matrix row has a corresponding failing-then-passing test
2. Full suite passes (run `test_dir_command` from config.json)
3. No regressions in previous phases
4. Code refactored (no duplication, clear naming)
5. Committed with a descriptive message
