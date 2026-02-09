# Strawman Implementation Plan

## Phase 1 — Lexer

- [ ] [P1][E1.1] Empty input
- [ ] [P1][E1.1] Single number / negative / float
- [ ] [P1][E1.1] String literal / string with escape
- [ ] [P1][E1.1] Symbol / operator symbol
- [ ] [P1][E1.1] Boolean #t / #f
- [ ] [P1][E1.1] Parentheses
- [ ] [P1][E1.1] Mixed expression (+ 1 2)
- [ ] [P1][E1.1] Comment skipping
- [ ] [P1][E1.1] Whitespace only
- [ ] [P1][E1.1] Unterminated string -> error
- [ ] [P1][E1.1] Nested parens

## Phase 2 — Parser

- [ ] [P2][E1.2] Single atom: number
- [ ] [P2][E1.2] Single atom: symbol
- [ ] [P2][E1.2] Single atom: string
- [ ] [P2][E1.2] Single atom: boolean
- [ ] [P2][E1.2] Simple list (+ 1 2)
- [ ] [P2][E1.2] Nested list (+ (* 2 3) 4)
- [ ] [P2][E1.2] Empty list ()
- [ ] [P2][E1.2] Deeply nested (a (b (c d)))
- [ ] [P2][E1.2] Quote sugar 'x -> (quote x)
- [ ] [P2][E1.2] Quote list '(1 2 3)
- [ ] [P2][E1.2] Multiple expressions via read-all-from-string
- [ ] [P2][E1.2] Error: unmatched open paren
- [ ] [P2][E1.2] Error: unmatched close paren
- [ ] [P2][E1.2] Error: empty input

## Phase 3 — Environment

- [ ] [P3][E1.3] Set and lookup
- [ ] [P3][E1.3] Unbound lookup -> error
- [ ] [P3][E1.3] Parent chain lookup
- [ ] [P3][E1.3] Shadowing
- [ ] [P3][E1.3] Update existing binding
- [ ] [P3][E1.3] Update unbound -> error
- [ ] [P3][E1.3] Extend with params/args
- [ ] [P3][E1.3] Extend arity mismatch -> error
- [ ] [P3][E1.3] Parent not mutated by child

## Phase 4 — Core Evaluator

- [ ] [P4][E1.4] Integer -> itself
- [ ] [P4][E1.4] Float -> itself
- [ ] [P4][E1.4] Negative -> itself
- [ ] [P4][E1.4] String -> itself
- [ ] [P4][E1.4] Boolean true/false -> itself
- [ ] [P4][E1.4] Bound symbol -> value
- [ ] [P4][E1.4] Unbound symbol -> error
- [ ] [P4][E1.5] (quote foo) -> symbol
- [ ] [P4][E1.5] (quote 42) -> number
- [ ] [P4][E1.5] (quote (1 2 3)) -> list
- [ ] [P4][E1.5] (quote (a (b c))) -> nested
- [ ] [P4][E1.5] (quote ()) -> empty list
- [ ] [P4][E1.5] (quote) -> arity error
- [ ] [P4][E1.5] (quote a b) -> arity error
- [ ] [P4][E1.6] True branch
- [ ] [P4][E1.6] False branch
- [ ] [P4][E1.6] Truthy zero
- [ ] [P4][E1.6] Truthy empty list
- [ ] [P4][E1.6] No alternative (true)
- [ ] [P4][E1.6] No alternative (false) -> void
- [ ] [P4][E1.6] Non-taken branch not evaluated
- [ ] [P4][E1.6] No args -> error
- [ ] [P4][E1.7] Single expr
- [ ] [P4][E1.7] Two/three exprs -> last
- [ ] [P4][E1.7] Empty begin -> void
- [ ] [P4][E1.7] Side effect ordering
- [ ] [P4][E1.7] Nested begin
- [ ] [P4][E1.8] Simple define
- [ ] [P4][E1.8] Define overwrites
- [ ] [P4][E1.8] Define with expression
- [ ] [P4][E1.8] Set! existing
- [ ] [P4][E1.8] Set! parent binding
- [ ] [P4][E1.8] Set! unbound -> error
- [ ] [P4][E1.8] Define returns void
- [ ] [P4][E1.9] Identity function
- [ ] [P4][E1.9] Multi param
- [ ] [P4][E1.9] Closure captures env
- [ ] [P4][E1.9] Closure over closure (make-adder)
- [ ] [P4][E1.9] Implicit begin in body
- [ ] [P4][E1.9] No params
- [ ] [P4][E1.9] Wrong arity -> error
- [ ] [P4][E1.9] Bad param list -> error
- [ ] [P4][E1.10] Builtin call
- [ ] [P4][E1.10] Closure call
- [ ] [P4][E1.10] Nested calls
- [ ] [P4][E1.10] Higher-order function
- [ ] [P4][E1.10] Non-procedure in operator -> error

## Phase 5 — Builtins

- [ ] [P5][E1.11] (+) -> 0
- [ ] [P5][E1.11] (+ 5) -> 5
- [ ] [P5][E1.11] (+ 1 2 3 4) -> 10
- [ ] [P5][E1.11] (+ 1.5 2.5) -> 4.0
- [ ] [P5][E1.11] (- 5) -> -5
- [ ] [P5][E1.11] (- 10 3) -> 7
- [ ] [P5][E1.11] (- 10 3 2) -> 5
- [ ] [P5][E1.11] (*) -> 1
- [ ] [P5][E1.11] (* 3 4) -> 12
- [ ] [P5][E1.11] (/ 10 2) -> 5
- [ ] [P5][E1.11] (/ 7 2) -> 3.5
- [ ] [P5][E1.11] (/ 1 0) -> error
- [ ] [P5][E1.11] (mod 10 3) -> 1
- [ ] [P5][E1.11] (mod 10 0) -> error
- [ ] [P5][E1.11] (+ 1 "a") -> error
- [ ] [P5][E1.12] < > <= >= true/false cases
- [ ] [P5][E1.12] = numeric equality
- [ ] [P5][E1.12] equal? atoms, strings, lists, nested, different
- [ ] [P5][E1.12] Non-number arg -> error
- [ ] [P5][E1.13] cons onto list / dotted
- [ ] [P5][E1.13] car / cdr
- [ ] [P5][E1.13] list empty / many
- [ ] [P5][E1.13] null? / pair? various types
- [ ] [P5][E1.13] car of empty -> error
- [ ] [P5][E1.13] cdr of atom -> error
- [ ] [P5][E1.14] and: all true, short-circuit, empty, one false
- [ ] [P5][E1.14] or: first true, all false, short-circuit, empty
- [ ] [P5][E1.14] not: true, false, truthy
- [ ] [P5][E1.15] Each predicate: positive and negative case
- [ ] [P5][E1.15] display string / number
- [ ] [P5][E1.15] newline

## Phase 6 — REPL & Entry Point

- [ ] [P6][E1.16] Simple expression -> prints result
- [ ] [P6][E1.16] Computation -> prints result
- [ ] [P6][E1.16] Define then use across inputs
- [ ] [P6][E1.16] Multi-line input (balanced parens)
- [ ] [P6][E1.16] Error recovery (error then normal expr)
- [ ] [P6][E1.16] Unbound variable error
- [ ] [P6][E1.16] (exit) terminates
- [ ] [P6][E1.16] (quit) terminates
- [ ] [P6][E1.16] EOF terminates
- [ ] [P6][E1.16] Wire strawman.rkt: no args -> REPL, file arg -> execute

## Phase 7 — Namespaces & Recursion

- [ ] [P7][E2.1] Simple / two bindings / parallel semantics
- [ ] [P7][E2.1] Shadowing / outer unchanged
- [ ] [P7][E2.1] Body implicit begin / nested let / empty bindings
- [ ] [P7][E2.1] Malformed -> error
- [ ] [P7][E2.2] Sequential deps / three deps / shadow across / empty
- [ ] [P7][E2.3] Self-recursive (factorial)
- [ ] [P7][E2.3] Mutual recursion (even?/odd?)
- [ ] [P7][E2.3] Non-lambda value
- [ ] [P7][E2.4] (define (f x) body) simple, multi-param, with body, recursive, no params
- [ ] [P7][E2.5] Factorial 0 / factorial 10
- [ ] [P7][E2.5] Fibonacci 10
- [ ] [P7][E2.5] Map with lambda
- [ ] [P7][E2.5] Filter with lambda
- [ ] [P7][E2.5] Closure scope (lexical vs dynamic)
- [ ] [P7][E2.5] Accumulator (mutable closure)

## Phase 8 — Continuations

- [ ] [P8][E3.1] Run all existing tests against CPS evaluator -> must pass unchanged
- [ ] [P8][E3.2] call/cc: normal return, early exit, in expression, saved continuation, non-procedure error
- [ ] [P8][E3.3] catch/throw: no throw, simple throw, nested, wrong tag, throw in function
- [ ] [P8][E3.4] block/return-from: no return, early return, nested, inner, unknown block
- [ ] [P8][E3.5] unwind-protect: normal, with throw, cleanup order

## Phase 9 — Side Effects & Mutation

- [ ] [P9][E4.1] Shared mutation between closures, lambda captures box
- [ ] [P9][E4.2] set-car!, set-cdr!, non-pair error
- [ ] [P9][E4.3] eq? vs equal?: same symbol, same number, different lists, same binding, structural equality
- [ ] [P9][E4.4] Vectors: make/ref, set/ref, length, type pred, out of bounds

## Phase 10 — Denotational Semantics

- [ ] [P10][E5.1] Write docs/semantics.md
- [ ] [P10][E5.2] Derive one test per valuation clause, run full suite

## Phase 11 — Fast Interpretation

- [ ] [P11][E6.1] Run all existing tests through pretreat + fast-eval -> identical results
- [ ] [P11][E6.2] Lexical addressing: local var, free var one level, multiple params
- [ ] [P11][E6.3] Benchmark harness: factorial, fibonacci, ackermann, map

## Phase 12 — Bytecode Compilation

- [ ] [P12][E7.1] Document instruction set in docs/bytecode.md
- [ ] [P12][E7.2] Compile each core form, test bytecode output
- [ ] [P12][E7.3] VM execute: all existing tests via compile + VM -> identical results
- [ ] [P12][E7.4] REPL with --compiled flag, all REPL tests pass in both modes

## Phase 13 — Eval & Reflection

- [ ] [P13][E8.1] eval: simple, constructed, with define, nested, non-list
- [ ] [P13][E8.2] First-class envs: capture, isolation, type predicate

## Phase 14 — Macros

- [ ] [P14][E9.1] define-macro: simple macro, swap macro, expansion
- [ ] [P14][E9.2] macroexpand: expand once, non-macro unchanged
- [ ] [P14][E9.3] Quasiquote: simple unquote, splicing, nested, no unquote
- [ ] [P14][E9.4] Standard macros: cond (first/second/else/none), when, unless

## Phase 15 — Object System

- [ ] [P15][E10.1] define-class: simple, subclass, duplicate field error, unknown parent error
- [ ] [P15][E10.2] Instantiation: construct, access, mutate, predicate, wrong arity, wrong type
- [ ] [P15][E10.3] Generics: single method, inherited, no method, override
- [ ] [P15][E10.4] Inheritance: inherited field, own field, is-a? direct/parent/unrelated
