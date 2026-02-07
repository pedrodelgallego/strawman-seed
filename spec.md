# Strawman Specification

A minimal Lisp interpreter in Racket, structured around the chapters of
*Lisp in Small Pieces* by Christian Queinnec.

Each epic corresponds to a chapter of the book. Epics 1–2 form the MVP.
Later epics extend the interpreter toward a more complete system.

---

## Epic 1 — Basic Interpreter
*Based on LiSP Chapter 1: The Basics of Interpretation*

The foundational eval/apply interpreter. Given a string, tokenize it, parse it
into an S-expression, and evaluate it in an environment.

---

### E1.1 — Lexer: Tokenize source text

#### Narrative
As a **compiler developer**
I want **a tokenizer that converts raw source strings into a flat list of typed tokens**
So that **downstream phases (parser) operate on clean, classified units instead of raw characters**

#### Requirements
##### Functional
- `tokenize : string -> (listof token)` is the sole public interface
- Token types: `LPAREN`, `RPAREN`, `NUMBER`, `STRING`, `SYMBOL`, `BOOLEAN`
- Numbers: integers (`42`, `-3`) and floats (`3.14`, `-0.5`)
- Strings: delimited by `"`, supporting `\"` and `\\` escapes
- Symbols: any sequence of non-whitespace, non-paren chars that isn't a number, string, or boolean (includes `+`, `-`, `<=`, `define`, `foo?`, `set!`)
- Booleans: `#t` and `#f` only
- Comments: `;` discards to end of line
- Whitespace (space, tab, newline) is skipped between tokens

##### Non-functional
- Correctness: tokenizing then re-joining tokens must preserve all semantic content of the input
- Diagnostics: on malformed input (unterminated string), raise an error with a descriptive message
- Compatibility: token set matches a Scheme subset — no Racket-specific extensions

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Empty | `""` | `'()` |
| Single number | `"42"` | `(list (token 'NUMBER 42))` |
| Negative number | `"-3"` | `(list (token 'NUMBER -3))` |
| Float | `"3.14"` | `(list (token 'NUMBER 3.14))` |
| String | `"\"hello\""` | `(list (token 'STRING "hello"))` |
| String with escape | `"\"a\\\"b\""` | `(list (token 'STRING "a\"b"))` |
| Symbol | `"foo"` | `(list (token 'SYMBOL 'foo))` |
| Operator symbol | `"+"` | `(list (token 'SYMBOL '+))` |
| Boolean true | `"#t"` | `(list (token 'BOOLEAN #t))` |
| Boolean false | `"#f"` | `(list (token 'BOOLEAN #f))` |
| Parens | `"()"` | `(list (token 'LPAREN "(") (token 'RPAREN ")"))` |
| Mixed expr | `"(+ 1 2)"` | 4 tokens: LPAREN, SYMBOL +, NUMBER 1, NUMBER 2, RPAREN |
| Comment skipped | `"42 ; ignore"` | `(list (token 'NUMBER 42))` |
| Whitespace only | `"   \n\t  "` | `'()` |
| Unterminated string | `"\"abc"` | Error: unterminated string |
| Nested parens | `"((a))"` | LPAREN LPAREN SYMBOL RPAREN RPAREN |

#### Acceptance Criteria
1. **Given** the tokenizer is called with `"(+ (* 2 3) 4)"`
   **When** `tokenize` runs
   **Then** it returns 9 tokens in correct order: `( + ( * 2 3 ) 4 )`

2. **Given** input contains a comment `"42 ; this is ignored\n7"`
   **When** `tokenize` runs
   **Then** only tokens for `42` and `7` are returned

3. **Given** input `"\"unterminated`
   **When** `tokenize` runs
   **Then** it raises an error whose message contains `"unterminated string"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **lexer**
- [ ] No regressions in existing test suite
- [ ] `raco test tests/test-lexer.rkt` passes

---

### E1.2 — Parser: Build S-expressions from tokens

#### Narrative
As a **compiler developer**
I want **a parser that converts a flat token list into nested S-expression ASTs**
So that **the evaluator receives structured, tree-shaped data instead of a linear stream**

#### Requirements
##### Functional
- `parse : (listof token) -> (values s-expr (listof token))` consumes tokens and returns an AST plus remaining tokens
- `read-from-string : string -> s-expr` is the convenience wrapper (tokenize + parse)
- Atoms: NUMBER token → Racket number, STRING → Racket string, BOOLEAN → Racket boolean, SYMBOL → Racket symbol
- Lists: `LPAREN ... RPAREN` → Racket list of parsed children
- Quote sugar: `'x` parses as `(quote x)` — handled either in lexer (emit QUOTE token) or parser
- Multiple top-level expressions: `read-all-from-string : string -> (listof s-expr)`

##### Non-functional
- Correctness: `(read-from-string "(+ 1 2)")` must produce the list `'(+ 1 2)` where `+` is a symbol
- Diagnostics: unmatched `)` → error with "unexpected closing paren"; unmatched `(` → error with "unexpected end of input"; empty input → error with "empty input"
- Compatibility: the parsed AST uses standard Racket values so the evaluator needs no intermediate representation

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Single atom number | `"42"` | `42` |
| Single atom symbol | `"foo"` | `'foo` |
| Single atom string | `"\"hi\""` | `"hi"` |
| Single atom bool | `"#t"` | `#t` |
| Simple list | `"(+ 1 2)"` | `'(+ 1 2)` |
| Nested list | `"(+ (* 2 3) 4)"` | `'(+ (* 2 3) 4)` |
| Empty list | `"()"` | `'()` |
| Deeply nested | `"(a (b (c d)))"` | `'(a (b (c d)))` |
| Quote sugar | `"'x"` | `'(quote x)` |
| Quote list | `"'(1 2 3)"` | `'(quote (1 2 3))` |
| Multiple exprs | `"1 2 3"` | `read-all` → `'(1 2 3)` |
| Unmatched open | `"(+ 1"` | Error: unexpected end of input |
| Unmatched close | `")"` | Error: unexpected closing paren |
| Empty input | `""` | Error: empty input |

#### Acceptance Criteria
1. **Given** input `"(define (square x) (* x x))"`
   **When** `read-from-string` runs
   **Then** it returns `'(define (square x) (* x x))`

2. **Given** input `"(+ 1"` (missing close paren)
   **When** `read-from-string` runs
   **Then** it raises an error whose message contains `"unexpected end of input"`

3. **Given** input `"'(a b c)"`
   **When** `read-from-string` runs
   **Then** it returns `'(quote (a b c))`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **parser**
- [ ] No regressions in existing test suite
- [ ] `raco test tests/test-parser.rkt` passes

---

### E1.3 — Environment: Variable bindings with lexical scope

#### Narrative
As a **compiler developer**
I want **an environment data structure that maps symbols to values with parent-chain lookup**
So that **the evaluator can implement lexical scoping, `define`, `set!`, and function parameters**

#### Requirements
##### Functional
- `make-env : [env?] -> env` — create an environment, optionally with a parent
- `env-lookup : env × symbol -> value` — search current frame, then parent chain
- `env-set! : env × symbol × value -> void` — bind in the current frame (for `define`)
- `env-update! : env × symbol × value -> void` — mutate nearest existing binding (for `set!`), error if unbound
- `env-extend : env × (listof symbol) × (listof value) -> env` — create child frame binding params to args

##### Non-functional
- Correctness: lookup must find the nearest binding in the chain (shadowing works)
- Diagnostics: `env-lookup` on unbound symbol → error with `"unbound variable: <name>"`; `env-update!` on unbound symbol → error with `"cannot set! unbound variable: <name>"`
- Performance: O(1) per frame lookup (hash-based), O(d) chain walk where d = scope depth

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Set and lookup | set `x`→1, lookup `x` | `1` |
| Unbound lookup | lookup `y` in empty env | Error: `"unbound variable: y"` |
| Parent lookup | parent has `x`→1, child has no `x` | child lookup `x` → `1` |
| Shadowing | parent `x`→1, child `x`→2 | child lookup `x` → `2` |
| Update existing | set `x`→1, update `x`→2, lookup `x` | `2` |
| Update unbound | update `y` in empty env | Error: `"cannot set! unbound variable: y"` |
| Extend | extend env with `(a b)` → `(1 2)` | child lookup `a`→`1`, `b`→`2` |
| Extend arity mismatch | extend with `(a b)` → `(1)` | Error: arity mismatch |
| Parent not mutated | extend, set `x` in child | parent lookup `x` → unbound |

#### Acceptance Criteria
1. **Given** a parent env with `x`→10 and a child created via `env-extend`
   **When** the child looks up `x`
   **Then** it returns `10`

2. **Given** a child env that shadows `x`→20 over parent `x`→10
   **When** the child looks up `x`
   **Then** it returns `20` and parent still has `10`

3. **Given** an empty environment
   **When** `env-update!` is called with symbol `z`
   **Then** it raises an error whose message contains `"cannot set! unbound variable: z"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **environment**
- [ ] No regressions in existing test suite
- [ ] `raco test tests/test-env.rkt` passes

---

### E1.4 — Evaluator: Self-evaluating forms and symbol lookup

#### Narrative
As a **language user**
I want **numbers, strings, and booleans to evaluate to themselves, and symbols to resolve from the environment**
So that **the most basic expressions produce values and variables work**

#### Requirements
##### Functional
- `straw-eval : s-expr × env -> value`
- Numbers → return as-is
- Strings → return as-is
- Booleans → return as-is
- Symbols → `env-lookup` in the current environment

##### Non-functional
- Correctness: self-eval forms are identity; symbols resolve to the nearest binding
- Diagnostics: unbound symbol → error `"unbound variable: <name>"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Integer | `42` | `42` |
| Float | `3.14` | `3.14` |
| Negative | `-7` | `-7` |
| String | `"hello"` | `"hello"` |
| Boolean true | `#t` | `#t` |
| Boolean false | `#f` | `#f` |
| Bound symbol | `x` where env has `x`→5 | `5` |
| Unbound symbol | `y` in empty env | Error: `"unbound variable: y"` |

#### Acceptance Criteria
1. **Given** `straw-eval` called with `42` and any env
   **When** evaluation completes
   **Then** it returns `42`

2. **Given** env where `x`→`"hello"`
   **When** `straw-eval` called with symbol `x`
   **Then** it returns `"hello"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite
- [ ] `raco test tests/test-eval.rkt` passes

---

### E1.5 — Evaluator: `quote`

#### Narrative
As a **language user**
I want **`(quote expr)` to return its argument unevaluated**
So that **I can treat code as data and construct symbolic expressions**

#### Requirements
##### Functional
- `(quote <datum>)` returns `<datum>` without evaluating it
- Works for atoms: `(quote foo)` → symbol `foo`
- Works for lists: `(quote (1 2 3))` → the list `(1 2 3)`
- Works for nested: `(quote (a (b c)))` → `(a (b c))`

##### Non-functional
- Correctness: the returned value must be structurally identical to the datum in the source
- Diagnostics: `(quote)` or `(quote a b)` → error `"quote expects exactly one argument"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Quote symbol | `(quote foo)` | `'foo` |
| Quote number | `(quote 42)` | `42` |
| Quote list | `(quote (1 2 3))` | `'(1 2 3)` |
| Quote nested | `(quote (a (b c)))` | `'(a (b c))` |
| Quote empty list | `(quote ())` | `'()` |
| No args | `(quote)` | Error: quote expects exactly one argument |
| Too many args | `(quote a b)` | Error: quote expects exactly one argument |

#### Acceptance Criteria
1. **Given** `(quote (+ 1 2))`
   **When** evaluated
   **Then** returns the list `(+ 1 2)`, NOT `3`

2. **Given** `(quote foo)` where `foo` is not bound
   **When** evaluated
   **Then** returns the symbol `foo` without error

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.6 — Evaluator: `if` conditional

#### Narrative
As a **language user**
I want **`(if test consequent alternative)` to evaluate only the chosen branch**
So that **I can write conditional logic and the non-taken branch has no side effects**

#### Requirements
##### Functional
- `(if test consequent alternative)` — evaluate `test`; if truthy evaluate `consequent`, else evaluate `alternative`
- Only `#f` is falsy; everything else (including `0`, `""`, `'()`) is truthy
- `(if test consequent)` with no alternative returns `void` when test is false

##### Non-functional
- Correctness: the non-taken branch must NOT be evaluated (verifiable via side effects)
- Diagnostics: `(if)` or `(if test)` with wrong arity → error `"if expects 2 or 3 arguments"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| True branch | `(if #t 1 2)` | `1` |
| False branch | `(if #f 1 2)` | `2` |
| Truthy zero | `(if 0 "yes" "no")` | `"yes"` |
| Truthy empty list | `(if '() "yes" "no")` | `"yes"` |
| No alternative (true) | `(if #t 42)` | `42` |
| No alternative (false) | `(if #f 42)` | `(void)` |
| Non-taken not evaled | `(if #t 1 (error "boom"))` | `1` (no error) |
| No args | `(if)` | Error: if expects 2 or 3 arguments |

#### Acceptance Criteria
1. **Given** `(if (> 3 2) "yes" "no")`
   **When** evaluated
   **Then** returns `"yes"`

2. **Given** `(if #f (error "should not run") 42)`
   **When** evaluated
   **Then** returns `42` without raising an error

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.7 — Evaluator: `begin` sequences

#### Narrative
As a **language user**
I want **`(begin e1 e2 ... en)` to evaluate each expression in order and return the last result**
So that **I can sequence side effects and group multiple expressions where one is expected**

#### Requirements
##### Functional
- `(begin e1 e2 ... en)` evaluates left-to-right, returns value of `en`
- `(begin)` with no args returns `void`
- Side effects from earlier expressions are visible to later ones

##### Non-functional
- Correctness: evaluation order is strictly left-to-right
- Diagnostics: none specific (any sub-expression may raise its own errors)

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Single expr | `(begin 42)` | `42` |
| Two exprs | `(begin 1 2)` | `2` |
| Three exprs | `(begin 1 2 3)` | `3` |
| Empty begin | `(begin)` | `(void)` |
| Side effect order | `(begin (define x 1) (define x 2) x)` | `2` |
| Nested begin | `(begin (begin 1 2) 3)` | `3` |

#### Acceptance Criteria
1. **Given** `(begin (define x 10) (+ x 5))`
   **When** evaluated
   **Then** returns `15`

2. **Given** `(begin)`
   **When** evaluated
   **Then** returns `void`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.8 — Evaluator: `define` and `set!`

#### Narrative
As a **language user**
I want **`define` to create new bindings and `set!` to mutate existing ones**
So that **I can name values and update state when needed**

#### Requirements
##### Functional
- `(define name expr)` — evaluate `expr`, bind `name` in the current frame
- `(set! name expr)` — evaluate `expr`, update the nearest existing binding of `name`
- `define` returns `void`
- `set!` returns `void`
- `set!` on an unbound variable is an error

##### Non-functional
- Correctness: `define` always writes to the current frame; `set!` walks the chain
- Diagnostics: `(set! unbound 1)` → `"cannot set! unbound variable: unbound"`; `(define)` → arity error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple define | `(begin (define x 42) x)` | `42` |
| Define overwrites | `(begin (define x 1) (define x 2) x)` | `2` |
| Define expr | `(begin (define x (+ 1 2)) x)` | `3` |
| Set! existing | `(begin (define x 1) (set! x 99) x)` | `99` |
| Set! parent | `(begin (define x 1) ((lambda () (set! x 5))) x)` | `5` |
| Set! unbound | `(set! z 1)` | Error: cannot set! unbound variable |
| Define returns void | `(define x 1)` | `(void)` |

#### Acceptance Criteria
1. **Given** `(begin (define x 1) (set! x 2) x)`
   **When** evaluated
   **Then** returns `2`

2. **Given** `(set! never-defined 42)` in a fresh environment
   **When** evaluated
   **Then** raises error containing `"cannot set! unbound variable"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.9 — Evaluator: `lambda` and closures

#### Narrative
As a **language user**
I want **`(lambda (params...) body)` to create a first-class function that captures its lexical environment**
So that **I can define reusable abstractions, pass functions as values, and build closures**

#### Requirements
##### Functional
- `(lambda (p1 p2 ...) body)` returns a closure value
- The closure captures the environment at the point of creation
- The body is evaluated in an extended environment where params are bound to arguments
- A closure is a distinct value type (not a Racket procedure)
- Body may be multiple expressions (implicit `begin`)

##### Non-functional
- Correctness: free variables in the body resolve in the captured env, not the call-site env
- Diagnostics: `(lambda)` → error; `(lambda "bad" body)` → error about param list format

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Identity | `((lambda (x) x) 42)` | `42` |
| Multi param | `((lambda (x y) (+ x y)) 3 4)` | `7` |
| Closure captures | `(begin (define a 10) (define f (lambda (x) (+ x a))) (f 5))` | `15` |
| Closure over closure | `(begin (define make-adder (lambda (n) (lambda (x) (+ n x)))) ((make-adder 3) 7))` | `10` |
| Implicit begin | `((lambda (x) (define y 1) (+ x y)) 5)` | `6` |
| No params | `((lambda () 42))` | `42` |
| Wrong arity | `((lambda (x) x) 1 2)` | Error: arity mismatch |
| Bad param list | `(lambda 42 x)` | Error: expected parameter list |

#### Acceptance Criteria
1. **Given** `(begin (define make-counter (lambda () (define n 0) (lambda () (set! n (+ n 1)) n))) (define c (make-counter)) (c) (c) (c))`
   **When** evaluated
   **Then** the three calls return `1`, `2`, `3` — demonstrating mutable closure state

2. **Given** `((lambda (x) x))` (missing argument)
   **When** evaluated
   **Then** raises error about arity mismatch

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.10 — Evaluator: Function application

#### Narrative
As a **language user**
I want **`(f arg1 arg2 ...)` to evaluate the operator, evaluate the arguments, and call the function**
So that **both user-defined closures and built-in primitives can be invoked uniformly**

#### Requirements
##### Functional
- Evaluate the operator position to get a callable value
- Evaluate all arguments left-to-right
- If operator is a closure: extend its captured env with params→args, evaluate body
- If operator is a builtin: call the underlying Racket procedure with the args
- Non-callable in operator position is an error

##### Non-functional
- Correctness: argument evaluation order is left-to-right (observable via side effects)
- Diagnostics: `(42 1 2)` → `"not a procedure: 42"`; wrong number of args → arity error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Builtin call | `(+ 1 2)` | `3` |
| Closure call | `((lambda (x) (* x x)) 5)` | `25` |
| Nested call | `(+ (* 2 3) (- 10 4))` | `12` |
| Higher-order | `((lambda (f x) (f x)) (lambda (n) (* n 2)) 5)` | `10` |
| Non-procedure | `(42)` | Error: not a procedure |
| Non-procedure string | `("hello" 1)` | Error: not a procedure |

#### Acceptance Criteria
1. **Given** `(+ 1 2 3 4)`
   **When** evaluated with variadic `+`
   **Then** returns `10`

2. **Given** `(1 2 3)`
   **When** evaluated
   **Then** raises error containing `"not a procedure"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E1.11 — Builtins: Arithmetic

#### Narrative
As a **language user**
I want **`+`, `-`, `*`, `/`, `mod` available as built-in procedures**
So that **I can perform numeric computation without defining these myself**

#### Requirements
##### Functional
- `+` : variadic, `(+)` → `0`, `(+ a)` → `a`, `(+ a b ...)` → sum
- `-` : `(- a)` → negation, `(- a b ...)` → left fold subtraction
- `*` : variadic, `(*)` → `1`, `(* a b ...)` → product
- `/` : `(/ a b)` → quotient (integer div if both int, else float)
- `mod` : `(mod a b)` → remainder

##### Non-functional
- Correctness: integer arithmetic stays exact; float contaminates
- Diagnostics: `/` or `mod` by zero → `"division by zero"`; non-numeric arg → `"expected number"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Add zero args | `(+)` | `0` |
| Add one | `(+ 5)` | `5` |
| Add many | `(+ 1 2 3 4)` | `10` |
| Add floats | `(+ 1.5 2.5)` | `4.0` |
| Sub negate | `(- 5)` | `-5` |
| Sub two | `(- 10 3)` | `7` |
| Sub many | `(- 10 3 2)` | `5` |
| Mul zero args | `(*)` | `1` |
| Mul | `(* 3 4)` | `12` |
| Div int | `(/ 10 2)` | `5` |
| Div float | `(/ 7 2)` | `3.5` |
| Div zero | `(/ 1 0)` | Error: division by zero |
| Mod | `(mod 10 3)` | `1` |
| Mod zero | `(mod 10 0)` | Error: division by zero |
| Non-number | `(+ 1 "a")` | Error: expected number |

#### Acceptance Criteria
1. **Given** `(+ (* 3 4) (- 10 5))`
   **When** evaluated
   **Then** returns `17`

2. **Given** `(/ 1 0)`
   **When** evaluated
   **Then** raises error containing `"division by zero"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **builtins**
- [ ] No regressions in existing test suite

---

### E1.12 — Builtins: Comparison and equality

#### Narrative
As a **language user**
I want **`<`, `>`, `<=`, `>=`, `=`, `equal?` as built-in procedures**
So that **I can compare values and write conditional logic**

#### Requirements
##### Functional
- `<`, `>`, `<=`, `>=` : numeric comparison, two arguments
- `=` : numeric equality (not structural)
- `equal?` : deep structural equality (works on all types)

##### Non-functional
- Correctness: numeric comparisons work on integers and floats; `equal?` recurses into lists
- Diagnostics: non-numeric arg to `<` etc → `"expected number"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Less true | `(< 1 2)` | `#t` |
| Less false | `(< 2 1)` | `#f` |
| Less equal | `(< 2 2)` | `#f` |
| Greater | `(> 3 1)` | `#t` |
| Leq true | `(<= 2 2)` | `#t` |
| Geq false | `(>= 1 2)` | `#f` |
| Num eq | `(= 5 5)` | `#t` |
| Num neq | `(= 5 6)` | `#f` |
| Equal? atoms | `(equal? 42 42)` | `#t` |
| Equal? strings | `(equal? "ab" "ab")` | `#t` |
| Equal? lists | `(equal? '(1 2) '(1 2))` | `#t` |
| Equal? diff | `(equal? '(1 2) '(1 3))` | `#f` |
| Non-number | `(< "a" "b")` | Error: expected number |

#### Acceptance Criteria
1. **Given** `(< 1 2)`
   **When** evaluated
   **Then** returns `#t`

2. **Given** `(equal? (list 1 (list 2 3)) (list 1 (list 2 3)))`
   **When** evaluated
   **Then** returns `#t`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **builtins**
- [ ] No regressions in existing test suite

---

### E1.13 — Builtins: List operations

#### Narrative
As a **language user**
I want **`cons`, `car`, `cdr`, `list`, `null?`, `pair?` as built-in procedures**
So that **I can construct and destructure lists, the fundamental Lisp data structure**

#### Requirements
##### Functional
- `(cons a b)` → a pair; `(cons 1 '(2 3))` → `(1 2 3)`
- `(car p)` → first element of pair
- `(cdr p)` → rest of pair
- `(list a b ...)` → build a proper list; variadic
- `(null? x)` → `#t` iff `x` is the empty list
- `(pair? x)` → `#t` iff `x` is a cons pair

##### Non-functional
- Correctness: `(cons 1 2)` creates an improper pair (dotted pair); `(car '())` is an error
- Diagnostics: `(car 42)` → `"car: expected pair"`; `(cons)` → arity error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Cons onto list | `(cons 1 '(2 3))` | `'(1 2 3)` |
| Cons dotted | `(cons 1 2)` | `'(1 . 2)` |
| Car | `(car '(a b c))` | `'a` |
| Cdr | `(cdr '(a b c))` | `'(b c)` |
| Car of cons | `(car (cons 1 2))` | `1` |
| Cdr of cons | `(cdr (cons 1 2))` | `2` |
| List empty | `(list)` | `'()` |
| List many | `(list 1 2 3)` | `'(1 2 3)` |
| Null? empty | `(null? '())` | `#t` |
| Null? non-empty | `(null? '(1))` | `#f` |
| Null? non-list | `(null? 42)` | `#f` |
| Pair? pair | `(pair? '(1 2))` | `#t` |
| Pair? empty | `(pair? '())` | `#f` |
| Pair? atom | `(pair? 42)` | `#f` |
| Car of empty | `(car '())` | Error: car expects pair |
| Cdr of atom | `(cdr 42)` | Error: cdr expects pair |

#### Acceptance Criteria
1. **Given** `(car (cdr (list 1 2 3)))`
   **When** evaluated
   **Then** returns `2`

2. **Given** `(car 42)`
   **When** evaluated
   **Then** raises error containing `"car"` and `"pair"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **builtins**
- [ ] No regressions in existing test suite

---

### E1.14 — Evaluator: `and` / `or` as special forms; builtin `not`

#### Narrative
As a **language user**
I want **`and`, `or` with short-circuit evaluation and `not` as a builtin**
So that **I can write boolean logic efficiently, where `and`/`or` skip evaluation of unnecessary branches**

#### Requirements
##### Functional
- `(and e1 e2 ...)` — evaluate left-to-right; return first falsy value or the last value. `(and)` → `#t`
- `(or e1 e2 ...)` — evaluate left-to-right; return first truthy value or the last value. `(or)` → `#f`
- `and`/`or` are special forms (not builtins) because they must short-circuit
- `(not x)` — builtin: returns `#t` if `x` is `#f`, else `#f`

##### Non-functional
- Correctness: short-circuit is observable — `(and #f (error "boom"))` must NOT error
- Only `#f` is falsy

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| And all true | `(and 1 2 3)` | `3` |
| And short-circuit | `(and #f (error "boom"))` | `#f` |
| And empty | `(and)` | `#t` |
| And one false | `(and 1 #f 3)` | `#f` |
| Or first true | `(or 1 2)` | `1` |
| Or all false | `(or #f #f)` | `#f` |
| Or short-circuit | `(or 1 (error "boom"))` | `1` |
| Or empty | `(or)` | `#f` |
| Not true | `(not #t)` | `#f` |
| Not false | `(not #f)` | `#t` |
| Not truthy | `(not 42)` | `#f` |

#### Acceptance Criteria
1. **Given** `(and (> 3 2) (< 1 5))`
   **When** evaluated
   **Then** returns `#t`

2. **Given** `(or #f #f (+ 1 2))`
   **When** evaluated
   **Then** returns `3`

3. **Given** `(and #f (error "must not reach"))`
   **When** evaluated
   **Then** returns `#f` without error

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] `and`/`or` integrated as special forms in **evaluator**
- [ ] `not` integrated in **builtins**
- [ ] No regressions in existing test suite

---

### E1.15 — Builtins: Type predicates and I/O

#### Narrative
As a **language user**
I want **type-checking predicates and basic output procedures**
So that **I can inspect value types at runtime and print results**

#### Requirements
##### Functional
- `(number? x)` → `#t` iff `x` is a number
- `(string? x)` → `#t` iff `x` is a string
- `(symbol? x)` → `#t` iff `x` is a symbol
- `(boolean? x)` → `#t` iff `x` is a boolean
- `(procedure? x)` → `#t` iff `x` is a closure or builtin
- `(display x)` → prints `x` to stdout, returns `void`
- `(newline)` → prints a newline to stdout, returns `void`

##### Non-functional
- Correctness: every value answers exactly one of the type predicates positively (except `procedure?` which overlaps with no other)
- I/O: `display` prints without a trailing newline; strings print without quotes

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Number? yes | `(number? 42)` | `#t` |
| Number? no | `(number? "x")` | `#f` |
| String? yes | `(string? "hi")` | `#t` |
| String? no | `(string? 42)` | `#f` |
| Symbol? yes | `(symbol? 'foo)` | `#t` |
| Symbol? no | `(symbol? 42)` | `#f` |
| Boolean? yes | `(boolean? #t)` | `#t` |
| Boolean? no | `(boolean? 0)` | `#f` |
| Procedure? lambda | `(procedure? (lambda (x) x))` | `#t` |
| Procedure? builtin | `(procedure? +)` | `#t` |
| Procedure? no | `(procedure? 42)` | `#f` |
| Display string | `(display "hi")` | stdout: `hi`, returns `void` |
| Display number | `(display 42)` | stdout: `42`, returns `void` |
| Newline | `(newline)` | stdout: `\n`, returns `void` |

#### Acceptance Criteria
1. **Given** `(number? (+ 1 2))`
   **When** evaluated
   **Then** returns `#t`

2. **Given** `(procedure? car)`
   **When** evaluated
   **Then** returns `#t`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **builtins**
- [ ] No regressions in existing test suite

---

### E1.16 — REPL: Interactive read-eval-print loop

#### Narrative
As a **language user**
I want **an interactive REPL where I type expressions and see results**
So that **I can explore the language incrementally without writing files**

#### Requirements
##### Functional
- Prompt: `strawman> `
- Read input; if parens are unbalanced, continue reading on next line with `...      ` prompt
- Tokenize → Parse → Eval → Print the result
- `void` results are not printed
- Errors are caught and printed as `Error: <message>`, then the REPL continues
- `(exit)` or `(quit)` terminates the REPL
- EOF (Ctrl-D) terminates the REPL

##### Non-functional
- Correctness: environment persists across REPL inputs (define in one line, use in next)
- Diagnostics: errors include the error message but do not crash the REPL
- UX: multi-line input works for any well-formed expression

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple expr | `42` | prints `42` |
| Computation | `(+ 1 2)` | prints `3` |
| Define + use | `(define x 5)` then `x` | prints nothing, then `5` |
| Multi-line | `(+ 1\n   2)` | prints `3` |
| Error recovery | `(/ 1 0)` then `42` | prints `Error: division by zero`, then `42` |
| Unbound error | `foo` | prints `Error: unbound variable: foo` |
| Exit | `(exit)` | REPL terminates |
| Quit | `(quit)` | REPL terminates |
| EOF | Ctrl-D | REPL terminates |

#### Acceptance Criteria
1. **Given** the REPL is running
   **When** user types `(define x 10)` then `(+ x 5)`
   **Then** first input prints nothing, second prints `15`

2. **Given** the REPL is running
   **When** user types `(car 42)` (an error)
   **Then** REPL prints `Error: ...` and continues to accept input

3. **Given** the REPL is running
   **When** user types `(exit)`
   **Then** the REPL terminates cleanly

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **REPL**
- [ ] Entry point `strawman.rkt` wired up: no args → REPL, file arg → execute
- [ ] No regressions in existing test suite

---
---

## Epic 2 — Namespaces & Recursion
*Based on LiSP Chapter 2: Lisp, 1, 2, ..., ω*

Explore scoping strategies and support recursive definitions properly.

---

### E2.1 — Evaluator: `let` local bindings

#### Narrative
As a **language user**
I want **`(let ((x e1) (y e2)) body)` to create parallel local bindings**
So that **I can introduce temporary names without polluting the outer scope**

#### Requirements
##### Functional
- `(let ((v1 e1) (v2 e2) ...) body ...)` — evaluate all `ei` in the outer env, then bind `vi`→values in a new child env, evaluate body with implicit `begin`
- Bindings are parallel: `e2` cannot see `v1`
- The outer environment is not modified

##### Non-functional
- Correctness: parallel semantics — `(let ((x 1) (y x)) y)` is an error if `x` is unbound in outer env
- Diagnostics: malformed binding list → descriptive error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple | `(let ((x 1)) x)` | `1` |
| Two bindings | `(let ((x 1) (y 2)) (+ x y))` | `3` |
| Parallel | `(let ((x 1) (y x)) y)` with no outer `x` | Error: unbound variable |
| Shadowing | `(begin (define x 10) (let ((x 20)) x))` | `20` |
| Outer unchanged | `(begin (define x 10) (let ((x 20)) x) x)` | `10` |
| Body implicit begin | `(let ((x 1)) (define y 2) (+ x y))` | `3` |
| Nested let | `(let ((x 1)) (let ((y 2)) (+ x y)))` | `3` |
| Empty bindings | `(let () 42)` | `42` |
| Malformed | `(let (x 1) x)` | Error: malformed binding |

#### Acceptance Criteria
1. **Given** `(let ((a 1) (b 2)) (+ a b))`
   **When** evaluated
   **Then** returns `3`

2. **Given** `(begin (define x 5) (let ((x 99)) x) x)`
   **When** evaluated
   **Then** the `let` returns `99` and the final `x` returns `5`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator** (special form)
- [ ] No regressions in existing test suite

---

### E2.2 — Evaluator: `let*` sequential bindings

#### Narrative
As a **language user**
I want **`(let* ((x e1) (y e2)) body)` to create sequential local bindings where each sees the previous**
So that **I can build up values step-by-step in local scope**

#### Requirements
##### Functional
- `(let* ((v1 e1) (v2 e2) ...) body ...)` — evaluate `e1`, bind `v1`, then evaluate `e2` in env with `v1` visible, bind `v2`, etc.
- Body evaluated with all bindings visible

##### Non-functional
- Correctness: each binding sees all previous ones

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Sequential | `(let* ((x 1) (y (+ x 1))) y)` | `2` |
| Three deps | `(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)` | `3` |
| Shadow across | `(let* ((x 1) (x (+ x 1))) x)` | `2` |
| Empty | `(let* () 42)` | `42` |

#### Acceptance Criteria
1. **Given** `(let* ((x 10) (y (* x 2))) y)`
   **When** evaluated
   **Then** returns `20`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator** (special form)
- [ ] No regressions in existing test suite

---

### E2.3 — Evaluator: `letrec` recursive bindings

#### Narrative
As a **language user**
I want **`(letrec ((f (lambda ...))) body)` to create bindings that can refer to each other**
So that **I can define mutually recursive local functions**

#### Requirements
##### Functional
- `(letrec ((v1 e1) (v2 e2) ...) body ...)` — create a child env with all `vi` bound to `undefined`, evaluate each `ei` in that env, update bindings, then evaluate body
- Typically used with lambda values so that functions can call each other

##### Non-functional
- Correctness: `(letrec ((f (lambda () (g))) (g (lambda () 1))) (f))` must work
- Diagnostics: accessing an uninitialized letrec binding before its lambda is set → undefined behavior or error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Self-recursive | `(letrec ((f (lambda (n) (if (<= n 0) 1 (* n (f (- n 1))))))) (f 5))` | `120` |
| Mutual recursion | `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 4))` | `#t` |
| Mutual odd | (same as above) `(odd? 3)` in body | `#t` |
| Non-lambda | `(letrec ((x 42)) x)` | `42` |

#### Acceptance Criteria
1. **Given** the mutual even?/odd? letrec above
   **When** `(even? 10)` is evaluated
   **Then** returns `#t`

2. **Given** `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 10))`
   **When** evaluated
   **Then** returns `3628800`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator** (special form)
- [ ] No regressions in existing test suite

---

### E2.4 — Evaluator: `(define (f x) body)` shorthand

#### Narrative
As a **language user**
I want **`(define (f x y) body)` as syntactic sugar for `(define f (lambda (x y) body))`**
So that **function definitions are less verbose**

#### Requirements
##### Functional
- `(define (name params...) body ...)` desugars to `(define name (lambda (params...) body ...))`
- Works in both top-level and local scope

##### Non-functional
- Correctness: `(define (f x) x)` and `(define f (lambda (x) x))` are identical in behavior

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple | `(begin (define (f x) x) (f 42))` | `42` |
| Multi-param | `(begin (define (add a b) (+ a b)) (add 3 4))` | `7` |
| With body | `(begin (define (g x) (define y 1) (+ x y)) (g 5))` | `6` |
| Recursive | `(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5))` | `120` |
| No params | `(begin (define (f) 42) (f))` | `42` |

#### Acceptance Criteria
1. **Given** `(begin (define (square x) (* x x)) (square 9))`
   **When** evaluated
   **Then** returns `81`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator** (define handling)
- [ ] No regressions in existing test suite

---

### E2.5 — Integration: Recursion and scoping

#### Narrative
As a **language user**
I want **recursive and higher-order programs to work correctly with lexical scoping**
So that **I can trust the interpreter with real programs like factorial, fibonacci, map, and filter**

#### Requirements
##### Functional
- Top-level `define` allows self-reference (recursion)
- Closures capture their definition environment (not call site)
- Higher-order functions work: pass a lambda, get correct results

##### Non-functional
- Correctness: classic programs produce correct results

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Factorial 0 | `(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 0))` | `1` |
| Factorial 10 | `(fact 10)` | `3628800` |
| Fibonacci | `(begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))` | `55` |
| Map | `(begin (define (map f lst) (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst))))) (map (lambda (x) (* x x)) '(1 2 3 4)))` | `'(1 4 9 16)` |
| Filter | `(begin (define (filter p lst) (if (null? lst) '() (if (p (car lst)) (cons (car lst) (filter p (cdr lst))) (filter p (cdr lst))))) (filter (lambda (x) (> x 2)) '(1 2 3 4 5)))` | `'(3 4 5)` |
| Closure scope | `(begin (define x 10) (define (f) x) (define (g) (define x 20) (f)) (g))` | `10` (lexical) |
| Accumulator | `(begin (define (make-acc init) (define n init) (lambda (x) (set! n (+ n x)) n)) (define a (make-acc 0)) (a 5) (a 3) (a 2))` | `5`, `8`, `10` |

#### Acceptance Criteria
1. **Given** the map definition above
   **When** `(map (lambda (x) (+ x 1)) '(10 20 30))`
   **Then** returns `'(11 21 31)`

2. **Given** the closure scope test
   **When** `(g)` is called
   **Then** returns `10` (lexical scope), not `20` (dynamic scope)

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] All tests pass with the full interpreter pipeline
- [ ] No regressions in existing test suite

---
---

## Epic 3 — Escape & Continuations
*Based on LiSP Chapter 3: Escape and Return*

Add control flow operators built on continuations.

---

### E3.1 — CPS evaluator rewrite

#### Narrative
As a **compiler developer**
I want **the evaluator rewritten in continuation-passing style**
So that **control operators like `call/cc`, `catch`/`throw`, and `block`/`return-from` can be implemented on top of it**

#### Requirements
##### Functional
- `straw-eval` signature changes to `straw-eval : s-expr × env × continuation -> value`
- A continuation is a one-argument Racket procedure `(lambda (value) ...)`
- All existing special forms and builtins work identically under CPS
- The REPL passes the identity continuation as the top-level continuation

##### Non-functional
- Correctness: all existing tests must still pass unchanged
- Performance: CPS adds overhead; acceptable for an educational interpreter

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| All Epic 1 tests | (existing suite) | Same results |
| All Epic 2 tests | (existing suite) | Same results |
| Identity continuation | `(+ 1 2)` with identity k | `3` |

#### Acceptance Criteria
1. **Given** the full existing test suite
   **When** run against the CPS evaluator
   **Then** all tests pass with identical results

#### Definition of Done
- [ ] Evaluator rewritten in CPS
- [ ] All existing tests pass (zero regressions)
- [ ] CPS plumbing is invisible to the language user

---

### E3.2 — `call/cc`: First-class continuations

#### Narrative
As a **language user**
I want **`(call/cc (lambda (k) body))` to capture the current continuation as a callable value**
So that **I can implement non-local exits, coroutines, and other advanced control flow**

#### Requirements
##### Functional
- `(call/cc proc)` calls `proc` with one argument: the current continuation `k`
- `(k value)` aborts the current computation and returns `value` to the point where `call/cc` was invoked
- If `proc` returns normally (without invoking `k`), that value is the result of `call/cc`
- `k` can be called multiple times (re-entrant)

##### Non-functional
- Correctness: captured continuation includes the full evaluation context
- Diagnostics: `(call/cc 42)` → `"call/cc: expected procedure"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Normal return | `(call/cc (lambda (k) 42))` | `42` |
| Early exit | `(call/cc (lambda (k) (k 10) 20))` | `10` |
| In expression | `(+ 1 (call/cc (lambda (k) (k 5))))` | `6` |
| Saved continuation | `(begin (define saved #f) (+ 1 (call/cc (lambda (k) (set! saved k) 10))) )` | `11`, and `(saved 20)` → `21` |
| Non-procedure | `(call/cc 42)` | Error: expected procedure |

#### Acceptance Criteria
1. **Given** `(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))`
   **When** evaluated
   **Then** returns `4` (the `(+ 2 ...)` is abandoned)

2. **Given** a saved continuation is called later
   **When** `(saved 100)` is invoked
   **Then** the computation resumes as if `call/cc` returned `100`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator** (special form)
- [ ] No regressions in existing test suite

---

### E3.3 — `catch` / `throw`: Tagged non-local exit

#### Narrative
As a **language user**
I want **`(catch tag body)` and `(throw tag value)` for tagged non-local exits**
So that **I can implement exception-like control flow with named exit points**

#### Requirements
##### Functional
- `(catch tag body)` — evaluate `body`; if `(throw tag value)` is invoked during `body`, `catch` returns `value`
- `(throw tag value)` — unwind to the nearest `catch` with matching `tag`
- Tags are compared with `equal?`
- If no matching `catch` is found, raise an error

##### Non-functional
- Diagnostics: `(throw 'foo 1)` with no matching catch → `"no matching catch for tag: foo"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| No throw | `(catch 'x 42)` | `42` |
| Simple throw | `(catch 'x (begin (throw 'x 10) 20))` | `10` |
| Nested catch | `(catch 'a (catch 'b (throw 'a 1)))` | `1` |
| Wrong tag | `(catch 'a (throw 'b 1))` | Error: no matching catch |
| Throw in function | `(catch 'x ((lambda () (throw 'x 99))))` | `99` |

#### Acceptance Criteria
1. **Given** `(catch 'err (begin (throw 'err "oops") "unreachable"))`
   **When** evaluated
   **Then** returns `"oops"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E3.4 — `block` / `return-from`: Structured non-local exit

#### Narrative
As a **language user**
I want **`(block name body)` and `(return-from name value)` for lexically scoped exits**
So that **I can exit from a named block without using tags**

#### Requirements
##### Functional
- `(block name body ...)` — evaluate body; if `(return-from name value)` is called, return `value`
- `name` is lexically scoped — not dynamically looked up
- Unlike `catch`/`throw`, `block`/`return-from` is resolved at compile time (or at least lexically)

##### Non-functional
- Diagnostics: `(return-from nonexistent 1)` → error about unknown block name

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| No return | `(block b 42)` | `42` |
| Early return | `(block b (return-from b 10) 20)` | `10` |
| Nested blocks | `(block a (block b (return-from a 1)))` | `1` |
| Return from inner | `(block a (block b (return-from b 2) 3))` | `3` |
| Unknown block | `(return-from z 1)` | Error |

#### Acceptance Criteria
1. **Given** `(block done (return-from done 42) (error "unreachable"))`
   **When** evaluated
   **Then** returns `42`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---

### E3.5 — `unwind-protect`: Guaranteed cleanup

#### Narrative
As a **language user**
I want **`(unwind-protect body cleanup ...)` to guarantee that cleanup runs even on non-local exit**
So that **I can release resources reliably**

#### Requirements
##### Functional
- `(unwind-protect body cleanup ...)` — evaluate `body`, then evaluate `cleanup` expressions regardless of how `body` exits (normally, via `throw`, via `return-from`, via continuation)
- Normal exit: returns the value of `body`
- Non-local exit: `cleanup` runs, then the non-local exit continues

##### Non-functional
- Correctness: cleanup must run even when `throw` or continuation crosses the boundary

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Normal | `(unwind-protect 42 (display "clean"))` | `42`, stdout: `clean` |
| With throw | `(catch 'x (unwind-protect (throw 'x 1) (display "clean")))` | `1`, stdout: `clean` |
| Cleanup order | `(unwind-protect 10 (display "a") (display "b"))` | `10`, stdout: `ab` |

#### Acceptance Criteria
1. **Given** `(catch 'e (unwind-protect (throw 'e "err") (display "cleaned")))`
   **When** evaluated
   **Then** stdout contains `"cleaned"` and result is `"err"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in pipeline stage: **evaluator**
- [ ] No regressions in existing test suite

---
---

## Epic 4 — Assignment & Side Effects
*Based on LiSP Chapter 4: Assignment and Side Effects*

Formalize the distinction between variables and values, and add data mutation.

---

### E4.1 — Box model for variables

#### Narrative
As a **compiler developer**
I want **variables implemented as mutable boxes (indirection cells)**
So that **`set!` has precise semantics: it mutates the box, and all references to that variable see the new value**

#### Requirements
##### Functional
- Each variable binding stores a box (mutable cell) containing the value
- `env-lookup` returns the box's contents
- `env-update!` mutates the box
- Closures that share a binding share the same box

##### Non-functional
- Correctness: two closures that close over the same variable see each other's mutations

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Shared mutation | `(begin (define x 0) (define inc (lambda () (set! x (+ x 1)))) (define get (lambda () x)) (inc) (inc) (get))` | `2` |
| Lambda captures box | `(begin (define (make) (define n 0) (list (lambda () (set! n (+ n 1)) n) (lambda () n))) (define pair (make)) ((car pair)) ((car pair)) ((car (cdr pair))))` | `1`, `2`, `2` |

#### Acceptance Criteria
1. **Given** two closures sharing a mutable variable
   **When** one mutates it
   **Then** the other sees the mutation

#### Definition of Done
- [ ] Environment internals refactored to use boxes
- [ ] All existing tests still pass
- [ ] New tests for shared mutation added

---

### E4.2 — `set-car!` / `set-cdr!`: Pair mutation

#### Narrative
As a **language user**
I want **`(set-car! pair val)` and `(set-cdr! pair val)` to mutate pairs in place**
So that **I can build mutable data structures like queues and circular lists**

#### Requirements
##### Functional
- `(set-car! p v)` — mutate the car of pair `p` to `v`, return `void`
- `(set-cdr! p v)` — mutate the cdr of pair `p` to `v`, return `void`
- Requires pairs to be mutable (Racket `mcons` or custom struct)

##### Non-functional
- Diagnostics: `(set-car! 42 1)` → `"set-car!: expected mutable pair"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Set car | `(begin (define p (cons 1 2)) (set-car! p 10) (car p))` | `10` |
| Set cdr | `(begin (define p (cons 1 2)) (set-cdr! p 20) (cdr p))` | `20` |
| Non-pair | `(set-car! 42 1)` | Error: expected mutable pair |

#### Acceptance Criteria
1. **Given** `(begin (define p (cons 1 2)) (set-car! p 99) p)`
   **When** evaluated
   **Then** returns `(99 . 2)`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Pairs implemented as mutable structures
- [ ] No regressions in existing test suite

---

### E4.3 — `eq?` vs `equal?`: Identity vs structural equality

#### Narrative
As a **language user**
I want **`eq?` to test object identity and `equal?` to test structural equality**
So that **I can distinguish whether two values are the same object or merely look alike**

#### Requirements
##### Functional
- `(eq? a b)` → `#t` iff `a` and `b` are the exact same object in memory
- `(equal? a b)` → `#t` iff `a` and `b` are structurally identical (recursive for lists)
- For numbers and booleans, `eq?` and `equal?` agree
- For cons cells, `eq?` is identity; `equal?` compares element-wise

##### Non-functional
- Correctness: `(eq? (list 1) (list 1))` may be `#f`; `(equal? (list 1) (list 1))` is `#t`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| eq? same symbol | `(eq? 'a 'a)` | `#t` |
| eq? same number | `(eq? 42 42)` | `#t` |
| eq? different lists | `(eq? (list 1) (list 1))` | `#f` |
| eq? same binding | `(begin (define x (list 1)) (eq? x x))` | `#t` |
| equal? lists | `(equal? (list 1 2) (list 1 2))` | `#t` |
| equal? nested | `(equal? '(1 (2 3)) '(1 (2 3)))` | `#t` |
| equal? different | `(equal? '(1 2) '(1 3))` | `#f` |

#### Acceptance Criteria
1. **Given** `(begin (define a (list 1 2)) (define b (list 1 2)) (list (eq? a b) (equal? a b)))`
   **When** evaluated
   **Then** returns `(#f #t)`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in **builtins**
- [ ] No regressions in existing test suite

---

### E4.4 — Vectors: Mutable indexed arrays

#### Narrative
As a **language user**
I want **`make-vector`, `vector-ref`, `vector-set!`, `vector-length`**
So that **I have O(1) indexed mutable storage**

#### Requirements
##### Functional
- `(make-vector n [fill])` → create vector of size `n`, optional fill value (default `0`)
- `(vector-ref v i)` → element at index `i`
- `(vector-set! v i val)` → set element at index `i`, return `void`
- `(vector-length v)` → number of elements
- `(vector? x)` → type predicate

##### Non-functional
- Diagnostics: out-of-bounds index → `"vector-ref: index out of range"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Make and ref | `(begin (define v (make-vector 3 0)) (vector-ref v 0))` | `0` |
| Set and ref | `(begin (define v (make-vector 3 0)) (vector-set! v 1 42) (vector-ref v 1))` | `42` |
| Length | `(vector-length (make-vector 5))` | `5` |
| Type pred | `(vector? (make-vector 1))` | `#t` |
| Not vector | `(vector? '(1 2))` | `#f` |
| Out of bounds | `(vector-ref (make-vector 3) 5)` | Error: index out of range |

#### Acceptance Criteria
1. **Given** `(begin (define v (make-vector 3 0)) (vector-set! v 2 99) (vector-ref v 2))`
   **When** evaluated
   **Then** returns `99`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in **builtins**
- [ ] No regressions in existing test suite

---
---

## Epic 5 — Denotational Semantics
*Based on LiSP Chapter 5: Denotational Semantics*

Express the interpreter as a mathematical denotation.

---

### E5.1 — Formal semantics document

#### Narrative
As a **compiler developer**
I want **a written denotational semantics for Strawman's core forms**
So that **the language meaning is unambiguous and can be used to verify correctness**

#### Requirements
##### Functional
- Define semantic domains: Env, Value, Cont, Mem
- Define valuation functions for: numbers, symbols, `quote`, `if`, `begin`, `define`, `set!`, `lambda`, application
- Express in standard notation (or a readable ASCII approximation)

##### Non-functional
- Correctness: the denotational semantics must match the interpreter's observable behavior on all test cases

#### Acceptance Criteria
1. **Given** the semantics document
   **When** compared with interpreter behavior on the full test suite
   **Then** all results match

#### Definition of Done
- [ ] `docs/semantics.md` written covering all core forms
- [ ] Reviewed against interpreter behavior

---

### E5.2 — Semantics-driven test cases

#### Narrative
As a **compiler developer**
I want **test cases derived directly from the denotational semantics**
So that **the interpreter is verified against the formal specification, not just ad-hoc examples**

#### Requirements
##### Functional
- For each valuation function clause, at least one test case
- Edge cases derived from domain boundaries (empty env, empty list, etc.)

#### Acceptance Criteria
1. **Given** the semantics document with N valuation clauses
   **When** the test suite is inspected
   **Then** at least N tests exist with direct traceability to semantics clauses

#### Definition of Done
- [ ] Tests created in `tests/test-semantics.rkt`
- [ ] Each test annotated with the semantic clause it validates
- [ ] All tests pass

---
---

## Epic 6 — Fast Interpretation
*Based on LiSP Chapter 6: Fast Interpretation*

Pre-process expressions to eliminate repeated work during evaluation.

---

### E6.1 — Pretreatment pass

#### Narrative
As a **compiler developer**
I want **a pretreatment pass that analyzes expressions before evaluation**
So that **variable resolution and other repeated work happens once, not on every evaluation**

#### Requirements
##### Functional
- `pretreat : s-expr × static-env -> treated-expr`
- The treated expression encodes resolved variable references
- The evaluator operates on treated expressions instead of raw S-expressions

##### Non-functional
- Correctness: `pretreat` + fast-eval must produce identical results to naive eval on all programs
- Performance: measurably faster on loop-heavy programs

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| All existing tests | (full suite) | Identical results under pretreat + fast-eval |

#### Acceptance Criteria
1. **Given** the full test suite
   **When** run with pretreatment enabled
   **Then** all tests pass with identical output

#### Definition of Done
- [ ] `src/pretreat.rkt` implemented
- [ ] All existing tests pass
- [ ] Fast evaluator added in `src/fast-eval.rkt`

---

### E6.2 — Lexical addressing

#### Narrative
As a **compiler developer**
I want **variable references replaced with (depth, offset) pairs during pretreatment**
So that **runtime lookup is O(1) array access instead of hash lookup with chain walking**

#### Requirements
##### Functional
- During pretreatment, each variable reference becomes `(lexical-ref depth offset)`
- The runtime environment is a vector of vectors (ribs) instead of hash tables
- `depth` = number of enclosing scopes to traverse; `offset` = index within that scope

##### Non-functional
- Performance: lookup becomes O(depth) vector indexing

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Local var | `(lambda (x) x)` | ref at depth 0, offset 0 |
| Free var one level | `(lambda (x) (lambda (y) x))` | inner `x` at depth 1, offset 0 |
| Multiple params | `(lambda (a b c) b)` | `b` at depth 0, offset 1 |

#### Acceptance Criteria
1. **Given** `(lambda (x) (lambda (y) (+ x y)))`
   **When** pretreated
   **Then** `x` in inner body has depth=1, offset=0 and `y` has depth=0, offset=0

#### Definition of Done
- [ ] Lexical addressing implemented in pretreatment
- [ ] Runtime environment uses vector-of-vectors
- [ ] All tests pass

---

### E6.3 — Benchmark harness

#### Narrative
As a **compiler developer**
I want **a benchmark harness to compare naive eval vs fast eval**
So that **I can measure the actual speedup from pretreatment and lexical addressing**

#### Requirements
##### Functional
- Benchmark programs: factorial(20), fibonacci(30), ackermann(3,7), map over large list
- Measure wall-clock time for each under both evaluators
- Report results in a table

##### Non-functional
- Reproducibility: benchmarks produce consistent results across runs

#### Acceptance Criteria
1. **Given** `racket bench/run.rkt`
   **When** executed
   **Then** prints a comparison table showing times for naive vs fast eval

#### Definition of Done
- [ ] `bench/` directory with benchmark programs
- [ ] `bench/run.rkt` harness implemented
- [ ] Results documented

---
---

## Epic 7 — Bytecode Compilation
*Based on LiSP Chapter 7: Compilation*

Compile Strawman programs to bytecode and execute on a virtual machine.

---

### E7.1 — Instruction set design

#### Narrative
As a **compiler developer**
I want **a defined bytecode instruction set for Strawman**
So that **the compiler has a clear target and the VM has a clear contract**

#### Requirements
##### Functional
- Instructions: `CONST`, `LOOKUP`, `SET`, `DEFINE`, `JUMP`, `JUMP-IF-FALSE`, `CALL`, `RETURN`, `CLOSURE`, `POP`, `HALT`
- Each instruction has a fixed format: opcode + operands
- Document the instruction set with semantics for each opcode

##### Non-functional
- Completeness: the instruction set must be sufficient to compile all core forms

#### Acceptance Criteria
1. **Given** the instruction set document
   **When** every core form is analyzed
   **Then** each can be expressed as a sequence of instructions

#### Definition of Done
- [ ] `docs/bytecode.md` with instruction set specification
- [ ] Each instruction has defined semantics

---

### E7.2 — Compiler: S-expression to bytecode

#### Narrative
As a **compiler developer**
I want **a compiler that translates Strawman S-expressions into bytecode sequences**
So that **programs can be executed by the VM instead of the tree-walking interpreter**

#### Requirements
##### Functional
- `compile : s-expr -> (listof instruction)`
- Compile all core forms: self-eval, symbols, `quote`, `if`, `begin`, `define`, `set!`, `lambda`, application
- Lambda compilation produces a `CLOSURE` instruction referencing the compiled body

##### Non-functional
- Correctness: compiled program produces same result as interpreted program

#### Test Matrix
| Case | Input | Expected bytecode (sketch) |
|---|---|---|
| Number | `42` | `CONST 42` |
| Symbol | `x` | `LOOKUP x` |
| If | `(if #t 1 2)` | `CONST #t, JUMP-IF-FALSE L1, CONST 1, JUMP L2, L1: CONST 2, L2:` |
| Application | `(+ 1 2)` | `LOOKUP +, CONST 1, CONST 2, CALL 2` |

#### Acceptance Criteria
1. **Given** `(+ 1 2)` compiled and executed on the VM
   **When** the VM runs
   **Then** it produces `3`

#### Definition of Done
- [ ] `src/compiler.rkt` implemented
- [ ] Tests for compilation of each core form
- [ ] Compiled + VM output matches interpreter output on full test suite

---

### E7.3 — Virtual machine

#### Narrative
As a **compiler developer**
I want **a virtual machine that executes bytecode with a program counter, stack, and environments**
So that **compiled Strawman programs can run**

#### Requirements
##### Functional
- VM state: program counter (PC), value stack, environment register, code vector
- Execute instructions in a fetch-decode-execute loop
- `CALL` pushes a frame; `RETURN` pops and restores
- `CLOSURE` captures the current environment

##### Non-functional
- Correctness: VM execution on compiled code matches tree-walking interpreter
- Diagnostics: stack underflow, invalid opcode → clear errors

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| All existing tests | Compile + VM execute | Identical results to interpreter |

#### Acceptance Criteria
1. **Given** the full test suite compiled to bytecode
   **When** executed on the VM
   **Then** all produce identical results to the tree-walking interpreter

#### Definition of Done
- [ ] `src/vm.rkt` implemented
- [ ] All tests pass via compile + VM path
- [ ] Error handling for VM faults

---

### E7.4 — REPL integration with bytecode

#### Narrative
As a **language user**
I want **the REPL to transparently compile and execute via the bytecode VM**
So that **I get the performance benefit without changing how I interact with the REPL**

#### Requirements
##### Functional
- REPL pipeline changes from tokenize→parse→eval to tokenize→parse→compile→VM-execute
- All REPL behavior (error handling, multi-line, exit) unchanged
- Optional flag to switch between interpreted and compiled modes

##### Non-functional
- UX: user sees no difference except potentially faster execution

#### Acceptance Criteria
1. **Given** `racket strawman.rkt --compiled`
   **When** user interacts with REPL
   **Then** behavior is identical to interpreted mode

#### Definition of Done
- [ ] REPL supports `--compiled` flag
- [ ] All REPL tests pass in both modes
- [ ] No regressions

---
---

## Epic 8 — Eval & Reflection
*Based on LiSP Chapter 8: Evaluation and Reflection*

Add `eval` and first-class environments.

---

### E8.1 — `eval`: Runtime evaluation

#### Narrative
As a **language user**
I want **`(eval expr)` to evaluate a dynamically constructed expression at runtime**
So that **I can build and execute code on the fly**

#### Requirements
##### Functional
- `(eval expr)` — evaluate `expr` to get a datum, then evaluate that datum as code in the current environment
- `(eval expr env)` — optional second argument: evaluate in a specific environment
- Works with all forms: `(eval '(+ 1 2))` → `3`

##### Non-functional
- Correctness: `(eval (list '+ 1 2))` must produce `3`
- Security: `eval` has full access to the environment it runs in

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple | `(eval '(+ 1 2))` | `3` |
| Constructed | `(eval (list '* 3 4))` | `12` |
| With define | `(begin (eval '(define x 42)) x)` | `42` |
| Nested | `(eval '(eval '(+ 1 1)))` | `2` |
| Non-list | `(eval 42)` | `42` |

#### Acceptance Criteria
1. **Given** `(eval (cons '+ (list 1 2)))`
   **When** evaluated
   **Then** returns `3`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Feature integrated in **builtins** or **evaluator**
- [ ] No regressions in existing test suite

---

### E8.2 — First-class environments

#### Narrative
As a **language user**
I want **environments to be first-class values that can be captured, passed, and used with `eval`**
So that **I can implement sandboxes, REPLs-within-REPLs, and reflective programs**

#### Requirements
##### Functional
- `(the-environment)` returns the current environment as a value
- Environments can be passed to `eval` as the second argument
- `(environment? x)` type predicate

##### Non-functional
- Correctness: `(eval 'x env)` looks up `x` in `env`, not the caller's env

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Capture | `(begin (define x 42) (define e (the-environment)) (eval 'x e))` | `42` |
| Isolation | `(begin (define e (the-environment)) (define x 99) (eval 'x e))` | depends on timing — `99` since same env |
| Type pred | `(environment? (the-environment))` | `#t` |
| Not env | `(environment? 42)` | `#f` |

#### Acceptance Criteria
1. **Given** `(begin (define x 10) (define e (the-environment)) (eval '(+ x 1) e))`
   **When** evaluated
   **Then** returns `11`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] `the-environment` and `environment?` integrated
- [ ] `eval` accepts optional env argument
- [ ] No regressions

---
---

## Epic 9 — Macros
*Based on LiSP Chapter 9: Macros*

Add a macro system for syntactic extension.

---

### E9.1 — `define-macro`: User-defined macros

#### Narrative
As a **language user**
I want **`(define-macro (name args...) body)` to define syntactic transformations**
So that **I can extend the language syntax without modifying the evaluator**

#### Requirements
##### Functional
- `(define-macro (name params...) body)` registers a macro
- When `(name ...)` appears in code, the macro function is called with the unevaluated arguments
- The returned form is evaluated in place of the original
- Macros are expanded before evaluation (expansion time ≠ evaluation time)

##### Non-functional
- Correctness: macro body receives syntax (unevaluated S-expressions), not values
- Diagnostics: redefining a special form as a macro → warning or error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple macro | `(begin (define-macro (my-if c t f) (list 'if c t f)) (my-if #t 1 2))` | `1` |
| Swap macro | `(begin (define-macro (swap! a b) (list 'let (list (list 'tmp a)) (list 'set! a b) (list 'set! b 'tmp))) ...)` | swaps values |
| Expansion | `(macroexpand '(my-if #t 1 2))` | `'(if #t 1 2)` |

#### Acceptance Criteria
1. **Given** a user-defined `my-when` macro
   **When** `(my-when #t (display "yes"))` is evaluated
   **Then** stdout contains `"yes"`

#### Definition of Done
- [ ] Tests added for all rows in Test Matrix
- [ ] Macro expansion pass integrated before evaluation
- [ ] No regressions in existing test suite

---

### E9.2 — `macroexpand`: Inspect macro expansion

#### Narrative
As a **compiler developer**
I want **`(macroexpand expr)` to show the expanded form of a macro call**
So that **I can debug macros by seeing what code they generate**

#### Requirements
##### Functional
- `(macroexpand '(macro-name args...))` → the expanded S-expression without evaluating it
- Expands only one level (not recursive expansion)
- `(macroexpand-all expr)` — optional: fully recursive expansion

##### Non-functional
- Correctness: the expanded form, when evaluated, produces the same result as evaluating the macro call directly

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Expand once | `(macroexpand '(my-if #t 1 2))` | `'(if #t 1 2)` |
| Non-macro | `(macroexpand '(+ 1 2))` | `'(+ 1 2)` (unchanged) |

#### Acceptance Criteria
1. **Given** `(macroexpand '(my-if cond a b))`
   **When** evaluated
   **Then** returns the template with `cond`, `a`, `b` substituted

#### Definition of Done
- [ ] `macroexpand` implemented as builtin
- [ ] Tests pass
- [ ] No regressions

---

### E9.3 — Quasiquote: Template syntax

#### Narrative
As a **language user**
I want **quasiquote (`` ` ``), unquote (`,`), and unquote-splicing (`,@`)**
So that **I can write macro templates concisely instead of building lists manually**

#### Requirements
##### Functional
- `` `(a ,b c) `` where `b`→`2` evaluates to `(a 2 c)`
- `` `(a ,@lst c) `` where `lst`→`(1 2)` evaluates to `(a 1 2 c)`
- Quasiquote can nest

##### Non-functional
- Correctness: quasiquote is syntactic sugar; the expansion must be equivalent to explicit `list`/`cons`/`append` calls

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple unquote | `` (let ((x 1)) `(a ,x c)) `` | `'(a 1 c)` |
| Splicing | `` (let ((xs '(1 2))) `(a ,@xs c)) `` | `'(a 1 2 c)` |
| Nested quasi | `` `(a `(b ,(+ 1 2))) `` | `'(a (quasiquote (b (unquote (+ 1 2)))))` |
| No unquote | `` `(a b c) `` | `'(a b c)` |

#### Acceptance Criteria
1. **Given** `` `(+ ,(+ 1 2) 4) ``
   **When** evaluated
   **Then** returns `'(+ 3 4)`

#### Definition of Done
- [ ] Quasiquote handling in parser or expander
- [ ] Tests pass
- [ ] No regressions

---

### E9.4 — Standard macros: `cond`, `when`, `unless`

#### Narrative
As a **language user**
I want **`cond`, `when`, `unless` implemented as macros over core forms**
So that **I have convenient syntax without growing the evaluator**

#### Requirements
##### Functional
- `(cond (test1 expr1) (test2 expr2) ... (else exprN))` → nested `if`
- `(when test body ...)` → `(if test (begin body ...) void)`
- `(unless test body ...)` → `(if test void (begin body ...))`

##### Non-functional
- Correctness: `cond` with `else` always matches; `cond` with no match returns `void`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Cond first | `(cond (#t 1) (#t 2))` | `1` |
| Cond second | `(cond (#f 1) (#t 2))` | `2` |
| Cond else | `(cond (#f 1) (else 3))` | `3` |
| Cond none | `(cond (#f 1) (#f 2))` | `void` |
| When true | `(when #t 42)` | `42` |
| When false | `(when #f 42)` | `void` |
| Unless true | `(unless #t 42)` | `void` |
| Unless false | `(unless #f 42)` | `42` |

#### Acceptance Criteria
1. **Given** `(cond ((> 3 2) "yes") (else "no"))`
   **When** evaluated
   **Then** returns `"yes"`

2. **Given** `(macroexpand '(when #t 42))`
   **When** evaluated
   **Then** returns an `if`-based expression

#### Definition of Done
- [ ] `cond`, `when`, `unless` defined as macros
- [ ] Tests pass for all rows
- [ ] No regressions

---
---

## Epic 10 — Object System
*Based on LiSP Chapter 11: An Object System*

Add a minimal object system inspired by CLOS / Meroonet.

---

### E10.1 — `define-class`: Class definitions

#### Narrative
As a **language user**
I want **`(define-class name (parent) (field1 field2 ...))` to define a class**
So that **I can model structured data with named types**

#### Requirements
##### Functional
- `(define-class Point () (x y))` defines a class `Point` with fields `x`, `y`
- `(define-class Point3D (Point) (z))` defines a subclass inheriting `x`, `y`, adding `z`
- Class objects are first-class values
- `()` parent means root class

##### Non-functional
- Diagnostics: duplicate field names → error; unknown parent class → error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Simple class | `(define-class Point () (x y))` | Class `Point` defined |
| Subclass | `(define-class Point3D (Point) (z))` | Class with fields `x y z` |
| Duplicate field | `(define-class Bad () (x x))` | Error: duplicate field |
| Unknown parent | `(define-class C (Unknown) (a))` | Error: unknown parent class |

#### Acceptance Criteria
1. **Given** `(define-class Point () (x y))`
   **When** evaluated
   **Then** `Point` is bound to a class object

#### Definition of Done
- [ ] `define-class` implemented as special form
- [ ] Tests pass
- [ ] No regressions

---

### E10.2 — Instantiation and field access

#### Narrative
As a **language user**
I want **`(make-Point x y)` to create instances and `(Point-x obj)` to access fields**
So that **I can create and inspect structured objects**

#### Requirements
##### Functional
- `define-class` auto-generates: constructor `make-<Class>`, accessors `<Class>-<field>`, mutators `set-<Class>-<field>!`, predicate `<Class>?`
- `(make-Point 1 2)` → a Point instance with x=1, y=2
- `(Point-x p)` → value of x field
- `(set-Point-x! p 10)` → mutate x field
- `(Point? p)` → `#t`

##### Non-functional
- Diagnostics: wrong number of constructor args → arity error; accessor on wrong type → type error

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Construct | `(begin (define-class Point () (x y)) (define p (make-Point 1 2)) (Point-x p))` | `1` |
| Access y | `(Point-y p)` | `2` |
| Mutate | `(begin (set-Point-x! p 99) (Point-x p))` | `99` |
| Predicate yes | `(Point? p)` | `#t` |
| Predicate no | `(Point? 42)` | `#f` |
| Wrong arity | `(make-Point 1)` | Error: arity mismatch |
| Wrong type | `(Point-x 42)` | Error: expected Point |

#### Acceptance Criteria
1. **Given** `(begin (define-class Point () (x y)) (define p (make-Point 3 4)) (+ (Point-x p) (Point-y p)))`
   **When** evaluated
   **Then** returns `7`

#### Definition of Done
- [ ] Constructor, accessors, mutators, predicate auto-generated
- [ ] Tests pass
- [ ] No regressions

---

### E10.3 — Generic functions and method dispatch

#### Narrative
As a **language user**
I want **generic functions that dispatch on the class of their first argument**
So that **different classes can respond to the same operation differently (polymorphism)**

#### Requirements
##### Functional
- `(define-generic name)` creates a generic function
- `(define-method (name (self ClassName) args...) body)` adds a method
- Dispatch on the class of the first argument
- If no method matches, error

##### Non-functional
- Correctness: most-specific method wins (subclass before parent)
- Diagnostics: no applicable method → `"no method for <generic> on <class>"`

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Single method | `(begin (define-generic area) (define-class Rect () (w h)) (define-method (area (self Rect)) (* (Rect-w self) (Rect-h self))) (area (make-Rect 3 4)))` | `12` |
| Inherited method | `(begin (define-class Square (Rect) ()) (area (make-Square 5 5)))` | `25` |
| No method | `(area 42)` | Error: no method |
| Override | define `area` for `Square` separately | Uses Square method |

#### Acceptance Criteria
1. **Given** a `describe` generic with methods on `Point` and `Point3D`
   **When** called with a `Point3D` instance
   **Then** the `Point3D` method runs (not the `Point` one)

#### Definition of Done
- [ ] `define-generic` and `define-method` implemented
- [ ] Dispatch selects most-specific method
- [ ] Tests pass
- [ ] No regressions

---

### E10.4 — Inheritance: Field and method inheritance

#### Narrative
As a **language user**
I want **subclasses to inherit fields from parents and method dispatch to walk the class hierarchy**
So that **I can build class hierarchies with shared behavior**

#### Requirements
##### Functional
- Subclass instances have all parent fields plus their own
- Constructor requires values for all fields (parent + own)
- Method dispatch walks from most-specific class to parent until a method is found
- `(is-a? obj class)` → `#t` if `obj` is an instance of `class` or any subclass

##### Non-functional
- Correctness: field order is parent-first, then own

#### Test Matrix
| Case | Input | Expected |
|---|---|---|
| Inherited field | `(begin (define-class A () (x)) (define-class B (A) (y)) (A-x (make-B 1 2)))` | `1` |
| Own field | `(B-y (make-B 1 2))` | `2` |
| is-a? direct | `(is-a? (make-B 1 2) B)` | `#t` |
| is-a? parent | `(is-a? (make-B 1 2) A)` | `#t` |
| is-a? unrelated | `(is-a? (make-B 1 2) Point)` | `#f` |

#### Acceptance Criteria
1. **Given** a three-level hierarchy A→B→C
   **When** a C instance is created
   **Then** it has all fields from A, B, and C, and `(is-a? c A)` is `#t`

#### Definition of Done
- [ ] Inheritance of fields works
- [ ] Method dispatch walks hierarchy
- [ ] `is-a?` implemented
- [ ] Tests pass
- [ ] No regressions

---
---

## Priority & Ordering

| Priority | Epics | Milestone |
|----------|-------|-----------|
| **P0 — MVP** | Epic 1, Epic 2 | A working Lisp with closures, recursion, `let` forms |
| **P1 — Depth** | Epic 3, Epic 4 | Continuations, proper mutation semantics |
| **P2 — Performance** | Epic 5, Epic 6 | Formal semantics, fast interpretation |
| **P3 — Compilation** | Epic 7 | Bytecode VM |
| **P4 — Power** | Epic 8, Epic 9 | Reflection, macros |
| **P5 — OOP** | Epic 10 | Object system |

Each priority level assumes the previous ones are complete.
