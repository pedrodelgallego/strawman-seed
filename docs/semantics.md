# Denotational Semantics of Strawman Lisp

A formal specification of Strawman's core forms, following the style of
*Lisp in Small Pieces* (Queinnec, Chapter 5). The semantics uses a
continuation-passing, store-passing style to model both control flow and
mutation.

---

## 1. Semantic Domains

```
Value   = Number | String | Boolean | Symbol | Pair | Null | Closure | Void | Vector
Env     = Symbol -> Location
Mem     = Location -> Value          (the mutable store / memory)
Cont    = Value x Mem -> Answer      (continuation: receives a value and store)
Answer  = Value                      (final program result)
Location = Natural                   (abstract address in the store)
```

**Value** is the union of all runtime values the interpreter can produce.

**Env** (environment) maps variable names to *locations* (not directly to
values). This indirection through Mem is what makes mutation (`set!`) work:
multiple closures sharing a binding share the same location.

**Mem** (memory / store) maps locations to values. `define` allocates a new
location; `set!` updates the value at an existing location.

**Cont** (continuation) represents "what to do next" with a value. Every
evaluation takes a continuation and delivers its result to it. This makes
control-flow forms like `call/cc`, `catch`/`throw`, and `block`/`return-from`
expressible.

---

## 2. Notation

We write the valuation function as:

```
E[[expr]] env mem cont   — evaluate expr in environment env, store mem,
                           delivering result to continuation cont
```

Auxiliary:

```
E*[[e1 e2 ... en]] env mem cont
    — evaluate a list of expressions left-to-right, collecting values,
      then deliver the list to cont

B[[e1 e2 ... en]] env mem cont
    — evaluate a body (implicit begin): evaluate each expression in order,
      deliver the last result to cont
```

We write `mem[l := v]` for the store updated at location `l` with value `v`,
and `fresh(mem)` for a location not yet in the domain of `mem`.

---

## 3. Valuation Functions

### 3.1 Self-evaluating: number, string, boolean

```
E[[n]] env mem cont  =  cont(n, mem)          where n is a number literal
E[[s]] env mem cont  =  cont(s, mem)          where s is a string literal
E[[b]] env mem cont  =  cont(b, mem)          where b is #t or #f
```

Numbers, strings, and booleans evaluate to themselves. The store is
unchanged.

### 3.2 Symbol lookup

```
E[[x]] env mem cont  =  cont(mem(env(x)), mem)    where x is a symbol
```

Look up the symbol in the environment to get a location, then look up that
location in the store to get the current value. If `x` is not in `env`,
signal an error: `"unbound variable: x"`.

### 3.3 Quote

```
E[[(quote datum)]] env mem cont  =  cont(datum, mem)
```

Return the datum as-is, without evaluating it. The store is unchanged.

### 3.4 If (conditional)

```
E[[(if test consequent alternative)]] env mem cont  =
    E[[test]] env mem (lambda (v, mem')  ->
        if v != #f
        then E[[consequent]] env mem' cont
        else E[[alternative]] env mem' cont)

E[[(if test consequent)]] env mem cont  =
    E[[test]] env mem (lambda (v, mem')  ->
        if v != #f
        then E[[consequent]] env mem' cont
        else cont(void, mem'))
```

Evaluate the test expression first. If the result is anything other than
`#f` (i.e., truthy), evaluate the consequent; otherwise evaluate the
alternative. When no alternative is given, a false test yields `void`.
Only one branch is evaluated (the non-taken branch produces no effects).

### 3.5 Begin (sequencing)

```
E[[(begin)]] env mem cont  =  cont(void, mem)

E[[(begin e)]] env mem cont  =  E[[e]] env mem cont

E[[(begin e1 e2 ... en)]] env mem cont  =
    E[[e1]] env mem (lambda (_, mem')  ->
        E[[(begin e2 ... en)]] env mem' cont)
```

Evaluate expressions in order, threading the store through. The value of the
entire `begin` is the value of the last expression. An empty `begin` yields
`void`.

### 3.6 Define (binding creation)

```
E[[(define x expr)]] env mem cont  =
    E[[expr]] env mem (lambda (v, mem')  ->
        let l = fresh(mem')
        cont(void, mem'[l := v]))
    where env is extended with x -> l
```

Evaluate the expression, allocate a fresh location, store the value there,
and extend the environment to map `x` to that location. The result of
`define` is `void`.

For the shorthand `(define (f params...) body...)`, it is equivalent to
`(define f (lambda (params...) body...))`.

### 3.7 Set! (mutation)

```
E[[(set! x expr)]] env mem cont  =
    E[[expr]] env mem (lambda (v, mem')  ->
        let l = env(x)
        cont(void, mem'[l := v]))
```

Evaluate the expression, then update the store at the location already
bound to `x`. If `x` is not in `env`, signal an error:
`"cannot set! unbound variable: x"`. The result is `void`.

Note: `set!` does not create a new binding — it modifies an existing one.
Because closures that share a binding share the same location, mutation via
`set!` is visible to all of them.

### 3.8 Lambda (abstraction)

```
E[[(lambda (p1 ... pn) body...)]] env mem cont  =
    cont(Closure(p1...pn, body, env), mem)
```

Capture the current environment and return a closure. The body is not
evaluated until the closure is applied. This is what makes Strawman a
lexically scoped language: the closure remembers the environment at the
point of its creation.

### 3.9 Application (function call)

```
E[[(f a1 a2 ... an)]] env mem cont  =
    E[[f]] env mem (lambda (func, mem1)  ->
        E*[[a1 ... an]] env mem1 (lambda (args, mem2)  ->
            Apply(func, args, mem2, cont)))
```

where:

```
Apply(Closure(p1...pn, body, cenv), [v1...vn], mem, cont)  =
    let l1...ln = fresh(mem), fresh(mem[l1:=v1]), ...
    let env' = cenv extended with {p1->l1, ..., pn->ln}
    let mem' = mem[l1:=v1, ..., ln:=vn]
    B[[body]] env' mem' cont

Apply(Builtin(proc), args, mem, cont)  =
    cont(proc(args...), mem)
```

Evaluate the operator and all operands left-to-right. If the operator is a
closure, extend its captured environment with parameter bindings and evaluate
the body. If it is a builtin, call the underlying procedure directly. If the
number of arguments does not match the number of parameters, signal an error:
`"arity mismatch: expected n, got m"`. If the operator is not a procedure,
signal an error: `"not a procedure: v"`.

### 3.10 Let, Let*, Letrec

**let** — parallel binding:
```
E[[(let ((x1 e1) ... (xn en)) body...)]] env mem cont  =
    E*[[e1 ... en]] env mem (lambda (vals, mem')  ->
        let l1...ln = fresh locations
        let env' = env extended with {x1->l1, ..., xn->ln}
        let mem'' = mem'[l1:=v1, ..., ln:=vn]
        B[[body]] env' mem'' cont)
```

**let*** — sequential binding:
```
E[[(let* ((x1 e1) (x2 e2) ...) body...)]] env mem cont  =
    E[[e1]] env mem (lambda (v1, mem1)  ->
        let l1 = fresh(mem1), env1 = env + {x1->l1}, mem1' = mem1[l1:=v1]
        E[[e2]] env1 mem1' (lambda (v2, mem2)  ->
            ... B[[body]] envn memn' cont))
```

Each binding expression is evaluated in the environment that includes all
previous bindings (sequential dependency).

**letrec** — recursive binding:
```
E[[(letrec ((x1 e1) ... (xn en)) body...)]] env mem cont  =
    let l1...ln = fresh locations
    let env' = env extended with {x1->l1, ..., xn->ln}
    let mem' = mem[l1:=void, ..., ln:=void]
    E[[e1]] env' mem' (lambda (v1, mem1)  ->
        let mem1' = mem1[l1:=v1]
        E[[e2]] env' mem1' (lambda (v2, mem2)  ->
            ... B[[body]] env' memn' cont))
```

All binding locations are allocated first (initialized to void), then each
init expression is evaluated in the extended environment. This allows
mutually recursive lambdas.

### 3.11 And / Or (short-circuit)

```
E[[(and)]] env mem cont  =  cont(#t, mem)

E[[(and e)]] env mem cont  =  E[[e]] env mem cont

E[[(and e1 e2 ...)]] env mem cont  =
    E[[e1]] env mem (lambda (v, mem')  ->
        if v = #f
        then cont(#f, mem')
        else E[[(and e2 ...)]] env mem' cont)
```

```
E[[(or)]] env mem cont  =  cont(#f, mem)

E[[(or e)]] env mem cont  =  E[[e]] env mem cont

E[[(or e1 e2 ...)]] env mem cont  =
    E[[e1]] env mem (lambda (v, mem')  ->
        if v != #f
        then cont(v, mem')
        else E[[(or e2 ...)]] env mem' cont)
```

`and` returns `#f` at the first false value; `or` returns the first truthy
value. Both short-circuit: remaining expressions are not evaluated once the
result is determined.

---

## 4. Auxiliary Operations

### 4.1 Left-to-right evaluation of expression lists

```
E*[[]] env mem cont  =  cont([], mem)

E*[[e1 e2 ... en]] env mem cont  =
    E[[e1]] env mem (lambda (v1, mem1)  ->
        E*[[e2 ... en]] env mem1 (lambda (vs, memn)  ->
            cont(v1 :: vs, memn)))
```

### 4.2 Body evaluation (implicit begin)

```
B[[]] env mem cont  =  cont(void, mem)
B[[e]] env mem cont  =  E[[e]] env mem cont
B[[e1 e2 ...]] env mem cont  =
    E[[e1]] env mem (lambda (_, mem')  ->
        B[[e2 ...]] env mem' cont)
```

---

## 5. Correspondence to Implementation

| Semantic function | Racket implementation |
|---|---|
| `E[[expr]] env mem cont` | `(straw-eval/k expr env k)` |
| `E*[[...]]` | `(eval-list exprs env k)` |
| `B[[...]]` | `(eval-body exprs env k)` |
| Env | `env` struct with hash of symbol -> box |
| Mem | Implicit — Racket boxes *are* the store |
| Cont | Racket lambda `(lambda (v) ...)` |
| Closure | `(closure params body env)` struct |
| Location | Racket box (the indirection is the box itself) |

In the implementation, Env and Mem are fused: environment bindings map
directly to Racket boxes, so `env-lookup` performs both the `env(x)` and
`mem(l)` steps in one operation. The store is Racket's mutable heap.

---

## 6. Properties

The following properties hold for Strawman Lisp, derivable from the
semantics above:

1. **Lexical scope**: A closure captures the environment at the point of
   creation. Free variables are resolved in the defining environment, not
   the calling environment.

2. **Left-to-right evaluation**: In function application and `let`, operands
   are evaluated left to right. Side effects from earlier operands are visible
   to later ones.

3. **Tail position**: The last expression in `begin`, `if` branches, `let`
   bodies, and lambda bodies is in tail position — the continuation is passed
   directly, enabling tail-call optimization.

4. **Mutation via shared locations**: `set!` modifies the store at a shared
   location. All closures capturing the same binding see the update.

5. **Truthiness**: Only `#f` is false. All other values (including `0`, `""`,
   and `'()`) are truthy.
