# Strawman Lisp Examples

A collection of small programs demonstrating the language.

## Running

From the project root, run a `.straw` file using the configured interpreter
(see `run_command` and `entry_point` in `config.json`):

```bash
# Default (Racket):
racket strawman.rkt examples/fizzbuzz.straw

# General form:
# <run_command> <entry_point> examples/<file>.straw
```

Available examples:

```
fizzbuzz.straw      fibonacci.straw     list-library.straw
hanoi.straw         sorting.straw       church.straw
assoc-db.straw      coroutines.straw    regex.straw
queue.straw         matrix.straw        metacircular.straw
retry.straw
```

## What's here

| File | Description | Features exercised |
|------|-------------|--------------------|
| `fizzbuzz.straw` | Classic FizzBuzz | Recursion, `mod`, `display` |
| `fibonacci.straw` | Memoized Fibonacci | Vectors for caching |
| `list-library.straw` | `map`, `filter`, `fold` from scratch | Higher-order functions, `cons`/`car`/`cdr` |
| `hanoi.straw` | Tower of Hanoi | Recursion, string output |
| `sorting.straw` | Mergesort on lists | List splitting, recursive merge |
| `church.straw` | Church numeral arithmetic | Pure lambda calculus, closures |
| `assoc-db.straw` | Key-value store on association lists | Functional data structures, `equal?` |
| `coroutines.straw` | Producer/consumer interleaving | `call/cc`, saved continuations |
| `regex.straw` | Pattern matcher on symbol lists | Recursive descent, `quote`, nested data |
| `queue.straw` | Mutable FIFO queue | `set-cdr!`, mutation, cons cells |
| `matrix.straw` | Matrix add, multiply, transpose | Vectors of vectors, nested loops |
| `metacircular.straw` | A Lisp interpreter in Strawman | Closures, environments, self-interpretation |
| `retry.straw` | Retry combinator with cleanup | `catch`/`throw`, `unwind-protect`, mutation |
