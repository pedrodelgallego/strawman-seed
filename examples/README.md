# Strawman Lisp Examples

A collection of small programs demonstrating the language.

## Running

From the project root, pass the file to `racket strawman.rkt`:

```bash
racket strawman.rkt examples/fizzbuzz.straw
racket strawman.rkt examples/fibonacci.straw
racket strawman.rkt examples/list-library.straw
```

## What's here

| File | Description |
|------|-------------|
| `fizzbuzz.straw` | Classic FizzBuzz (loops, conditionals, `mod`, `display`) |
| `fibonacci.straw` | Memoized Fibonacci using vectors for caching |
| `list-library.straw` | `map`, `filter`, `fold` built from scratch, then composed |
