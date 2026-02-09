# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/lexer.rkt` — Exports `tokenize` and `token`.
  - `token` struct: `(token type value)`, `#:transparent`.
  - `tokenize : string -> (listof token)` — currently returns `'()` (stub).
- `tests/test-lexer.rkt` — Requires `rackunit` and `src/lexer.rkt`.
  - 1 test: empty input `(tokenize "")` → `'()`.

## Conventions & Decisions

- Token struct is `(struct token (type value) #:transparent)`.
- Token types will be symbols: `'NUMBER`, `'STRING`, `'SYMBOL`, `'BOOLEAN`, `'LPAREN`, `'RPAREN`.
- Test file uses `check-equal?` with descriptive messages.
- Tests organized by Test Matrix rows from spec.md.

## Gotchas & Notes for Next Task

- `tokenize` is a stub that always returns `'()`. The next task (single number /
  negative / float) must add actual character-by-character lexing logic.
- The `token` struct is exported but not yet used in any test assertion that
  constructs tokens. Next test will be the first to use `(token 'NUMBER 42)`.
