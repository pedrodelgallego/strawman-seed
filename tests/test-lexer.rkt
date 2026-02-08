#lang racket

(require rackunit
         "../src/lexer.rkt")

;; E1.1 — Lexer: Tokenize source text

;; Empty input → empty list
(check-equal? (tokenize "") '()
              "empty input returns empty token list")
