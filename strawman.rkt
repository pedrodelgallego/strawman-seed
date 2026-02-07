#lang racket
(require "src/repl.rkt")

(define args (vector->list (current-command-line-arguments)))
(cond
  [(null? args) (run-repl)]
  [else (run-file (car args))])
