#lang racket

(provide tokenize token)

;; Token struct: type is a symbol, value is the datum
(struct token (type value) #:transparent)

;; tokenize : string -> (listof token)
(define (tokenize input)
  '())
