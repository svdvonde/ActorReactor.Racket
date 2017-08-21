#lang racket


(provide IN foldp)

(define IN (λ (stx) (raise-syntax-error 'IN "literal can only be used in an actor or reactor" stx)))
(define foldp (λ (stx) (raise-syntax-error 'foldp "literal can only be used in an actor or reactor" stx)))
