#lang racket

(require (for-syntax racket/syntax))
(require racket/stxparam)

(provide APPLICATION_CONTEXT_RACKET
         APPLICATION_CONTEXT_REACTOR
         APPLICATION_CONTEXT_OPERATOR
         #%app-context
         %constant-root%)

(define APPLICATION_CONTEXT_RACKET 'racket)
(define APPLICATION_CONTEXT_REACTOR 'reactor)
(define APPLICATION_CONTEXT_OPERATOR 'operator)


(define #%app-context (make-parameter APPLICATION_CONTEXT_RACKET))
(define %constant-root% (make-parameter 'not-defined))