#lang racket

(require (for-syntax racket/syntax
                     syntax/parse))

(require racket/function
         "reactor-internal-graph.rkt"
         "application-context.rkt"
         (only-in "signal.rkt"    Signal?)
         (only-in "operators.rkt" make-operator))

(provide (except-out (all-from-out racket) #%app)
         (rename-out (actor-reactor-application #%app)))


(define-syntax (actor-reactor-application stx)
  (syntax-parse stx
    [(_ function arg ...)
     #'(cond ((equal? (#%app-context) APPLICATION_CONTEXT_RACKET)
              (#%app function arg ...))


             ((equal? (#%app-context) APPLICATION_CONTEXT_REACTOR)
              (define arguments
                (map (λ (original-argument)
                       (cond ((reactor-node? original-argument)
                              original-argument)
                             (else (create-constant-node (λ () original-argument) (%constant-root%)))))
                     (list arg ...)))
              (create-function-node function arguments))

             
             ((equal? (#%app-context) APPLICATION_CONTEXT_OPERATOR)
              (make-operator
               (thunk (parameterize ([#%app-context APPLICATION_CONTEXT_RACKET])
                        (#%app function arg ...)))))
             

             (else (raise-syntax-error 'bad-application "expected an application" (list function arg ...))))]))