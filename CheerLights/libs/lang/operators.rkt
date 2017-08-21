#lang racket

(require (for-syntax syntax/parse)
         racket/future
         racket/async-channel
         "message.rkt"
         "signal.rkt"
         "application-context.rkt")

(provide PARALLEL/AND
         
         PARALLEL/OR
         
         AWAIT
         
         FOLLOW
         monitor-reactive-input
         make-operator)


(struct Operator (body) #:constructor-name make-operator)


(define-syntax (AWAIT stx)
  (syntax-parse stx
    [(_ signal:expr)
     #'(if (Signal? signal)
           (let ((channel-instance (signal->onetimeuse-channel signal)))
             (channel-instance-get channel-instance))
           (raise-argument-error 'AWAIT "Signal?" signal))]))



(begin-for-syntax
  (define-syntax-class operator-arguments
    #:description "operator arguments"
    (pattern ((actor-reactor:id output:id) ...+))))


(define-syntax (PARALLEL/AND stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (with-syntax ([(id ...) (generate-temporaries #'(body ...))])
       #'(let ((id (make-channel)) ...)
           (thread (λ () (channel-put id body))) ...
           (values (sync id) ...)))]))



(define-syntax (PARALLEL/OR stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (with-syntax ([(id ...) (generate-temporaries #'(body ...))])
       #'(let ((id (make-channel)) ...)
           (thread (λ () (channel-put id body))) ...
           (sync id ...)))]))




(define-syntax (FOLLOW stx)
  (syntax-parse stx
    [(_ actorreactor:expr input:expr ...+)
     #'(send-meta-message actorreactor
                          'set-reactive-input
                          (let ((inputs (list
                                         (parameterize ([#%app-context APPLICATION_CONTEXT_OPERATOR])
                                           input) ...)))
                            (printf "following with: ~a~n" inputs)
                            inputs))]))



                
                                   
(define (monitor-reactive-input owner message-identifier channel-or-operator)
  (cond ((Operator? channel-or-operator)
         (thread
          (λ () (let loop () (~>d owner message-identifier ((Operator-body channel-or-operator))) (loop)))))
        ((Channel-Permanent? channel-or-operator)
         (thread
          (λ () (let loop () (~>d owner message-identifier (channel-instance-get channel-or-operator)) (loop)))))
        (else (raise-argument-error 'signal-or-operator "a signal or an operator" channel-or-operator))))