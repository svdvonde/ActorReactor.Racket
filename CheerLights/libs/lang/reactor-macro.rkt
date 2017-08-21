#lang racket


(require (for-syntax racket/syntax
                     syntax/parse))
(require racket/stxparam
         "signal.rkt"
         "reactor-internal-graph.rkt"
         "application-context.rkt"
         "literals.rkt"
         "operators.rkt")

(provide REACTOR
         SPAWN-REACTOR
         TICK OUT)


(struct reactor-behaviour (thunk) #:constructor-name make-reactor-behaviour #:transparent)
(struct reactor (inputs ticks outputs event-dispatcher) #:constructor-name make-reactor #:transparent)

; ##########---------------------------####################---------------------------##########
; ##########--------------------------------- MACROS ---------------------------------##########
; ##########---------------------------####################---------------------------##########


(define TICK (λ (stx) (raise-syntax-error 'TICK "literal can only be used in a reactor" stx)))
(define OUT (λ (stx) (raise-syntax-error 'OUT "literal can only be used in a reactor" stx)))

(define %output-production-func% (make-parameter void))

(begin-for-syntax
  (define-splicing-syntax-class reactor-input
    #:description "reactor input requirements"
    #:literals (IN)
    (pattern (IN identifier:id ...)))

  (define-syntax-class reactor-tick
    #:description "reactor tick"
    #:literals (TICK)
    (pattern (TICK identifier:id body:expr)))
  
  (define-splicing-syntax-class reactor-output
    #:description "reactor output"
    #:literals (OUT)
    (pattern (OUT identifier:id ...))))



(define-syntax-rule (foldp func acc input #:spit-when pred)
  ; revert the application contetx of the foldp primitive to regular racket
  (parameterize ([#%app-context APPLICATION_CONTEXT_RACKET])
    (create-foldp-node func acc (list input) pred)))



(define-syntax (REACTOR stx)
  (syntax-parse stx
    [(_ root:reactor-input tick:reactor-tick ...+ output:reactor-output)
     #:with (local-root ...) (syntax-local-introduce #'(root.identifier ...))
     #'(make-reactor-behaviour
        (λ ()
          (parameterize ([%constant-root% (create-constant-root-node)])
            (define (produce-output tick-id value)
              ((%output-production-func%) tick-id value))
            
            (define root.identifier (create-root-node 'root.identifier))
            ...
            (define input-roots (list root.identifier ...))
            (define tick.identifier
              (let ((node-creation-func (λ (local-root ...)
                                          (parameterize ([#%app-context APPLICATION_CONTEXT_REACTOR]) tick.body))))
                (node-creation-func root.identifier ...)))
            ...
            (send tick.identifier modify-update-callback (curry produce-output 'tick.identifier))
            ...
            (define ticks (list tick.identifier ...))
            (define event-dispatcher (create-event-dispatcher (cons (%constant-root%) input-roots)))
            (make-reactor '(root.identifier ...) ticks (list 'output.identifier ...) event-dispatcher))))]))

(define-syntax (SPAWN-REACTOR stx)
  (syntax-parse stx
    [(_ behaviour:expr input:expr ...+)
     #'(thread
        (λ ()
          (define reactor ((reactor-behaviour-thunk behaviour)))

          (define signal-channels (make-hash))
          (define (get-signal-channel signal-identifier)
            (hash-ref signal-channels signal-identifier make-signal-channel))
          (define (set-signal-channel signal-identifier new-signal-channel)
            (hash-set! signal-channels signal-identifier new-signal-channel))


          (define (get-signals-or-operators inputs)
            (map (λ (maybe-signal)
                   (if (Signal? maybe-signal)
                       (signal->permanent-channel maybe-signal) maybe-signal))
                 inputs))
          

          (define raw-input     (list input ...))
          (define input-threads (map (curry monitor-reactive-input (current-thread)) (reactor-inputs reactor) (get-signals-or-operators raw-input)))


          (define (produce-output identifier value)
            (define signal-channel (get-signal-channel identifier))
            ; send the payload to all receivers - a new signal channel is returned,
            ; because "one time use" channels may have been removed
            (define updated-signal-channel (signal-channel-put signal-channel value))
            (set-signal-channel identifier updated-signal-channel))
          
           
          (define (handle-meta-message source identifier arguments)
            (case identifier
              ((set-reactive-input)
               (define reactor-inputs (reactor-inputs reactor))
               (define new-input-threads
                 (map (λ (input-identifier old-raw-input old-input-thread new-raw-input)
                        (cond ((equal? old-raw-input new-raw-input)
                               old-input-thread)
                              (else (kill-thread old-input-thread)
                                    (if (Signal? new-raw-input)
                                        (monitor-reactive-input (current-thread) input-identifier (signal->permanent-channel new-raw-input))
                                        ; input is already an operator
                                        (monitor-reactive-input (current-thread) input-identifier new-raw-input)))))
                      (reactor-inputs reactor)
                      raw-input
                      input-threads
                      arguments))
               (set! input-threads new-input-threads)
               (set! raw-input arguments))

               
              ((create-output-channel)
               (define signal-identifier (car arguments))
               (define signal-channel (get-signal-channel signal-identifier))
               (define-values (channel-instance new-signal-channel)
                 (signal-channel-create-permanent-receiver (current-thread) signal-identifier signal-channel))
               (set-signal-channel signal-identifier new-signal-channel)
               (channel-put source channel-instance))

              ((create-one-time-use-channel)
               (define signal-identifier (car arguments))
               (define signal-channel (get-signal-channel signal-identifier))
               (define-values (channel-instance new-signal-channel)
                 (signal-channel-create-onetimeuse-receiver signal-identifier signal-channel))
               (set-signal-channel signal-identifier new-signal-channel)
               (channel-put source channel-instance))))
        
          (define (handle-base-message source identifier arguments)
            (define dispatcher (reactor-event-dispatcher reactor))
            (parameterize ((%output-production-func% produce-output))
              (update-signal dispatcher identifier arguments)))


          (let event-loop ()
            (define message    (thread-receive))
            (define source     (Message-source message))
            (define identifier (Message-identifier message))
            (define arguments  (Message-arguments message))
            
            (if (Meta-Message? message)
                (handle-meta-message source identifier arguments)
                (handle-base-message source identifier arguments))
          
            (event-loop))))]))