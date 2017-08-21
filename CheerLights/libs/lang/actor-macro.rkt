#lang racket

(require (for-syntax syntax/parse
                     syntax/transformer)
         racket/set
         "signal.rkt"
         "literals.rkt"
         "operators.rkt"
         "message.rkt")

(require racket/stxparam)


(provide ACTOR
         LOCAL_STATE
         MESSAGE
         SPIT
         SPAWN-ACTOR
         self)


(struct actor-message-handler (identifier formal-parameters function) #:constructor-name make-message-handler #:transparent)
(struct actor-behaviour (state-variables reactive-input-variables handlers) #:constructor-name make-actor-behaviour #:transparent)

(define LOCAL_STATE
  (λ (stx) (raise-syntax-error 'LOCAL_STATE "should only be used inside an actor behaviour" stx)))

(define-syntax self (make-variable-like-transformer #'(current-thread)))

(define %spit-function% (make-parameter identity))

; ##########---------------------------####################---------------------------##########
; ##########----------------------------- SYNTAX CLASSES -----------------------------##########
; ##########---------------------------####################---------------------------##########

(begin-for-syntax
  (define-splicing-syntax-class actor-local-state
    #:description "actor local state"
    #:literals (LOCAL_STATE)
    (pattern (LOCAL_STATE state-variable:id ...))
    (pattern (~seq)
             #:with (state-variable ...) #'()))
  
  (define-syntax-class message-pattern
    #:description "actor message pattern"
    (pattern (identifier:id argument:id ...)))

  (define-splicing-syntax-class actor-reactive-input
    #:description "actor reactive input"
    #:literals (IN)
    (pattern (IN handler-identifier:id ...))
    (pattern (~seq)
             #:with (handler-identifier ...) #'())))


; define a syntax parameter such that we can pass syntax information from the ACTOR macro to the MESSAGE macro
(define-syntax-parameter local-state-variables
  (λ (stx) (raise-syntax-error 'local-state-variables "reserved keyword for actors" stx)))



; ##########---------------------------####################---------------------------##########
; ##########----------------------------- MESSAGE MACROS -----------------------------##########
; ##########---------------------------####################---------------------------##########


(define-syntax (SPIT stx)
  (syntax-parse stx
    [(_ out-identifier:expr value:expr)
     #'(if (symbol? out-identifier)
           ((%spit-function%) out-identifier value)
           (raise-syntax-error 'output-identifier "Symbol?" out-identifier))]))

; ##########---------------------------####################---------------------------##########
; ##########----------------------------- MESSAGE MACROS -----------------------------##########
; ##########---------------------------####################---------------------------##########

(define-syntax (MESSAGE stx)
  (syntax-parse stx
    [(_ pattern:message-pattern body:expr ...+)
     ; Introduce local syntax for the state variables of the actor. See the ACTOR macro for more information. 
     #:with (local-state-variable ...) (syntax-local-introduce (syntax-parameter-value #'local-state-variables))
     #'(make-message-handler
        'pattern.identifier
        '(pattern.argument ...)
        ; A message can be transformed to a lambda
        ;  the formal parameters of the message are the formal parameters of the lambda
        ;  this effectively hygienically binds these free identifiers in the body of the message
        (λ (pattern.argument ... #:local-state [current-state '()])
          ; define a new procedure to evaluate the body of the message, note that actor state is bound here
          (define (evaluate-body local-state-variable ...)
            ; calling this function first evaluates the body
            body ...
            ; and then returns the new (updated) local state
            (list local-state-variable ...))
          ; the result of evaluating a message is new actor state (this is expected by the actor instance)
          (apply evaluate-body current-state)))]))


   
; ##########---------------------------####################---------------------------##########
; ##########------------------------- ACTOR BEHAVIOUR MACROS -------------------------##########
; ##########---------------------------####################---------------------------##########

(define-syntax (ACTOR stx)
  (syntax-parse stx
    [(_ input:actor-reactive-input state:actor-local-state handler:expr ...+)
     ; There is an interplay between this ACTOR macro and the MESSAGE macro.
     ; Consider the body of a message: there are two sets of free identifiers that need to be bound by our macro
     ; For example, consider
     ; (ACTOR (LOCAL_STATE data)
     ;        (MESSAGE (save new_data)
     ;                   (set! data new-data)))
     ; The first (and immediately obvious) set are the formal parameters of the message, in our example 'new_data'
     ;   formal parameters are very easy to bind, since a message is transformed to a lambda with the same formal parameters
     ;   in our example: (λ (new_data) (set! data new_date))
     ;   thus, new_data is bound and we're all set.
     ; The second set of free identifiers is the local state, in our example the 'data' local state variable.
     ;   the local state variables are introduced in the ACTOR macro, and not the MESSAGE macro
     ;   if we were to use the same technique of introducing some new local scope via a lambda (or equivalent),
     ;   then the identifiers introduced by the ACTOR scope would be different from those in the MESSAGE scope,
     ;   despite them having the same name. This is only natural, since otherwise we would be unhygienically binding free variables!
     ;   thus we need to pass the identifiers dynamically to the MESSAGE macro via a syntax parameter,
     ;   where we introduce them as local syntax (via syntax-local-introduce) to the body of the messages
     ; for reference, see this post: http://stackoverflow.com/questions/41139868/dynamically-binding-free-identifiers-in-a-function-body
     
     #'(syntax-parameterize ([local-state-variables #'(state.state-variable ...)])
         (define handler-implementations (list handler ...))
         
         (let ((identifiers (map actor-message-handler-identifier handler-implementations)))
           (unless (andmap (curryr member identifiers) '(input.handler-identifier ...))
             (raise-syntax-error 'IN "Arguments to IN must reference implemented message handlers in the behaviour" 'input.handler-identifier ...)))
           
         (make-actor-behaviour
          '(state.state-variable ...)
          '(input.handler-identifier ...)
          handler-implementations))]))


; ##########---------------------------####################---------------------------##########
; ##########---------------- BEHAVIOUR INSTANCES & EVENT LOOP MACROS -----------------##########
; ##########---------------------------####################---------------------------##########

(define-syntax (SPAWN-ACTOR stx)
  (syntax-parse stx
    #:literals (IN)
    [(_ behaviour:id)
     #'(thread
        (λ ()
          ; First create a hash table mapping messages to message handlers (so we don't have to traverse the list all the time)
          ;   to do this, collect all handlers, the messages they accept, and throw it in a hash
          (define handler-hash
            (let* ((handlers (actor-behaviour-handlers behaviour))
                   (accepted-messages (map actor-message-handler-identifier handlers)))
              (make-hash (map cons accepted-messages handlers))))  

          (define signal-channels (make-hash))

          (define (get-signal-channel signal-identifier)
            (hash-ref signal-channels signal-identifier make-signal-channel))
           
          (define (set-signal-channel signal-identifier new-signal-channel)
            (hash-set! signal-channels signal-identifier new-signal-channel))

           
          ; initialize the local state of the actor, which is simply a list that corresponds to the state variables
          (define state-variables (actor-behaviour-state-variables behaviour))
          (define current-state (make-list (length state-variables) (void)))


          (define reactive-input-identifiers (actor-behaviour-reactive-input-variables behaviour))
          (define raw-inputs             (make-list (length reactive-input-identifiers) #f))
          (define reactive-input-threads (make-list (length reactive-input-identifiers) #f))
          
          (define (handle-meta-message source identifier arguments)
            (case identifier
              ((set-reactive-input)
               (define updated-reactive-threads
                 (map (λ (identifier old-raw-input old-reactive-thread new-raw-input)
                        (cond ((false? new-raw-input)
                               (when old-reactive-thread
                                 (kill-thread old-reactive-thread)))
                              ((false? old-raw-input)
                               (if (Signal? new-raw-input)
                                   (monitor-reactive-input (current-thread) identifier (signal->permanent-channel new-raw-input))
                                   (monitor-reactive-input (current-thread) identifier new-raw-input)))
                              ((equal? old-raw-input new-raw-input)
                               old-reactive-thread)
                              (else (kill-thread old-reactive-thread)
                                    (if (Signal? new-raw-input)
                                        (monitor-reactive-input (current-thread) identifier (signal->permanent-channel new-raw-input))
                                        (monitor-reactive-input (current-thread) identifier new-raw-input)))))
                      reactive-input-identifiers
                      raw-inputs
                      reactive-input-threads
                      arguments))
               (set! reactive-input-threads updated-reactive-threads)
               (set! raw-inputs arguments))

              
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
               (channel-put source channel-instance))

               
              (else (raise-argument-error 'handle-meta-message "a meta message that has been implemented by this actor" identifier))))
           
          (define (produce-output identifier value)
            (define signal-channel (get-signal-channel identifier))
            ; send the payload to all receivers - a new signal channel is returned,
            ; because "one time use" channels may have been removed
            (define updated-signal-channel (signal-channel-put signal-channel value))
            (set-signal-channel identifier updated-signal-channel))

          
          
          (define (handle-base-message source identifier arguments)
            ; define the new state of the actor by executing the message handler
            (define new-state
              ; install an exception handler 
              (with-handlers
                  ; hash-ref throws a fail exception when the key (i.e. message handler) is not found
                  ;   in this case return the current state
                  ([exn:fail:contract? (λ (exn) current-state)])
                ; find the message handler, extract its function, and apply it with the actual parameters and pass the current actor state
                (define handler (hash-ref handler-hash identifier))
                (define handler-function (actor-message-handler-function handler))
                (parameterize ([%spit-function% produce-output])
                  (apply handler-function arguments #:local-state current-state))))
            ; update the actor state after successfully evaluating the message handler
            (set! current-state new-state))


           
          ; create a new event loop that continuously dequeues messages and processes them.
          ; note that our actor makes use of the built-in message queue of racket threads, over which we have no control!
          ; this should be fine for now, and in the future we can potentially use a double queue with two threads (which is less than ideal, but whatever)
          (let event-loop ()
            (define message        (thread-receive))
            (define msg-identifier (Message-identifier message))
            (define msg-source     (Message-source message))
            (define msg-arguments  (Message-arguments message))

            (cond ((Meta-Message? message)
                   (handle-meta-message msg-source msg-identifier msg-arguments))
                  ((Message? message)
                   (handle-base-message msg-source msg-identifier msg-arguments))
                  (else (error "actor does not know how to handle this type of message: " message)))
            ; our actors implement implicit recursion 
            (event-loop))))]))

  



; ##########---------------------------####################---------------------------##########
; ##########-------------------------- TESTING & DEBUGGING ---------------------------##########
; ##########---------------------------####################---------------------------##########
              

