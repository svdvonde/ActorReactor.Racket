#lang racket


(require racket/class)

(provide create-event-dispatcher
         update-signal
         reactor-node?
         create-root-node
         create-constant-root-node
         create-function-node
         create-stateful-node
         create-constant-node
         create-foldp-node
         
         ; for debug purposes
         (struct-out EventDispatcher))

(define NO_VALUE (void))

(struct EventDispatcher (sources))

(define (create-event-dispatcher sources)
  (EventDispatcher sources))

(define (update-signal dispatcher topic value)
  ; One value is expected, but it's always treated as if it could be a list. So we can safely take the first argument as "the" value. 
  (define proper-value (car value))
  (define sources (EventDispatcher-sources dispatcher))
  (for-each (λ (source) (send source receive-event topic proper-value)) sources))





(struct Event (source value))
(struct Change   () #:super struct:Event)
(struct NoChange () #:super struct:Event)
  

(define (compact-duplicates lst)
  (define key car)
  (define value cdr)
  (define make-kv cons)
  (define kv-equal? (λ (kv1 kv2) (equal? (key kv1) (key kv2))))
  (cond ((empty? lst) '())
        (else
         (define kv (car lst))
         (define kv-key (key kv))
         (define duplicates (filter (curry kv-equal? kv) lst))
         (define non-duplicates (remove* duplicates lst))
         (define values (map value duplicates))
         (cons (make-kv kv-key values)
               (compact-duplicates non-duplicates)))))



(define node%
  (class object%
    (super-new)
    
    (init-field parents
                [children '()]
                [when-update void]
                ; Store the value that this most recently produced
                [most-recent-value NO_VALUE])

    (define/public (modify-update-callback callback)
      (set! when-update callback))

    (define/public (add-child new-child)
      ; When a function uses the same node twice (e.g. (+ seconds seconds)), then the + node would register itself 2 times with the same node
      ; In this case we do not add the child, such that we only send the data once.
      (unless (member new-child children)
        (set! children (cons new-child children))))
    
    (define/public (send-to-children event)
      (for-each (λ (child) (send child receive-event event)) children))
    
    (define/public (update-value new-value)
      ;(displayln "propagating change event")
      (when-update new-value)
      (set! most-recent-value new-value)
      (send-to-children (Change this new-value))
      new-value)

    (define/public (no-update)
      ;(displayln "propagating no-change event")
      (send-to-children (NoChange this most-recent-value))
      most-recent-value)

    ; debug methods
    (define/public (get-parents)
      parents)
    (define/public (get-children)
      children)
    (define/public (get-value)
      most-recent-value)
    
    (abstract receive-event)

    (for-each (λ (parent) (send parent add-child this)) parents)))

(define root-node%
  (class node%
    (super-new [parents '()])
    (init-field topic)
    
    (define/override (receive-event _topic value)
      (when (equal? topic _topic)
        (send this update-value value)))))

(define constant-root%
  (class root-node%
    (super-new [topic NO_VALUE])
    (define/override (receive-event _ value)
      (send this update-value value))))

(define constant-node%
  (class node%
    (super-new)

    (init-field thunk)
    (inherit-field most-recent-value)

    (define/override (receive-event _)
      ;(displayln "constant node received event")
      (if (equal? most-recent-value NO_VALUE)
          (send this update-value (thunk))
          (send this no-update)))))
    
(define function-node%
  (class node%
    (super-new)
    
    (init-field function)
    (inherit-field parents)

    (define (create-parent-to-index-hash parents)      
      ; First create a normal assoc list of (<parent> . int)
      (define unchecked (map cons parents (build-list (length parents) values)))
      ; then compact all the duplicates, such that duplicate parents get a list of ints
      (define compacted (compact-duplicates unchecked))
      (make-hash (compact-duplicates unchecked)))
      
    
    (field
     ; we can only evaluate this node once we received something from all parents
     ; if there are duplicates in the parents, this means we would be receiving the same value twice
     ; because same parent = literally the same object
     ; the parent is smart enough to only send us the data once,
     ; so we should consider the parent as one entity as well, but still respect their ordering in the input
     [parent-idx-hash (create-parent-to-index-hash parents)]
     [received-values (make-vector (length parents) NO_VALUE)])
    
    (define/private (evaluateable? events)
      (andmap Event? events))
    
    (define/private (updateable? events)
      (ormap  Change? events))
    
    (define/override (receive-event event)
      (save-event-value! event)
      (define events (vector->list received-values))

      (when (evaluateable? events)
        (cond ((updateable? events)
               (define args (map Event-value events))
               (vector-fill! received-values NO_VALUE)
               (define evaluation-result (send this evaluate args))
               (if (eq? evaluation-result NO_VALUE)
                   (send this no-update)
                   (send this update-value evaluation-result)))
              (else
               (vector-fill! received-values NO_VALUE)
               (send this no-change)))))

    (define/private (save-event-value! event)
      (define parent (Event-source event))
      (define indexes (hash-ref parent-idx-hash parent))
      (for-each (curryr (curry vector-set! received-values) event)
                indexes))

    (define/public (evaluate args)
      (apply function args))))


(define stateful-node%
  (class function-node%
    (super-new)
    
    (init-field initial-state)
    (inherit-field function
                   most-recent-value)
        
    (define/override (evaluate args)
      (if (equal? most-recent-value NO_VALUE)
          (apply function (cons initial-state args))
          (apply function (cons most-recent-value args))))))


(define foldp-node%
  (class function-node%
    (super-new)

    (init-field initial-accumulator
                predicate)
    (inherit-field function)
    (field [current-accumulator initial-accumulator])

    

    (define/override (evaluate args)
      (printf "invoking with ~s~n" (cons current-accumulator args))
      (define new-accumulator (apply function (reverse (cons current-accumulator args))))
      (printf "updated acc from ~s to ~s ~n" current-accumulator new-accumulator)
      (if (predicate new-accumulator)
          (begin (set! current-accumulator initial-accumulator)
                 new-accumulator)
          (begin (set! current-accumulator new-accumulator)
                 NO_VALUE)))))
          
                   

(define reactor-node? (curryr is-a? node%))

(define (create-root-node _topic #:when-update [callback void])
  (new root-node% [topic _topic] [when-update callback]))

(define (create-constant-root-node)
  (new constant-root%))

(define (create-constant-node thk root #:when-update [callback void])
  (new constant-node% [thunk thk] [parents (list root)] [when-update callback]))

(define (create-function-node func args #:when-update [callback void])
  (new function-node% [function func] [parents args] [when-update callback]))

(define (create-stateful-node func init args #:when-update [callback void])
  (new stateful-node% [function func] [initial-state init] [parents args] [when-update callback]))

(define (create-foldp-node func acc args pred #:when-update [callback void])
  (new foldp-node%
       [function func]
       [parents args]
       [initial-accumulator acc]
       [predicate pred]
       [when-update callback]))

(define seconds (create-root-node 'seconds))
;(define adder (create-function-node (curry + 5) (list seconds) #:when-update (lambda (x) (displayln x))))
;(define dispatcher (create-event-dispatcher (list seconds)))
;(update-signal dispatcher 'seconds 5)
;(update-signal dispatcher 'seconds 15)
;
;(update-signal dispatcher 'seconds 6)
;(update-signal dispatcher 'seconds 7)


;(define folder (create-foldp-node +  0 (list seconds) (lambda (acc) (> acc 15)) #:when-update (lambda (x) (displayln x))))
;(define dispatcher (create-event-dispatcher (list seconds)))
;(update-signal dispatcher 'seconds (list 5))
;(update-signal dispatcher 'seconds 10)
