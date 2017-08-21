#lang racket

(require racket/async-channel
         "message.rkt")

(provide (struct-out Message)
         (struct-out Meta-Message)
         (struct-out Channel-Instance)
         (struct-out Channel-Permanent)
         (struct-out Channel-OneTimeUse)

         (struct-out Signal)         
         get-signal
         signal->permanent-channel
         
         signal-channel-create-permanent-receiver
         signal-channel-create-onetimeuse-receiver
         signal-channel-put
         make-signal-channel
         signal->onetimeuse-channel
         signal-channel-remove-receiver
         channel-instance-get
         send-meta-message)



(struct Channel-Instance (identifier channel) #:transparent)
(struct Channel-Permanent (source) #:super struct:Channel-Instance #:constructor-name make-permanent-channel)
(struct Channel-OneTimeUse () #:super struct:Channel-Instance #:constructor-name make-onetimeuse-channel)

(struct Signal-Channel (receivers) #:transparent)
(struct Signal (source identifier) #:transparent)



(define (get-signal actorreactor output)
  (Signal actorreactor output))
  

(define (signal->permanent-channel signal)
  (define actorreactor (Signal-source signal))
  (define output (Signal-identifier signal))
  
  (unless (symbol? output)
    (raise-argument-error 'output-identifier "Symbol?" output))
  
  (define channel (make-channel))
  (send-meta-message actorreactor 'create-output-channel (list output) #:source channel) 
  (channel-get channel))



(define (signal->onetimeuse-channel signal)
  (define actorreactor (Signal-source signal))
  (define output (Signal-identifier signal))
  
  (unless (symbol? output)
    (raise-argument-error 'output-identifier "Symbol?" output))
  
  (define channel (make-channel))
  (send-meta-message actorreactor 'create-one-time-use-channel (list output) #:source channel) 
  (channel-get channel))


(define (channel-ref-onetimeuse thread output-identifier)
  (unless (symbol? output-identifier)
    (raise-argument-error 'output-identifier "Symbol?" output-identifier))
  
  (define channel (make-channel))
  (thread-send thread (make-meta-message channel 'create-one-time-use-channel (list output-identifier)))
  (channel-get channel))



(define (send-meta-message thread message arguments #:source [source #f])
  (define packet (make-meta-message source message arguments))
  (thread-send thread packet))





(define (make-signal-channel #:receivers [receivers '()])
  (Signal-Channel receivers))


(define (signal-channel-insert-receiver channel receiver)
  (define receivers (Signal-Channel-receivers channel))
  (define new-channel (make-signal-channel #:receivers (cons receiver receivers)))
  new-channel)

(define (signal-channel-remove-receiver channel receiver)
  (define receivers (Signal-Channel-receivers channel))
  (define new-channel (make-signal-channel #:receivers (remove receiver receivers)))
  new-channel)



(define (signal-channel-create-permanent-receiver source signal-identifier channel)
  (define receiver (make-permanent-channel signal-identifier (make-async-channel 1) source))
  (define new-channel (signal-channel-insert-receiver channel receiver))
  (values receiver new-channel))


(define (signal-channel-create-onetimeuse-receiver signal-identifier channel)
  (define receiver (make-onetimeuse-channel signal-identifier (make-async-channel 1)))
  (define new-channel (signal-channel-insert-receiver channel receiver))
  (values receiver new-channel))



(define (signal-channel-put channel payload)
  (define channel-instances (Signal-Channel-receivers channel))
  (define async-channels (map Channel-Instance-channel channel-instances))
  ; first remove any events that are still on the channel
  (for-each async-channel-try-get async-channels)
  ; then push a new value to all receivers
  (for-each (curryr async-channel-put payload) async-channels)
  (define permanent-instances (filter Channel-Permanent? channel-instances))
  (make-signal-channel #:receivers permanent-instances))



(define (channel-instance-get channel)
  (define underlying-channel (Channel-Instance-channel channel))
  (async-channel-get underlying-channel))



