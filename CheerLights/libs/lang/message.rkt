#lang racket

(require (for-syntax syntax/parse))

(provide (struct-out Message)
         (struct-out Meta-Message)
         ~>
         ~>d)


(struct Message (source identifier arguments) #:transparent #:constructor-name make-message)
(struct Meta-Message ()  #:super struct:Message #:constructor-name make-meta-message)


; ##########---------------------------####################---------------------------##########
; ##########------------------------- MESSAGE SENDING MACROS -------------------------##########
; ##########---------------------------####################---------------------------##########

; sending a message to an actor or reactor is simply performing a thread send,
;  since the event loop is an actual racket thread (the entry point of an actor or reactor)
(define-syntax (~> stx)
  (syntax-parse stx
    [(_ actorreactor:expr message-identifier:id actual-parameter:expr ...)
     #'(thread-send actorreactor (make-message (current-thread) 'message-identifier (list actual-parameter ...)))]))

(define-syntax (~>d stx)
  (syntax-parse stx
    [(_ actorreactor:expr message-identifier:expr actual-parameter:expr ...)
     #'(thread-send actorreactor (make-message (current-thread) message-identifier (list actual-parameter ...)))]))