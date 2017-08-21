#lang racket

(require "libs/oauth-single-user.rkt"
         "libs/lang/main.rkt"
         json)


(provide TwitterBehaviour)


(define TwitterBehaviour
  (ACTOR (LOCAL_STATE current-stream)
         (MESSAGE (open-stream keyword)
                  (set! current-stream
                        (send twitter-oauth get-request 
                              "https://stream.twitter.com/1.1/statuses/filter.json"
                              (list (cons 'track keyword))))
                  (~> self read-tweet))
         
         (MESSAGE (read-tweet)
                  (define data (read-json current-stream))
                  (when (jsexpr? data)
                    (define tweet-text (string-downcase (hash-ref data 'text)))
                    (define tweet-chars (string-split tweet-text ""))
                    (for-each (lambda (char)
                                (SPIT 'tweet-char char))
                              tweet-chars)
                    (~> self read-tweet)))))