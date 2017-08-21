#lang racket

(require "libs/lang/main.rkt"
         (only-in graphics/graphics make-rgb))


(provide CheerLightsBehaviour)


(define colour-map
  (make-hash (list
              (cons "red"     (make-rgb 1    0    0))
              (cons "green"   (make-rgb 0    0.5  0))
              (cons "blue"    (make-rgb 0    0    1))
              (cons "cyan"    (make-rgb 0    1    1))
              (cons "white"   (make-rgb 0    0    0))
              (cons "oldlace" (make-rgb 0.99 0.96 0.9))
              (cons "purple"  (make-rgb 0.5  0    0.5))
              (cons "magenta" (make-rgb 1    0    1))
              (cons "yellow"  (make-rgb 1    1    0))
              (cons "orange"  (make-rgb 1    0.65 0))
              (cons "pink"    (make-rgb 1    0.75 0.8))
              (cons "black"   (make-rgb 1    1    1)))))


(define recognized-colours (hash-keys colour-map))

(define (is-colour? string)
  (member string recognized-colours))

(define (colour->rgb string)
  (hash-ref colour-map string))

(define (get-tweet-username tweet)
  (define user (hash-ref tweet 'user))
  (hash-ref user 'name))

(define (get-tweet-text tweet)
  (hash-ref tweet 'text))


(define (word-append char str)
  (printf "debug: word appending with '~s' and '~s' ~n" char str)
  (if (or (string=? char " ")
          (string=? char "\n"))
      str
      (string-append str char)))


(define CheerLightsBehaviour
  (REACTOR
   (IN tweet-char)
   (TICK colour (foldp word-append "" tweet-char is-colour?))
   (TICK colour-rgb (colour->rgb colour))
   (OUT colour-rgb)))