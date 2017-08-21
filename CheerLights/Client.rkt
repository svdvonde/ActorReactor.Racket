#lang racket

(require "libs/lang/main.rkt"
         graphics/graphics)

(provide ClientBehaviour)

(define ClientBehaviour
  (ACTOR (IN change-colours)
         (LOCAL_STATE width height window)
         (MESSAGE (open-window w h)
                  (open-graphics)
                  (set! width  w)
                  (set! height h)
                  (set! window (open-viewport "CheerLights Client" w h)))

         (MESSAGE (set-background rgb)
                  ((clear-solid-rectangle window) (make-posn 0 0) width height)
                  ((draw-solid-rectangle window) (make-posn 0 0) width height rgb)
                  (sleep 0.5))
         
         (MESSAGE (change-colours rgbs)
                  (printf "received colours ~s" rgbs)
                  (for-each (lambda (rgb)
                              (~> self set-background rgb))
                            rgbs))))