#lang racket

(require "libs/lang/main.rkt"
         "Twitter.rkt"
         "CheerLights.rkt"
         "Client.rkt")




(define twitter    (SPAWN-ACTOR TwitterBehaviour))
(define tweets     (get-signal twitter 'tweet))

(define cheerlights   (SPAWN-REACTOR CheerLightsBehaviour tweets))
(define colours       (get-signal cheerlights 'colours-rgb))

(define client (SPAWN-ACTOR ClientBehaviour))
(~> client open-window 800 600)
(FOLLOW client colours)

(~> twitter open-stream "#cheerlights")