#lang racket
(require 2htdp/image)
(provide icon icon2 icon3 icon4)


(define (icon a) (let ([r (square a "solid" (make-color 250 0 0))]
                       [w (square a "solid" (make-color 250 250 250))]
                       [d (square a "solid" (make-color 0 100 0))]
                       [g (square a "solid" (make-color 0 150 0))]
                       [b (square a "solid" (make-color 0 200 0))])
                   (above (beside       d d d d)
                          (beside     d b b b b d)
                          (beside     g d d d d g)
                          (beside   d g g g g g g d)
                          (beside d b g g g g g g b d)
                          (beside   d d d d d d d d)
                          (beside     w r w w r w)
                          (beside     w w w w w w)
                          (beside     w w w w w w)
                          (beside       w w w w))
                   ))

(define (icon2 a) (let ([b (square a "solid" (make-color 0 0 150))]
                       [l (square a "solid" (make-color 0 0 250))]
                       [d (square a "solid" (make-color 0 0 0))]
                       [s (square a "solid" (make-color 250 200 150))]
                       [y (square a "solid" (make-color 250 150 100))])
                   (above (beside         b b)
                          (beside     b b l l b b)
                          (beside   b l l y y l l b)
                          (beside b l l l y y l l l b)
                          (beside   b d d d d d d b)
                          (beside     s d d d d s)
                          (beside     s b s s b s)
                          (beside     s s s s s s)
                          (beside     s s d d s s)
                          (beside       s s s s))
                   ))
(define (icon3 a) (let ([b (square a "solid" (make-color 150 100 100))]
                       [d (square a "solid" (make-color 0 0 0))]
                       [s (square a "solid" (make-color 250 200 150))]
                       )
                   (above (beside     b b b b b b)
                          (beside     b b b b b b)
                          (beside   b b b b b b b b)
                          (beside   b s s s s s s b)
                          (beside   b s s s s s s b)
                          (beside     d d d d d d)
                          (beside     d d s s d d)
                          (beside     b s s s s b)
                          (beside     b s s s s b)
                          (beside       b b b b))
                   ))

(define (icon4 a) (let ([r (square a "solid" (make-color 250 0 0))]
                       [w (square a "solid" (make-color 250 250 250))]
                       [d (square a "solid" (make-color 0 100 0))]
                       [g (square a "solid" (make-color 0 150 0))]
                       [b (square a "solid" (make-color 0 200 0))])
                   (above (beside       g b b g)
                          (beside       g g g g)
                          (beside       g g g g)
                          (beside   d d g g g g d d)
                          (beside d b b g g g g b b d)
                          (beside   d d d d d d d d)
                          (beside     w r w w r w)
                          (beside     w w w w w w)
                          (beside       w w w w)
                          (beside         w w))
                   ))
