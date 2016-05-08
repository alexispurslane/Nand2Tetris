#lang racket

(require math
         math/number-theory
         math/matrix)

(provide (except-out (all-from-out racket) curry compose negate)
         (all-from-out math)
         (except-out (all-from-out math/number-theory) permutations)
         (all-from-out math/matrix)
         (rename-out [curry :]
                     [compose ~>]
                     [negate !]))

(define (add1 x) (+ 1 x))
(define (sub1 x) (- x 1))

(define (c+ x y) (+ x y))
(define (c- x y) (- x y))
(define (c* x y) (* x y))
(define (c/ x y) (/ x y))

