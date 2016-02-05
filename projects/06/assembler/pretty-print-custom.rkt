#lang racket

(require "parser.rkt")

(provide pretty-print/symbol-table
		 pretty-print/parse-tree)

(define (pretty-print/symbol-table st [width 100])
  (string-join
   (for/list ([pair (hash->list st)])
	 (match-define (cons k v) pair)
	 (define string-v (number->string (first v)))
	 (string-append k
					(~a ""
						#:width (- width (+ (string-length k)
										 (string-length string-v)))
						#:pad-string " ")
					string-v))
   "\n"))

(define (pretty-print/parse-tree pt)
  (string-join
   (for/list ([command pt])
	 (~a command))
   "\n"))
