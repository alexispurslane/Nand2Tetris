#lang racket

(require "parser.rkt")

(provide pretty-print/symbol-table
         pretty-print/parse-tree)

(define (pretty-print/symbol-table st [width 100])
  (string-append (string-join
                  (for/list ([pair (sort (hash->list st) < #:key second)]
                             #:when (or (eq? (third pair) 'VAR) (eq? (third pair) 'CONST)))
                    (match-define (cons k v) pair)
                    (define string-v (number->string (first v)))
                    (string-append k
                                   (~a ""
                                       #:width (- width (+ (string-length k)
                                                           (string-length string-v)))
                                       #:pad-string " ")
                                   string-v
                                   " "
                                   (symbol->string (second v))))
                  "\n")
                 "\n" "\n" "TOTAL VAR COUNT:"
                 (number->string
                  (count (λ (x) (eq? (third x) 'VAR)) (hash->list st)))))

(define (pretty-print/parse-tree pt)
  (string-join
   (for/list ([command pt])
	 (~a command))
   "\n"))
