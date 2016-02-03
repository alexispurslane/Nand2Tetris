#lang racket

(require "parser.rkt")
(provide generate-code)

(define (generate-code lst)
  (string-join
   (for/list ([command lst]
			  #:unless (command/l? command))
			 (match command
			   [(command/a r)       r]
			   [(command/c #f c #f) (string-append "111" c "000" "000")]
			   [(command/c #f c j)  (string-append "111" c "000" j)]
			   [(command/c d  c #f) (string-append "111" c d "000")]
			   [(command/c d  c j)  (string-append "111" c d j)]))
   "\n"))
