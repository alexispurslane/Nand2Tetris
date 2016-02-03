#lang racket

(require "parser.rkt")
(provide generate-code)

(define (generate-code lst)
  (string-join
   (for/list ([command lst])
	 (match command
	   [(command/a r)       r]
	   [(command/c #f c #f) (string-append "111" "000" c "000")]
	   [(command/c #f c j)  (string-append "111" "000" c j)]
	   [(command/c d  c #f) (string-append "111" d c "000")]
	   [(command/c d  c j)  (string-append "111" d c j)]))
   "\n"))
