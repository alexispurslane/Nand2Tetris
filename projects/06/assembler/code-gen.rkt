#lang racket

#|
PURPOSE: This module generates true binary words from the parse
tree. It's a mostly very simple process of pattern matching.

COMMENTS:
  * I sort of think I should combine this with the parser module if this doesn't
    get any bigger.

BUGS: No known ones.
|#

(require "parser.rkt")
(require "util.rkt")
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
