#lang racket

#|
PURPOSE: This module parses a Hack Assembly Language file in three steps:
1) It generates a basic parse tree of the commands with minimal metadata
(file->commands)
2) It passes once over the generated parse tree (really a list) and generates
a symbol table, where symbols are mapped to their location in memory as well
as information as to wether they are variable registers or jump locations.
(build-symbol-table)
3) The final parse tree is created when we apply the symbol table to the parse
tree, so that the known memory locations are used instead of symbols.
(apply-symbol-table)

COMMENTS:
* This multi-pass thing seems excessive...
* Some of this code probably isn't as concise as it probably could be.
* I'm wondering wether the code generation should be here too? It's tiny.

BUGS:
* The variable register counting doesn't seem to increment,
so all variables seem to end up in the same place.

|#

(require "util.rkt")

(provide
 file->commands
 string->command
 build-symbol-table
 apply-symbol-table
 
 (struct-out command/a)
 (struct-out command/c)
 (struct-out command/l))


(struct command/a (address) #:transparent)
(struct command/c (dest comp jump) #:transparent)
(struct command/l (symbol address) #:transparent)

(define (file->commands file-name)
  (define-values (_ cs)
    (for/fold
        ([wordn 0]
         [commands '()])
        ([line (file->lines file-name)]
         #:unless (or (string-prefix? line "//")
                      (equal? line "")))
      (define command (string->command
                       (string-trim (first (string-split line "//")))
                       wordn))
      (values (if (not (command/l? command))
                  (+ 1 wordn)
                  wordn)
              (cons command commands))))
  (reverse cs))

(define (build-symbol-table commands) 
  (for/fold ([symbol-table (apply hash DEFAULTS)])
            ([command commands])
    (match command
      [(command/l symbol location)
       (hash-set symbol-table symbol location)]
      [else symbol-table])))

(define (apply-symbol-table init-symbol-table commands)
  (define-values (v st cs)
    (for/fold
        ([varn 16]
         [symbol-table init-symbol-table]
         [commands '()])
        ([command commands])
      (match command
        [(command/a number)
         (let ([num (or (string->number number)
                        (hash-ref symbol-table number #f))])
           (cond
             [num (values varn
                          symbol-table
                          (append commands
                                  (list (command/a (format-binary
                                                    (number->binary-string num))))))]
             [else (values (+ varn 1)
                           (hash-set symbol-table number varn)
                           (append commands
                                   (list (command/a (format-binary
                                                     (number->binary-string varn))))))]))]
        [else (values varn
                      symbol-table
                      (append commands (list command)))])))
  cs)


(define (number->binary-string n)
  (cond [(not n) #f]
        [(< n 2) (number->string n)]
        [else (string-append (number->binary-string (quotient n 2))
                             (number->string (remainder n 2)))]))

(define (format-binary str [width 16])
  (cond [(not str) #f]
        [(~a str
             #:width 16
             #:pad-string "0"
             #:align 'right)]))

(define (string->command str wordn) 
  (match (string->list str)
    [(list #\@ number ...)
     (command/a (list->string number))]
    [(list dest ... #\= comp ... #\; jump ...)
     (command/c (string->dest (list->string dest))
                (string->operation (list->string comp))
                (string->jump (list->string jump)))]
    [(list dest ... #\= comp ...)
     (command/c (string->dest (list->string dest))
                (string->operation (list->string comp))
                #f)]
    [(list #\( label ... #\))
     (command/l (list->string label)
                wordn)]
    [(list comp ... #\; jump ...)
     (command/c #f
                (string->operation (list->string comp))
                (string->jump (list->string jump)))]))
