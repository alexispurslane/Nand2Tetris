#lang racket

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
      (define command (string->command (string-trim (first (string-split line "//"))) wordn))
      (values
       (if (not (command/l? command))
           (+ 1 wordn)
           wordn)
       (cons command commands))))
  (reverse cs))

(define (build-symbol-table commands)
  (define DEFAULTS '("SP"     0
                     "LCL"    1
                     "ARG"    2
                     "THIS"   3
                     "THAT"   4
                     "R0"     0
                     "R1"     1
                     "R2"     2
                     "R3"     3
                     "R4"     4
                     "R5"     5
                     "R6"     6
                     "R7"     7
                     "R8"     8
                     "R9"     9
                     "R10"    10
                     "R11"    11
                     "R12"    12
                     "R13"    13
                     "R14"    14
                     "R15"    15
                     "SCREEN" 16384
                     "KBD"    24576))
  (for/fold ([symbol-table (apply hash (map (λ (x) (if (number? x)
                                                       (list x 'VAR)
                                                       x))
                                            DEFAULTS))])
            ([command commands])
    (match command
      [(command/a number)
       (let ([n (string->number number)]
             [sym (hash-ref symbol-table number #f)])
         (if (not n)
             (if (not sym)
                 (hash-set symbol-table number
                           (list (+ 16 (length
                                        (filter (λ (x) (eq? (second x) 'VAR))
                                                (hash->list symbol-table))))
                                 'VAR))
                 symbol-table)
             symbol-table))]
      [(command/l symbol location)
       (hash-set symbol-table symbol (list location 'SYM))]
      [else symbol-table])))

(define (apply-symbol-table symbol-table commands)
  (for/list ([command commands])
    (match command
      [(command/a number)
       (let ([n (string->number number)]
             [sym (hash-ref symbol-table number #f)])
         (command/a (format-binary (number->binary-string
                                    (if (not n)
                                        (first sym)
                                        n)))))]
      [else command])))


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
  (define string->operation (match-lambda
                              ; a = 0
                              ["0"   "0101010"]
                              ["1"   "0111111"]
                              ["-1"  "0111010"]
                              ["D"   "0001100"]
                              ["A"   "0110000"]
                              ["!D"  "0001101"]
                              ["!A"  "0110001"]
                              ["-D"  "0001111"]
                              ["-A"  "0110011"]
                              ["D+1" "0011111"]
                              ["A+1" "0110111"]
                              ["D-1" "0001110"]
                              ["A-1" "0110010"]
                              ["D+A" "0000010"]
                              ["D-A" "0010011"]
                              ["A-D" "0000111"]
                              ["D&A" "0000000"]
                              ["D|A" "0010101"]
                              ; a = 1
                              ["M"   "1110000"]
                              ["!M"  "1110001"]
                              ["-M"  "1110011"]
                              ["M+1" "1110111"]
                              ["M-1" "1110010"]
                              ["D+M" "1000010"]
                              ["D-M" "1010011"]
                              ["M-D" "1000111"]
                              ["D&M" "1000000"]
                              ["D|M" "1010101"]))
  
  (define string->dest (match-lambda
                         ["M"   "001"]
                         ["D"   "010"]
                         ["MD"  "011"]
                         ["A"   "100"]
                         ["AM"  "101"]
                         ["AD"  "110"]
                         ["AMD" "111"]))
  
  (define string->jump (match-lambda
                         ["JGT" "001"]
                         ["JEQ" "010"]
                         ["JGE" "011"]
                         ["JLT" "100"]
                         ["JNE" "101"]
                         ["JLE" "110"]
                         ["JMP" "111"]))

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
