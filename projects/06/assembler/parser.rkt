#lang racket

(provide
 file->commands
 string->command
 
 (struct-out command/a)
 (struct-out command/c)
 (struct-out command/l))

(struct command/a (address) #:transparent)
(struct command/c (dest comp jump) #:transparent)
(struct command/l (symbol address) #:transparent)

(define (file->commands file-name)
  (define-values (_ st cs)
	(for/fold ([wordn 0]
			   [symbol-table (make-hash)]
			   [commands '()])
		([line (file->lines file-name)]
		 #:unless (or (string-prefix? line "//")
					  (equal? line "")))
	  (let ([wordn (+ 1 wordn)]
			[command (string->command (string-trim line)
									  wordn
									  symbol-table)])
		(values wordn (first command)
				(cons (second command) commands)))))
  (reverse cs))

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

(define (string->command str wordn symbol-table)
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

  (define command (match (string->list str)
					[(list #\@ number ...)
					 (command/a
					  (format-binary
					   (let ([n (number->binary-string
								 (string->number (list->string number)))])
						 (if (not n)
							 (number->binary-string
							  (hash-ref! symbol-table
										 (list->string number)
										 (+ 16 (length
												(hash-keys symbol-table)))))
							 n))))]
					
					[(list dest ... #\= comp ... #\; jump ...)
					 (command/c (string->dest (list->string dest))
								(string->operation (list->string comp))
								(string->jump (list->string jump)))]
					
					[(list dest ... #\= comp ...)
					 (command/c (string->dest (list->string dest))
								(string->operation (list->string comp))
								#f)]
					
					[(list #\( label ... #\))
					 (hash-set! symbol-table (list->string label) wordn)
					 (command/l (list->string label)
								(hash-ref symbol-table (list->string label)))]
					
					[(list comp ... #\; jump ...)
					 (command/c #f
								(string->operation (list->string comp))
								(string->jump (list->string jump)))]))
  (list symbol-table command))
