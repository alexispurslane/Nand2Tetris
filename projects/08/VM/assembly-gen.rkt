#lang rackjure
;; 07
(require "parser.rkt"
         "constants.rkt")
(provide generate-assembly)

(define (generate-assembly files-commands names) 
  (string-join (append (list (join-line "@256"
                                        "D=A"
                                        "@SP"
                                        "M=D"))
                       (map commands->assembly
                            (append (list (list
                                           (command "call" "Sys.init" #f)))
                                    files-commands)
                            (append (list "bootstrap")
                                    (map #λ(last (string-split %1 "/")) names)))) "\n"))

(define (commands->assembly commands filen)
  (string-append
   (foldl (λ (command instruction-number acc)
            (match-define (list function-name assembly) acc)
            (string-append assembly (command->assembly command
                                                       filen
                                                       function-name
                                                       instruction-number)))
          (list "VM$outer" "")
          commands
          (range 0 (length commands)))
   (join-line
    "(END)"
    "@END"
    "0;JMP")))

(define (push-to x base type)
  (cond
    [(eq? type 'memory)
     (join-line
      (string-concat "@" x)
      "D=A"
      (string-concat "@" base)
      "A=D+M"
      "D=M"
      "@SP"
      "A=M"
      "M=D"
      incr-stack)]
    [(eq? type 'fixed)
     (join-line
      (string-concat "@" x)
      "D=A"
      (string-concat "@" base)
      "A=D+A"
      "D=M"
      "@SP"
      "A=M"
      "M=D"
      incr-stack)]))

(define (pop-to x base type)
  (cond
    [(eq? type 'memory)
     (join-line
      (string-concat "@" x)
      "D=A"
      (string-append "@" base)
      "AD=D+M"
      "@R13"
      "M=D"
      "@SP"
      "AM=M-1"
      "D=M"
      "@R13"
      "A=M"
      "M=D")]
    [(eq? type 'fixed)
     (join-line
      (string-concat "@" x)
      "D=A"
      (string-append "@" base)
      "AD=D+A"
      "@R13"
      "M=D"
      "@SP"
      "AM=M-1"
      "D=M"
      "@R13"
      "A=M"
      "M=D")]))

(define (stack-op op)
  (join-line
   "@SP"
   "AM=M-1"
   "D=M"
   "@SP"
   "AM=M-1"
   (string-concat "M=" op)
   incr-stack))

(define (bool-op type n)
  (join-line
   dncr-stack
   "A=M"
   "D=M"
   dncr-stack
   "A=M"
   "M=M-D"
   "D=M"
   (string-concat "@TRUE" n)
   (string-concat "D;J" (string-upcase type))
   (string-concat "@FALSE" n)
   "0;JMP"
   (string-concat "(" "TRUE" n ")")
   "@SP"
   "A=M"
   "M=-1"
   (string-concat "@NEXT" n)
   "0;JMP"
   (string-concat "(" "FALSE" n ")")
   "@SP"
   "A=M"
   "M=0"
   (string-concat "(" "NEXT" n ")")
   incr-stack))

(define (single-op op)
  (join-line
   dncr-stack
   "A=M"
   (string-concat "M=" op "M")
   incr-stack))

(define/match (command->assembly c filen cfname n)
  [((command "push" segment sx) _ _ _)
   (define x (string->number sx))
   (list cfname
         (match segment
           ["constant" (join-line
                        (string-concat "@" x)
                        "D=A"
                        "@SP"
                        "A=M"
                        "M=D"
                        incr-stack)]
           ["local"    (push-to x "LCL" 'memory)]
           ["argument" (push-to x "ARG" 'memory)]
           ["this"     (push-to x "THIS" 'memory)]
           ["that"     (push-to x "THAT" 'memory)]
           ["temp"     (push-to (+ 5 x) "0" 'fixed)]
           ["pointer"  (push-to (+ 3 x) "0" 'fixed)]
           ["static"   (push-to (+ 16 x) "0" 'fixed)]))]
  [((command "pop" segment sx) _ _ _)
   (define x (string->number sx))
   (list cfname
         (match segment
           ["local"    (pop-to x "LCL" 'memory)]
           ["argument" (pop-to x "ARG" 'memory)]
           ["that"     (pop-to x "THAT" 'memory)]
           ["this"     (pop-to x "THIS" 'memory)]
           ["temp"     (pop-to (+ 5 x) "0" 'fixed)]
           ["pointer"  (pop-to (+ 3 x) "0" 'fixed)]
           ["static"   (pop-to (+ 16 x) "0" 'fixed)]))]
  [((command "label" label #f) _ _ _)
   (list cfname (string-concat "(" (string-concat cfname "$" (string-upcase label)) ")\n"))]
  [((command "goto" label #f) _ _ _)
   (list cfname
         (join-line
          (string-concat "@" (string-concat cfname "$" (string-upcase label)))
          "0;JMP"))]
  [((command "if-goto" label #f) _ _ _)
   (list cfname
         (join-line
          "@SP"
          "AM=M-1"
          "D=M"
          (string-concat "@" (string-concat cfname "$" (string-upcase label)))
          "D;JNE"))]
  [((command "add" #f #f) _ _ _) (list cfname (stack-op "D+M"))]
  [((command "sub" #f #f) _ _ _) (list cfname (stack-op "M-D"))]
  [((command "not" #f #f) _ _ _) (list cfname (single-op "!"))]
  [((command "eq" #f #f) _ n _)  (list cfname (bool-op "eq" n))]
  [((command "gt" #f #f) _ n _)  (list cfname (bool-op "gt" n))]
  [((command "lt" #f #f) _ n _)  (list cfname (bool-op "lt" n))]
  [((command "and" #f #f) _ _ _) (list cfname (stack-op "D&M"))]
  [((command "or" #f #f) _ _ _)  (list cfname (stack-op "D|M"))]
  [((command "neg" #f #f) _ _ _) (list cfname (single-op "-"))]
  [((command "function" f n) _ _ _)
   (define full-f (string-concat filen "." f ))
   (list full-f
         (join-line (string-concat "(" full-f ")")
                    (string-concat "@" n)
                    "D=A"
                    "@FRAME"
                    "M=D"
                    (string-concat "(LOOP" n ")")
                    (last (command->assembly (command "push" "constant" "0")
                                             filen cfname (+ 1 n)))
                    "@FRAME"
                    "DM=M-1"
                    "@LOOP"
                    "D;JGT"))]
  [((command "call" f n) _ _ _)
   (define (local-compile-command command)
     (match-define (list p1 p2 p3) command)
     (last (command->assembly (command p1 p2 p3)
                              filen cfname (+ 1 n))))
   (define return-address (string-concat filn "." cfname "$" "RETURN" n))
   (list cfname
         (join-line (string-concat "@" return-address)
                    "D=A"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    "@LCL"
                    "D=M"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    "@ARG"
                    "D=M"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    "@THIS"
                    "D=M"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    "@THIS"
                    "D=M"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    ; ARG = SP - n - 5
                    "@SP"
                    "D=M"
                    "D=D-1"
                    "D=D-1"
                    "D=D-1"
                    "D=D-1"
                    "D=D-1"
                    (for/fold ([assembly ""])
                              ([x (range 0 n)])
                      (string-concat assembly "\n"
                                     "D=D-1"))
                    "@ARG"
                    "M=D"
                    "@SP"
                    "D=M"
                    "@LCL"
                    "M=D"
                    (string-concat "0;JMP " f)
                    (string-concat "(" return-address ")")))]
  [((command "return" #f #f) _ _ _)
   (define (local-compile-command command)
     (match-define (list p1 p2 p3) command)
     (last (command->assembly (command p1 p2 p3)
                              filen cfname (+ 1 n))))
   (define (reset-caller-variable distance segment)
     (join-line
      (string-concat "@" frameDistance)
      "D=A"
      "@R5"
      "A=M"
      "A=A-D"
      "D=M"
      (string-concat "@" segment)
      "M=D"))
   (join-line
    "@LCL" ; FRAME = LCL
    "D=M"
    "@R5"
    "M=D"
    (reset-caller-variable "5" "R6")
    dncr-stack
    "@SP"
    "A=M"
    "D=M"
    "@ARG"
    "A=M"
    "M=D"
    "@ARG"
    "D=M+1"
    "@SP"
    "M=D"
    (reset-caller-variable "1" "THAT")
    (reset-caller-variable "2" "THIS")
    (reset-caller-variable "3" "ARG")
    (reset-caller-variable "4" "LCL")
    "@R5"
    "0;JMP")])
