#lang rackjure
(require "parser.rkt"
         "constants.rkt")
(provide generate-assembly)

(define *prev-fname* "")
(define (generate-assembly files-commands names) 
  (string-join (append
                (list (join-line
                       "@256"
                       "D=A"
                       "@SP"
                       "M=D"
                       "@3000"
                       "D=A"
                       "@THIS"
                       "M=D"
                       "@4000"
                       "D=A"
                       "@THAT"
                       "M=D"))
                (list (last (command->assembly (command "call" "Sys.init" "0")
                                               "bootstrap"
                                               '("VM$outer")
                                               0))) 
                (map commands->assembly
                     files-commands
                     (map #λ(last (string-split %1 "/")) names))
                (list (join-line
                       "(END)"
                       "@END"
                       "0;JMP"))) "\n"))

(define (debug x)
  (displayln x)
  x)

(define (commands->assembly commands filen)
  (displayln filen)
  (string-append
   (last (foldl (λ (command instruction-number acc)
                  (match-define (list function-stack assembly) acc)
                  (match-define (list fstack nassem)
                                (command->assembly command filen function-stack
                                                   instruction-number))
                  (list fstack (string-append assembly nassem)))
                (list '("VM$outer") "")
                commands
                (range 0 (length commands))))))

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
   "D=D-M"
   (string-concat "@TRUE" n)
   (string-concat "D;J" type)
   "@SP"
   "A=M"
   "M=0"
   (string-concat "@FALSE" n)
   "D;JMP"
   (string-concat "(TRUE" n ")")
   "@SP"
   "A=M"
   "M=-1"
   (string-concat "(FALSE" n ")")
   incr-stack))

(define (single-op op)
  (join-line
   dncr-stack
   "A=M"
   (string-concat "M=" op "M")
   incr-stack))

(define/match (command->assembly c filen fstack n)
  [((command "push" segment sx) _ _ _)
   (define x (string->number sx))
   (list fstack
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
           ["temp"     (push-to x "5" 'fixed)]
           ["pointer"  (push-to x "3" 'fixed)]
           ["static"   (push-to x "16" 'fixed)]))]
  [((command "pop" segment sx) _ _ _)
   (define x (string->number sx))
   (list fstack
         (match segment
           ["local"    (pop-to x "LCL" 'memory)]
           ["argument" (pop-to x "ARG" 'memory)]
           ["that"     (pop-to x "THAT" 'memory)]
           ["this"     (pop-to x "THIS" 'memory)]
           ["temp"     (pop-to (+ 5 x) "0" 'fixed)]
           ["pointer"  (pop-to (+ 3 x) "0" 'fixed)]
           ["static"   (pop-to (+ 16 x) "0" 'fixed)]))]
  [((command "label" label #f) _ _ _)
   (list fstack (string-concat "(" (string-concat (last fstack) "$" (string-upcase label)) ")\n"))]
  [((command "goto" label #f) _ _ _)
   (list fstack
         (join-line
          (string-concat "@" (string-concat (last fstack) "$" (string-upcase label)))
          "0;JMP"))]
  [((command "if-goto" label #f) _ _ _)
   (list fstack
         (join-line
          "@SP"
          "AM=M-1"
          "D=M"
          (string-concat "@" (string-concat (last fstack) "$" (string-upcase label)))
          "D;JNE"))]
  [((command "add" #f #f) _ _ _) (list fstack (stack-op "D+M"))]
  [((command "sub" #f #f) _ _ _) (list fstack (stack-op "M-D"))]
  [((command "not" #f #f) _ _ _) (list fstack (single-op "!"))]
  [((command "eq" #f #f) _ _ n)  (list fstack (bool-op "EQ" n))]
  [((command "gt" #f #f) _ _ n)  (list fstack (bool-op "LT" n))]
  [((command "lt" #f #f) _ _ n)  (list fstack (bool-op "GT" n))]
  [((command "and" #f #f) _ _ _) (list fstack (stack-op "D&M"))]
  [((command "or" #f #f) _ _ _)  (list fstack (stack-op "D|M"))]
  [((command "neg" #f #f) _ _ _) (list fstack (single-op "-"))]
  [((command "function" f sn) _ _ in)
   (define n (if sn (string->number sn) 0))
   (define newstack (append fstack (list f)))
   
   (list newstack
         (join-line (string-concat "(" f ")")
                    (for/fold ([asm ""])
                              ([x (range 0 n)])
                      (string-append asm
                                     (last (command->assembly (command "push" "constant" "0")
                                                              filen
                                                              newstack (+ 1 x)))))))]
  [((command "call" f sn) _ _ instruction-n)
   (define n (if sn (string->number sn) 0))
   (define (local-compile-command command)
     (match-define (list p1 p2 p3) command)
     (last (command->assembly (command p1 p2 p3)
                              filen fstack (+ 1 n))))
   (define (save-function-var name)
     (join-line
      (string-concat "@" name)
      "D=M"
      "@SP"
      "A=M"
      "M=D"
      incr-stack))
   (define return-address (string-concat (last fstack) "$" "RETURN" instruction-n))
   (list fstack
         (join-line (string-concat "@" return-address)
                    "D=A"
                    "@SP"
                    "A=M"
                    "M=D"
                    incr-stack
                    (save-function-var "LCL")
                    (save-function-var "ARG")
                    (save-function-var "THIS")
                    (save-function-var "THAT")
                    ; ARG = SP - n - 5
                    "@SP"
                    "D=M"
                    (string-concat "@" n)
                    "D=D-A"
                    "@5"
                    "D=D-A"
                    "@ARG"
                    "M=D"
                    "@SP"
                    "D=M"
                    "@LCL"
                    "M=D"
                    (string-concat "@" f)
                    "0;JMP"
                    (string-concat "(" return-address ")")))]
  [((command "return" #f #f) _ _ _)
   (define (local-compile-command command)
     (match-define (list p1 p2 p3) command)
     (last (command->assembly (command p1 p2 p3)
                              filen fstack (+ 1 n))))
   (define (reset-caller-variable distance segment)
     (join-line
      (string-concat "@" distance)
      "D=A"
      "@R5"
      "A=M"
      "A=A-D"
      "D=M"
      (string-concat "@" segment)
      "M=D"))
   (list (butlast fstack)
         (join-line
          "@LCL"
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
          "@R6"
          "A=M"
          "0;JMP"))])
