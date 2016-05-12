#lang rackjure
;; 07
(require "parser.rkt"
         "constants.rkt")
(provide generate-assembly)

(define (generate-assembly files-commands names) 
  (string-join (map commands->assembly files-commands names) "\n"))

(define (commands->assembly commands filen)
  (string-append
   (foldl #λ(~> %3 (string-append (command->assembly %1 filen %2)))
          ""
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

(define/match (command->assembly c filen n)
  [((command "push" segment sx) _ _)
   (define x (string->number sx))
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
     ["static" (join-line
                (string-concat "@" filen "." x)
                "D=M"
                "@SP"
                "A=M"
                "M=D"
                incr-stack)])]
  [((command "pop" segment sx) _ _)
   (define x (string->number sx))
   (match segment
     ["local"    (pop-to x "LCL" 'memory)]
     ["argument" (pop-to x "ARG" 'memory)]
     ["that"     (pop-to x "THAT" 'memory)]
     ["this"     (pop-to x "THIS" 'memory)]
     ["temp"     (pop-to (+ 5 x) "0" 'fixed)]
     ["pointer"  (pop-to (+ 3 x) "0" 'fixed)]
     ["static"   (join-line
                  "@SP"
                  "A=M"
                  "D=M"
                  (string-concat "@" filen "." x)
                  "A=D"
                  dncr-stack)])]
  [((command "label" label #f) _ _)
   (string-concat "(" (string-upcase label) ")\n")]
  [((command "goto" label #f) _ _)
   (join-line
    (string-concat "@" (string-upcase label))
    "0;JMP")]
  [((command "if-goto" label #f) _ _)
   (join-line
    "@SP"
    "AM=M-1"
    "D=M"
    (string-concat "@" (string-upcase label))
    "D;JNE")]
  [((command "add" #f #f) _ _) (stack-op "D+M")]
  [((command "sub" #f #f) _ _) (stack-op "M-D")]
  [((command "not" #f #f) _ _) (single-op "!")]
  [((command "eq" #f #f) _ n)  (bool-op "eq" n)]
  [((command "gt" #f #f) _ n)  (bool-op "gt" n)]
  [((command "lt" #f #f) _ n)  (bool-op "lt" n)]
  [((command "and" #f #f) _ _) (stack-op "D&M")]
  [((command "or" #f #f) _ _)  (stack-op "D|M")]
  [((command "neg" #f #f) _ _) (single-op "-")])
