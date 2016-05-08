#lang racket

(require "parser.rkt")
(provide generate-assembly)

(define (generate-assembly files-commands names)
  (string-join (for/list ([commands files-commands]
                          [name     names])
                 (commands->assembly commands name)) "\n"))

(define (values-first x y) x)

(define (commands->assembly commands filen)
  (string-append
   (call-with-values (λ ()
                       (for/fold ([assembly (join-line "@256"
                                                       "D=A"
                                                       "@SP"
                                                       "M=D"
                                                       "@2048"
                                                       "D=A"
                                                       "@THIS"
                                                       "M=D"
                                                       "@2049"
                                                       "D=A"
                                                       "@THAT"
                                                       "M=D")]
                                  [n 0])
                                 ([c commands])
                         (values (string-append assembly
                                                (command->assembly c filen n))
                                 (+ n 1)))) values-first)
   (join-line
    "(END)"
    "@END"
    "0;JMP")))

(define (->string x)
  (cond
    [(number? x) (number->string x)]
    [(string? x) x]
    [(list? x) (list->string x)]
    [(hash? x) (list->string (hash->list x))]))

(define (string-concat . z)
  (string-join (map ->string z) ""))

(define (join-line . lines)
  (string-append (string-join lines "\n") "\n"))

(define incr-stack (join-line 
                    "@SP"
                    "M=M+1"))

(define (push-to x base)
  (join-line
   (string-concat "@" x)
   "D=A"
   (string-concat "@" base)
   "A=D+A"
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   incr-stack))

(define dncr-stack (join-line 
                    "@SP"
                    "M=M-1"))

(define (pop-to x base)
  (join-line
   (string-concat "@" x)
   "D=A"
   (string-concat "@" base)
   "D=D+A"
   "@R1"
   "M=D"
   "@SP"
   "A=M"
   "D=M"
   "@R1"
   "A=M"
   "M=D"
   dncr-stack))

(define (stack-op op)
  (join-line
   "@SP"
   "AM=M-1"
   "D=M"
   "@SP"
   "AM=M-1"
   (string-concat "M=D" op "M")
   "@SP"
   "M=M+1"))

(define (bool-op type n)
  (join-line
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@SP"
   "M=M-1"
   "A=M"
   "M=D-M"
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
   (string-concat "(" "NEXT" n ")")
   "@SP"
   "M=M+1"))

(define (single-op op)
  (join-line
   "@SP"
   "M=M-1"
   "A=M"
   (string-concat "M=" op "M")
   "@SP"
   "M=M+1"))

(define/match (command->assembly c filen n)
  [((command "push" segment x) _ _)
   (match segment
     ["constant" (join-line
                  (string-concat "@" x)
                  "D=A"
                  "@SP"
                  "A=M"
                  "M=D"
                  incr-stack)]
     ["local"    (push-to x "LCL")]
     ["argument" (push-to x "ARG")]
     ["this"     (push-to x "THIS")]
     ["that"     (push-to x "THAT")]
     ["temp"     (push-to x "5")]
     ["pointer"  (push-to x "3")]
     ["static" (join-line
                (string-concat "@" filen "." x)
                "D=M"
                "@SP"
                "A=M"
                "M=D"
                incr-stack)])]
  [((command "pop" segment x) _ _)
   (match segment
     ["local"    (pop-to x "LCL")]
     ["argument" (pop-to x "ARG")]
     ["that"     (pop-to x "THAT")]
     ["this"     (pop-to x "THIS")]
     ["temp"     (pop-to x "5")]
     ["pointer"  (pop-to x "3")]
     ["static"   (join-line
                  "@SP"
                  "A=M"
                  "D=M"
                  (string-concat "@" filen "." x)
                  "A=D"
                  dncr-stack)])]
  [((command "add" #f #f) _ _) (stack-op "+")]
  [((command "sub" #f #f) _ _) (stack-op "-")]
  [((command "not" #f #f) _ _) (single-op "!")]
  [((command "eq" #f #f) _ n)  (bool-op "eq" n)]
  [((command "gt" #f #f) _ n)  (bool-op "lt" n)]
  [((command "lt" #f #f) _ n)  (bool-op "gt" n)]
  [((command "and" #f #f) _ _) (stack-op "&")]
  [((command "or" #f #f) _ _)  (stack-op "|")]
  [((command "neg" #f #f) _ _) (single-op "-")])
