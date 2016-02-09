#lang racket

(require "parser.rkt")
(provide generate-assembly)

(define (generate-assembly files-commands)
  (string-join (map commands->assembly files-commands) "\n"))

(define (commands->assembly commands)
  (for/fold ([assembly "@256\nD=A\n@SP\nM=D\n"])
            ([c commands])
    (string-append assembly
                   (command->assembly c))))

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

(define/match (command->assembly c)
  [((command "push" "constant" x))
   (join-line
    (string-concat "@" x)
    "D=A"
    "@SP"
    "A=M"
    "M=D"
    "@SP"
    "M=M+1")]
  [((command "add" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D+M")]
  [((command "sub" #f #f))
   (join-line
    "@SP"
    "M=M-1" ; SP = SP - 1
    "A=M"
    "D=M"   ; D = stack[SP]
    "@SP"
    "A=M"
    "M=D-M")]
  [((command "not" #f #f))
   (join-line
    "@SP"
    "A=M"
    "M=!M")]
  [((command "eq" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D-M"
    "@TRUE"
    "M;JEQ"
    "@FALSE"
    "0;JMP"
    "(TRUE)"
    "@SP"
    "A=M"
    "M=-1"
    "(FALSE)"
    "@SP"
    "A=M"
    "M=0")]
  [((command "gt" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D-M"
    "@TRUE"
    "M;JGT"
    "@FALSE"
    "0;JMP"
    "(TRUE)"
    "@SP"
    "A=M"
    "M=-1"
    "(FALSE)"
    "@SP"
    "A=M"
    "M=0")]
  [((command "lt" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D-M"
    "@TRUE"
    "M;JLT"
    "@FALSE"
    "0;JMP"
    "(TRUE)"
    "@SP"
    "A=M"
    "M=-1"
    "(FALSE)"
    "@SP"
    "A=M"
    "M=0")]
  [((command "and" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D&M")]
  [((command "or" #f #f))
   (join-line
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "A=M"
    "M=D|M")]
  [((command "neg" #f #f))
   (join-line
    "@SP"
    "A=M"
    "M=-M")])
