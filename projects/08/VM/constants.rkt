#lang rackjure

(provide ->string
         join-line
         string-concat
         butlast
         dncr-stack
         incr-stack)

(define (->string x)
  (cond
    [(number? x) (number->string x)]
    [(string? x) x]
    [(list? x) (list->string x)]
    [(hash? x) (list->string (hash->list x))]))

(define (join-line . lines) (~> lines (string-join "\n") (string-append "\n")))
(define (string-concat . z) (~> (map ->string z) (string-join "")))

(define dncr-stack (join-line 
                    "@SP"
                    "M=M-1"))

(define incr-stack (join-line 
                    "@SP"
                    "M=M+1"))

(define butlast (compose reverse cdr reverse))
