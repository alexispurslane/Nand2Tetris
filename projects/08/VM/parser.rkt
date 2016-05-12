#lang rackjure

(provide file->commands
         make-command
         (struct-out command))

(define (split-by lst x [pred? equal?])
  (foldr (λ (element next)
           (if (pred? element x)
               (cons empty next)
               (cons (cons element (first next)) (rest next))))
         (list empty) lst))

(struct command (name seg arg)
  #:transparent)

(define (make-command nam [seg #f] [arg #f])
  (command nam seg arg))

(define (file->commands file-name)
  (for/list ([line (map (compose string-split string-trim)
                        (file->lines file-name))]
             #:unless (or (equal? line '())
                          (equal? (first line) "//")))
    (~> line
        (split-by "//")
        first
        line->command)))

(define/match (line->command line)
  [((list c))             (make-command c)]
  [((list c arg))         (make-command c arg)]
  [((list c segment arg)) (make-command c segment arg)])
