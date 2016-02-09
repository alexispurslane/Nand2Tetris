#lang racket

(provide file->commands
         make-command
         (struct-out command))

(struct command (name seg arg)
  #:transparent)

(define (make-command nam [seg #f] [arg #f])
  (command nam seg arg))

(define (file->commands file-name)
  (for/list ([line (map (compose string-split string-trim)
                        (file->lines file-name))]
             #:unless (or (equal? line '())
                          (equal? (first line) "//")))
    (line->command line)))

(define/match (line->command line)
  [((list c))             (make-command c)]
  [((list c arg))         (make-command c arg)]
  [((list c segment arg)) (make-command c segment arg)])
