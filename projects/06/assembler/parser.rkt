#lang racket

(provide
 file->commands
 string->command
 command-type
 (struct-out commands)
 
 (struct-out command/a)
 (struct-out command/c)
 (struct-out command/l))

(struct command/a (address symbol) #:transparent)
(struct command/c (dest comp jump) #:transparent)
(struct command/l (symbol) #:transparent)

(struct commands ([current-command #:mutable] command-list))

(define (command-type c)
  (match c
	[(command/a _ _) 'A]
	[(command/c _ _ _) 'C]
	[(command/l _) 'L]
	[else (error "unknown command type")]))

(define (file->commands file-name)
  (commands #f (for/list ([line (file->lines file-name)]
						  #:unless (or (string-prefix? line "//")
									   (equal? line "")))
				 (string->command (string-trim line)))))

(define (string->command str)
  (match (string->list str)
	[(list #\@ number ...) (command/a
							(string->number (list->string number))
							(list->string number))]))
