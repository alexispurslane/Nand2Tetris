#lang racket

(require "parser.rkt"
		 "code-gen.rkt"
		 racket/cmdline
		 2htdp/batch-io)


(define file-name
  (command-line
   #:program "assembler"
   #:args (file-name)
   file-name))

(write-file (string-append file-name ".hack")
			(generate-code (file->commands
							(string-append file-name ".asm"))))
