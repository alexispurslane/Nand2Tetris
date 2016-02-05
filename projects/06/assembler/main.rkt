#lang racket

#|
PURPOSE: This module is a command line interface for the assembler.
It sort of brings everything together.
|#

(require "parser.rkt"
		 "code-gen.rkt"
		 "pretty-print-custom.rkt"
		 racket/cmdline
		 2htdp/batch-io)

(define verbose-mode (make-parameter #f))

(define file-name
  (command-line
   #:program "assembler"
   #:once-each
   [("-v" "--verbose")
	"Compile with verbose information, including parse tree information and symbol-table info."
	(verbose-mode #t)]
   #:args (file-name)
   file-name))

(write-file (string-append file-name ".hack")
			(generate-code (let* ([code (file->commands
										 (string-append file-name ".asm"))]
								  [symbol-table (build-symbol-table code)])
							 (if (verbose-mode)
								 (begin (displayln
										 "------------------------- SYMBOL TABLE")
										(displayln
										 (pretty-print/symbol-table symbol-table))
										(displayln
										 "------------------------- PARSER TREE")
										(displayln
										 (pretty-print/parse-tree code)))
								 #f)
							 (apply-symbol-table symbol-table code))))
