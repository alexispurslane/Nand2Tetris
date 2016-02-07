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

(define symbols-mode (make-parameter #f))
(define parse-tree-mode (make-parameter #f))

(define file-name
  (command-line
   #:program "assembler"
   #:once-each
   [("-v" "--verbose")
    "Compile with verbose information, including parse tree information and symbol-table info."
    (symbols-mode #t)
    (parse-tree-mode #t)]
   [("-s" "--symbols")
     "Compile with verbose information for only the symbol-table."
     (symbols-mode #t)]
   [("-p" "--parse-tree")
     "Compile with verbose information for only the parse-tree."
     (parse-tree-mode #t)]
   #:args (file-name)
   file-name))

(write-file (string-append file-name ".hack")
            (generate-code (let* ([code (file->commands
                                         (string-append file-name ".asm"))]
                                  [symbol-table (build-symbol-table code)])
                             (if (symbols-mode)
                                 (begin (displayln
                                         "------------------------- SYMBOL TABLE")
                                        (displayln
                                         (pretty-print/symbol-table symbol-table)))
                                 #f)
                             (if (parse-tree-mode)
                                 (begin (displayln
                                         "------------------------- PARSER TREE")
                                        (displayln
                                         (pretty-print/parse-tree code)))
                                 #f)
                             (apply-symbol-table symbol-table code))))
