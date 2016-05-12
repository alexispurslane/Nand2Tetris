#lang rackjure

(require "parser.rkt"
         "assembly-gen.rkt"
         racket/cmdline
         2htdp/batch-io)

(define symbols-mode (make-parameter #f))
(define parse-tree-mode (make-parameter #f))

(define file-or-directory
  (command-line
   #:program "vmtranslator"
   #:args (file-or-directory)
   file-or-directory))

(define out-name (~> file-or-directory
                     (string-split ".")
                     first
                     (string-append ".asm")))

(write-file out-name (match (string-split file-or-directory ".")
                       [(list vm-code "vm") (generate-assembly
                                             (list
                                              (file->commands file-or-directory))
					     (list vm-code))]
                       [(list dir) (generate-assembly
                                    (map #λ{~> % path->string file->commands}
                                         (directory-list dir))
                                    (map path->string (directory-list dir)))]))
