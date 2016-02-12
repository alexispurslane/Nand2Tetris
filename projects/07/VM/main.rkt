#lang racket

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

(define out-name (string-append (first (string-split file-or-directory "."))
                                ".asm"))
(write-file out-name (match (string-split file-or-directory ".")
                       [(list vm-code "vm") (generate-assembly
                                             (list
                                              (file->commands file-or-directory)) vm-code)]
                       [(list dir) (generate-assembly
                                    (map (compose file->commands path->string)
                                         (directory-list dir))
                                    (map path->string (directory-list dir)))]))
