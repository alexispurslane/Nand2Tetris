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
                     (string-split "/")
                     last
                     (string-split ".")
                     first
                     (string-append ".asm")))

(when (string-suffix? file-or-directory "/")
    (set! out-name (string-append file-or-directory
                                  out-name)))

(write-file out-name (match (string-split file-or-directory ".")
                       [(list vm-code "vm") (generate-assembly
                                             (list
                                              (file->commands file-or-directory))
					     (list vm-code))]
                       [(list dir)
                        (define files (map (λ (f)
                                             (string-append dir
                                                            (path->string f)))
                                           (filter (λ (x) (~> x
                                                              path->string
                                                              (string-split ".")
                                                              second
                                                              (equal? "vm")))
                                                   (directory-list dir))))
                        (generate-assembly
                                    (map file->commands files)
                                    files)]))
