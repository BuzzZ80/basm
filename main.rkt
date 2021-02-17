#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/format
           racket/match
           racket/string
           racket/port
           racket/bytes
           "codegen.rkt")

  #;("mov ac, ((0o420 + ix) + ix)"
     "my_string:"
     "db 12, \"hello world\\0\""
     "const foo = 3"
     "const bar = foo"
     "mov (0x1), (0x2)"
     "mov ac, ac"
     "myLabel:"
     "mov ix, bar   ; load my constant"
     "jmp myLabel -NcZ")

  #;("const foo = 3"
     "start:"
     "mov ac, foo"
     "jmp start")



  (require racket/cmdline)
  (begin 
    (define output-mode (make-parameter 'text))
    (define output-path (make-parameter "-"))
    (define input-path (make-parameter "-"))
    (command-line
     #:program "basm"
     #:once-each
     [("-o" "--output") filepath "File to output to"
                        (output-path filepath)]
     #:once-any
     [("-t" "--text") "Output code in text format (one word per line in octal)"
                      (output-mode 'text)]
     [("-b" "--binary") "Output code in binary format (each word as a big-endian 16-bit integer)"
                        (output-mode 'binary)]
     #:args ([input-path-arg "-"])
     (input-path input-path-arg))

    (current-output-port
     (match (output-path)
       ["-" (current-output-port)]
       [filepath (open-output-file filepath #:mode (output-mode) #:exists 'truncate/replace)]))

    (current-input-port
     (match (input-path)
       ["-" (current-input-port)]
       [filepath (open-input-file filepath #:mode 'text)]))
        
    (define program (port->lines #:close? #true))
    
    (define bitcode (assemble program))

    (define length-written (case (output-mode)
      [(binary)
       (let ([buffer (make-bytes (* 2 (length bitcode)))])
         (for ([word bitcode]
               [index (in-naturals)])
           (integer->integer-bytes word
                                   2
                                   #false
                                   #true
                                   buffer
                                   (* 2 index)))
         (write-bytes buffer))]
      [(text)
       (write-string (string-join (map (λ (word) (~a (number->string word 8) #:width 4 #:align 'right #:pad-string "0"))
                                       bitcode)
                                  "\n" #:after-last "\n"))]))
    
    (close-output-port (current-output-port))))
  
  
