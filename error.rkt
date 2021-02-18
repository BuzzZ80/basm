#lang racket/base

(require racket/format)

(provide raise-error
         error-line-number
         error-file-name)

(define error-line-number (make-parameter #f))
(define error-file-name (make-parameter #f))

(define (raise-error message)
  (raise-user-error (if (error-line-number)
                        (if (error-file-name)
                            (format "~a:~a: ~a"
                                    (error-file-name)
                                    (error-line-number)
                                    message)
                            (format "-:~a: ~a"
                                    (error-line-number)
                                    message))
                        message)))
                     
 