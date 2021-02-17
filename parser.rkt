#lang racket/base

(require racket/match
         racket/symbol
         racket/port
         racket/list
         racket/generator)

(module+ test
  (require rackunit))

(provide parse-string
         (struct-out identifier)
         (struct-out int-literal)
         (struct-out expression)
         (struct-out string-literal)
         (struct-out db-expression)
         (struct-out binary-instruction)
         (struct-out unary-instruction)
         (struct-out implied-instruction)
         (struct-out direct-register)
         (struct-out reference)
         (struct-out reference-indexed)
         (struct-out operand)
         (struct-out immediate)
         (struct-out stack)
         (struct-out condition)
         (struct-out operation)
         (struct-out constant)
         (struct-out label)
         (struct-out db-directive)
         (struct-out empty-line))

(define (split-list lst sep-pred)
  (define split-pred (λ (i) (not (sep-pred i))))
  (for/list ([items (in-generator
                     (let loop ([l lst])
                       (if (null? l)
                           '()
                           (let-values ([(items rest) (splitf-at l split-pred)])
                             (yield items)
                             (loop (if (pair? rest)
                                       (cdr rest)
                                       '()))))))])
    items))

(define-match-expander list-split
  (syntax-rules ()
    [(_ sep-pred bind-pat)
     (app (λ (l) (split-list l sep-pred))
          bind-pat)]))

(struct int-literal-token (digits radix) #:prefab)
(struct string-literal-token (data) #:prefab)
(struct whitespace-token (text) #:prefab)
(struct identifier-token (name) #:prefab)
(struct implied-instruction-token (inst) #:prefab)
(struct unary-instruction-token (inst) #:prefab)
(struct binary-instruction-token (inst) #:prefab)
(struct register-token (name) #:prefab)
(struct const-token () #:prefab)
(struct db-token () #:prefab)
(struct punctuation-token (kind) #:prefab)
(struct condition-token (kind) #:prefab)

(define (scan str)
  (define implied-instrs '(hlt ret clc clz sec sez))
  (define unary-instrs '(add adc sbw swb nnd and aib anb bia bna ora nor jmp jsr dec inc xor xnr))
  (define binary-instrs '(mov sub sbb cmp))
  (define registers '(ac br ix sp stack))
  
  (define (cut prefix)
    (substring str (string-length prefix)))
  
  (match str
    [""
     '()]
    [(regexp #rx"^;.*" (list _))
     '()]
    [(regexp #rx"^([a-zA-Z_][a-zA-Z0-9_]*)" (list _ identifier))
     (begin
       (define (check-list keywords wrapper)
         (for/first ([keyword keywords]
                     #:when (string-ci=? identifier (symbol->immutable-string keyword)))
           (wrapper keyword)))

       (cons (or (check-list implied-instrs implied-instruction-token)
                 (check-list unary-instrs unary-instruction-token)
                 (check-list binary-instrs binary-instruction-token)
                 (check-list registers register-token)
                 (and (string-ci=? identifier "const") (const-token))
                 (and (string-ci=? identifier "db") (db-token))
                 (identifier-token identifier))
             (scan (cut identifier))))]
    [(regexp #rx"^0[bB]([01]+)" (list lit digits))
     (cons (int-literal-token digits 2)
           (scan (cut lit)))]
    [(regexp #rx"^0[oO]([0-7]+)" (list lit digits))
     (cons (int-literal-token digits 8)
           (scan (cut lit)))]
    [(regexp #rx"^0[xX]([0-9a-fA-F]+)" (list lit digits))
     (cons (int-literal-token digits 16)
           (scan (cut lit)))]
    [(regexp #rx"^([0-9]+)" (list lit digits))
     (cons (int-literal-token digits 10)
           (scan (cut lit)))]
    [(regexp #rx"^([\\(\\),:\\=\\+])" (list _ mark))
     (cons (match mark
             ["," (punctuation-token 'comma)]
             [":" (punctuation-token 'colon)]
             ["(" (punctuation-token 'open-paren)]
             [")" (punctuation-token 'close-paren)]
             ["=" (punctuation-token 'equal)]
             ["+" (punctuation-token 'plus)])
           (scan (cut mark)))]
    [(regexp #rx"^\"(([^\\\\\"]|\\\\.)*)\"" (list base contents _))
     (define char-escapes '((#\\ . #\\)
                            (#\" . #\")
                            (#\0 . #\null)
                            (#\t . #\tab)
                            (#\r . #\return)
                            (#\n . #\newline)))
     (define (transform-string chars)
       (match chars
         ['() '()]
         [(list* #\\ (? char? c) rest)
          (cons (cdr (assoc c char-escapes)) (transform-string rest))]
         [(list* (? char? c) rest)
          (cons c (transform-string rest))]))
     
     (cons (string-literal-token (transform-string (string->list contents)))
           (scan (cut base)))]
    [(regexp #rx"^-([nN]?[cC]?[zZ]?)" (list _ kind))
     (if (string=? "" kind)
         #false
         (condition-token (string->symbol (string-foldcase kind))))]
    [(regexp #rx"^([ \t]+)" (list _ text))
     #;(cons (whitespace-token text)
             (scan (cut text)))
     (scan (cut text))]))

(define-syntax define-app-pattern
  (syntax-rules ()
    [(_ id app-proc)
     (define-match-expander id
       (syntax-rules ()
         [(_ pat)
          (? app-proc (app app-proc pat))]))]))

(struct identifier (name) #:prefab)
(define-match-expander ~@ident
  (syntax-rules ()
    [(_ pat)
     (identifier-token (app identifier pat))]))

(struct int-literal (value) #:prefab)
(define-app-pattern ~@int-literal
  (match-lambda
    [(int-literal-token digits radix)
     (int-literal (string->number digits radix))]
    [_ #false]))

(struct expression (value) #:prefab)
(define-match-expander ~@expr
  (syntax-rules ()
    [(_ pat)
     (or (~@int-literal (app expression pat))
         (~@ident (app expression pat)))]))

(struct string-literal (value) #:prefab)
(define-match-expander ~@string-literal
  (syntax-rules ()
    [(_ pat)
     (string-literal-token (app string-literal pat))]))

(struct db-expression (value) #:prefab)
(define-match-expander ~@db-expr
  (syntax-rules ()
    [(_ pat)
     (or (~@string-literal (app db-expression pat))
         (~@expr (app db-expression pat)))]))

(struct binary-instruction (inst) #:prefab)
(define-match-expander ~@bin-inst
  (syntax-rules ()
    [(_ pat)
     (binary-instruction-token (app binary-instruction pat))]))

(struct unary-instruction (inst) #:prefab)
(define-match-expander ~@unary-inst
  (syntax-rules ()
    [(_ pat)
     (unary-instruction-token (app unary-instruction pat))]))

(struct implied-instruction (inst) #:prefab)
(define-match-expander ~@implied-inst
  (syntax-rules ()
    [(_ pat)
     (implied-instruction-token (app implied-instruction pat))]))

(struct direct-register (reg) #:prefab)
(define-app-pattern ~@direct-reg
  (match-lambda
    [(register-token 'ac)
     (direct-register 'ac)]
    [(register-token 'br)
     (direct-register 'br)]
    [(register-token 'ix)
     (direct-register 'ix)]
    [(register-token 'sp)
     (direct-register 'sp)]
    [_ #false]))

(struct reference (imm) #:prefab)
(define-app-pattern @reference
  (match-lambda
    [(list (punctuation-token 'open-paren) in ___ (punctuation-token 'close-paren))
     in]
    [_ #false]))

(struct reference-indexed (imm) #:prefab)
(define-app-pattern @reference-indexed
  (match-lambda
    [(list (punctuation-token 'open-paren)
           in ___
           (punctuation-token 'plus)
           (register-token 'ix)
           (punctuation-token 'close-paren))
     in]
    [_ #false]))

(struct operand (reg) #:prefab)
(struct immediate (imm) #:prefab)
(struct stack () #:prefab)
(define-app-pattern ~@operand
  (match-lambda
    [(list (~@direct-reg reg))
     (operand reg)]
    [(list (register-token 'stack))
     (operand (stack))]
    [(list (~@expr imm))
     (operand (immediate imm))]
    [(@reference (list (~@expr imm)))
     (operand (reference (immediate imm)))]
    [(@reference-indexed (list (~@expr imm)))
     (operand (reference-indexed (immediate imm)))]
    [_ #false]))

(struct condition (inv carry zero) #:prefab)
(define-app-pattern ~@condition
  (match-lambda
    ['()
     (condition #false #false #false)]
    [(condition-token kind)
     (match kind
       ['c (condition #false #true #false)]
       ['z (condition #false #false #true)]
       ['cz (condition #false #true #true)]
       ['nc (condition #true #true #false)]
       ['nz (condition #true #false #true)]
       ['ncz (condition #true #true #true)])]))

(struct operation (instr ops condition) #:prefab)

(define-app-pattern ~@operation
  (match-lambda
    [(list* (~@bin-inst inst) op1 ___ (punctuation-token 'comma) op2 ___ (~@condition con))
     (match inst
       [(binary-instruction mov)
        (match* (op1 op2)
          [((~@operand rs2) (~@operand rs1))
           (operation inst (list rs2 rs1) con)]
          [((list (~@direct-reg rs2)) (@reference (~@operand rs1)))
           (operation inst (list rs2 (reference rs1)) con)]
          [((list (~@direct-reg rs2)) (@reference-indexed (~@operand rs1)))
           (operation inst (list rs2 (reference-indexed rs1)) con)]
          [((@reference (~@operand rs1)) (list (~@direct-reg rs2)))
           (operation inst (list (reference rs1) rs2) con)]
          [((@reference-indexed (~@operand rs1)) (list (~@direct-reg rs2)))
           (operation inst (list (reference-indexed rs1) rs2) con)]
          [(_ _) #false])]
       [_
        (match* (op1 op2)
          [((~@operand op1) (~@operand op2))
           (operation inst (list op1 op2) con)]
          [(_ _) #false])])]
    [(list* (~@unary-inst inst) op1 ___ (~@condition con))
     (match op1
       [(~@operand rs1)
        (operation inst (list rs1) con)]
       [_ #false])]
    [(list* (~@implied-inst inst) (~@condition con))
     (operation inst '() con)]
    [_ #false]))

(struct constant (name value) #:prefab)
(struct label (name) #:prefab)
(struct db-directive (exprs) #:prefab)
(struct empty-line () #:prefab)

(define comma?
  (match-lambda
    [(punctuation-token 'comma) #t]
    [_ #f]))

(define (parse-tokens tokens)
  (match tokens
    [(list* (db-token) (list-split comma? (list (list (~@db-expr exprs)) ...)))
     (db-directive exprs)]
    [(list (const-token) (~@ident name) (punctuation-token 'equal) (~@expr value))
     (constant name value)]
    [(list (~@ident id) (punctuation-token 'colon))
     (label id)]
    [(~@operation op) op]
    ['() (empty-line)]
    [_ #false]))

(define (parse-string str) (parse-tokens (scan str)))

(module+ test
  (define valid-lines
    '("mov ac, ac"
      "mov ac, br"
      "mov ac, ix"
      "mov ac, sp"
      "mov ac, 0"
      "mov ac, 0b1001"
      "mov ac, 0o777"
      "mov ac, 0x50"
      "mov ac, (0x500)"
      "mov ac, (myVariable)"
      "mov ac, (myArray + ix)"
      "mov ac, stack"
      "mov ac, ((myArray) + ix)"
      "mov ((myArray) + ix), ac"
      "mov ((myArray)), ac"
      "mov ac, ((myArray))"
      "test_label:"
      "jmp 0x104"
      "jmp start"
      "jsr main"
      "jsr (vtable + ix)"
      "jsr ac"
      "hlt"
      "ret"
      "const foo = 0x50"
      "db \"hello\", 52, hello, 0x83"
      "mov ac, ac -ncz"))

  (for ([line valid-lines])
    (let ([result (parse-string line)])
      (unless result
        (check-true line)))))

#;(
   
   jmp 0x104
       jmp start


       jmp start
       my_global:
       db 0xff, 0xfe
       my_string:
       db "hello\0"
       start:
  
       )

