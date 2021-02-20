#lang racket/base

(require racket/match
         racket/list
         racket/format
         racket/function
         "error.rkt"
         "parser.rkt")

(provide assemble)

(struct op-code (condition op1 op2 rs1) #:prefab)

(define (replace-expression expr)
  (match expr
    [(expression (int-literal value)) value]
    [(expression (identifier id))
     (string->symbol id)]))

(define (op-code-generate op)
  (list* (+ (* (match (op-code-rs1 op)
                 [(? number? n) n]
                 [(list n _) n]))
            (* 8 (match (op-code-op2 op)
                   [(? number? n) n]
                   [(list n _) n]))
            (* 8 8 (op-code-op1 op))
            (* 8 8 8 (op-code-condition op)))
         (append (match (op-code-rs1 op)
                   [(list _ imm) (list imm)]
                   [_ '()])
                 (match (op-code-op2 op)
                   [(list _ imm) (list imm)]
                   [_ '()]))))

(define (operand->numeric op)
  (match op
    [(operand (direct-register reg))
     (match reg
       ['ac #b000]
       ['br #b001]
       ['ix #b010]
       ['sp #b011])]
    [(operand (stack)) #b111]
    [(operand (immediate (? expression? expr)))
     (list #b100 (replace-expression expr))]
    [(operand (reference (immediate (? expression? expr))))
     (list #b101 (replace-expression expr))]
    [(operand (reference-indexed (immediate (? expression? expr))))
     (list #b110 (replace-expression expr))]
    [_ (error "invalid operand")]))

(define (make-op-code con op1 op2 rs1)
  (op-code (+ (if (condition-inv con) 4 0)
              (if (condition-zero con) 2 0)
              (if (condition-carry con) 1 0))
           op1
           (match op2
             [(? operand? op2) (operand->numeric op2)]
             [(? number? op2) op2])
           (match rs1
             [(? operand? rs1) (operand->numeric rs1)]
             [(? number? rs1) rs1])))

(define (generate-code instr)
  (match instr
    [(label _) '()]
    [(constant _ _) '()]
    [(empty-line) '()]
    [(db-directive contents)
     (append* (for/list ([content contents])
                (match content
                  [(db-expression (? expression? expr))
                   (list (replace-expression expr))]
                  [(db-expression (string-literal chars))
                   (map char->integer chars)])))]
    [(operation (implied-instruction inst) '() con)
     (op-code-generate
      (match inst
        ['hlt (make-op-code con #b110 #b001 0)]
        ['ret (make-op-code con #b110 #b011 0)]
        ['clc (make-op-code con #b111 #b100 0)]
        ['clz (make-op-code con #b111 #b101 0)]
        ['sec (make-op-code con #b111 #b110 0)]
        ['sez (make-op-code con #b111 #b111 0)]))]
    [(operation (unary-instruction inst) (list rs1) con)
     ; unary instrs: (add adc sbw swb nnd and aib anb bia bna ora nor jmp jsr dec inc xor xnr)
     (op-code-generate (match inst
                         ['add (make-op-code con #b100 #b000 rs1)]
                         ['adc (make-op-code con #b100 #b001 rs1)]
                         ['sbw (make-op-code con #b100 #b110 rs1)]
                         ['swb (make-op-code con #b100 #b111 rs1)]
                         ['nnd (make-op-code con #b101 #b000 rs1)]
                         ['and (make-op-code con #b101 #b001 rs1)]
                         ['aib (make-op-code con #b101 #b010 rs1)]
                         ['anb (make-op-code con #b101 #b011 rs1)]
                         ['bia (make-op-code con #b101 #b100 rs1)]
                         ['bna (make-op-code con #b101 #b101 rs1)]
                         ['ora (make-op-code con #b101 #b110 rs1)]
                         ['nor (make-op-code con #b101 #b111 rs1)]
                         ['jmp (make-op-code con #b110 #b000 rs1)]
                         ['jsr (make-op-code con #b110 #b010 rs1)]
                         ['dec (make-op-code con #b110 #b100 rs1)]
                         ['inc (make-op-code con #b110 #b101 rs1)]
                         ['xor (make-op-code con #b111 #b010 rs1)]
                         ['xnr (make-op-code con #b111 #b011 rs1)]))]
    [(operation (binary-instruction inst) (list operand1 operand2) con)
     (define (make-swap-op op1 op2a op2b)
       (match* (operand1 operand2)
         [((operand (direct-register 'ac)) (? operand? rs1))
          (make-op-code con op1 op2a rs1)]
         [((? operand? rs1) (operand (direct-register 'ac)))
          (make-op-code con op1 op2b rs1)]
         [(_ _) (raise-error "Invalid operands. One operand must be `Ac`.")]))
     (define (encode-register reg)
       (match reg
         ['ac 0]
         ['bc 1]
         ['ix 2]
         ['sp 3]))
     (op-code-generate (match inst
                         ['cmp (make-swap-op #b111 #b000 #b001)]
                         ['sub (make-swap-op #b100 #b010 #b100)]
                         ['sbb (make-swap-op #b100 #b011 #b101)]
                         ['mov (match* (operand1 operand2)
                                 [((? operand? rs2) (? operand? rs1))
                                  (make-op-code con #b000 rs2 rs1)]
                                 [((direct-register reg) (reference rs1))
                                  (make-op-code con #b001 (+ #b000 (encode-register reg)) rs1)]
                                 [((direct-register reg) (reference-indexed rs1))
                                  (make-op-code con #b001 (+ #b100 (encode-register reg)) rs1)]
                                 [((reference rs1) (direct-register reg) )
                                  (make-op-code con #b010 (+ #b000 (encode-register reg)) rs1)]
                                 [((reference-indexed rs1) (direct-register reg))
                                  (make-op-code con #b010 (+ #b100 (encode-register reg)) rs1)]
                                 [(_ _) (raise-error "Invalid use of mov")])]
                         [_ (error "invalid instruction")]))]
    [_ (error "Invalid instruction")]))

(struct codegen-label (name) #:prefab)
(struct codegen-constant (name value) #:prefab)

(struct codegen-item (var bitcode) #:prefab)

(define (make-codegen-item source)
  (codegen-item (match source
                  [(label (identifier name))
                   (codegen-label (string->symbol name))]
                  [(constant (identifier name) (expression (int-literal value)))
                   (codegen-constant (string->symbol name) value)]
                  [(constant (identifier name) (expression (identifier var)))
                   (codegen-constant (string->symbol name) (string->symbol var))]
                  [_ #false])
                (generate-code source)))

(define (gather-environment items)
  (let gather ([offset 0]
               [items items]
               [env (hasheq)])
    (match items
      ['() env]
      [(list* (codegen-item ast bitcode) rest)
       (gather (+ offset (length bitcode))
               rest
               (match ast
                 [(codegen-label name)
                  (hash-set env name offset)]
                 [(codegen-constant name (? number? value))
                  (hash-set env name value)]
                 [(codegen-constant name (? symbol? var))
                  (hash-set env name (hash-ref env var))]
                 [_ env]))])))

(define (stitch-bitcode items env)
  (append* (for/list ([item items]
                      [line-number (in-naturals 1)])
             (parameterize ([error-line-number line-number])
               (map (match-lambda
                      [(? symbol? id) (hash-ref env
                                                id
                                                (thunk (raise-error (format "Unbound identifier '~a'" id))))]
                      [(? number? n) n])
                    (codegen-item-bitcode item))))))

; program must be a list of strings
(define (assemble program)
  (define codegen-items (for/list ([str program]
                                   [line-number (in-naturals 1)])
                          (parameterize ([error-line-number line-number])
                            (define ast (parse-string str))
                            (unless ast
                              (raise-error "invalid syntax"))
                            (make-codegen-item ast))))
  (define environment (gather-environment codegen-items))
  (define final-bitcode (stitch-bitcode codegen-items environment))
  final-bitcode)
