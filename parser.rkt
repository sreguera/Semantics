#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define-tokens data-tokens (NUM ID))
(define-empty-tokens punct-tokens (EOF IF THEN ELSE FI LPAREN RPAREN PLUS ASSIGN EQ NOT IMP))

(define the-lexer
  (lexer
   [(eof) (token-EOF)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["fi" (token-FI)]
   ["+" (token-PLUS)]
   [":=" (token-ASSIGN)]
   ["=" (token-EQ)]
   ["¬" (token-NOT)]
   ["⊃" (token-IMP)]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: alphabetic
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (the-lexer input-port)]))

(define test-input "( ) 123 A_1 abc1 ¬ ⊃ λ if then else fi + := =")

(define (get-tokens a-lexer)
  (define p (open-input-string test-input))
  (define (get-all l)
    (let ([t (a-lexer p)])
      (if (equal? t 'EOF)
          (reverse (cons t l))
          (get-all (cons t l)))))
  (get-all empty))


(define the-parser
  (parser
   [start stmt]
   [end EOF]
   [error void]
   [tokens data-tokens punct-tokens]
   [grammar
    [stmt [(ID ASSIGN expr) (list 'ASSIGN $1 $3)]]
    [expr [(LPAREN expr RPAREN) $2]
          [(NUM) $1]
          [(ID) $1]]]))

(define test-input2 "a := 23")

(define (parse-it s)
  (define p (open-input-string s))
  (the-parser (λ () (the-lexer p))))
