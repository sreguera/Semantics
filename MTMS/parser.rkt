#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(provide parse-string (protect-out mtpc-parser mtpc-lexer))

(define-tokens data-tokens (NUM ID))
(define-empty-tokens punct-tokens (EOF IF THEN ELSE FI LPAREN RPAREN PLUS ASSIGN EQ NOT IMP))

(define mtpc-lexer
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
   [whitespace (mtpc-lexer input-port)]))

(define mtpc-parser
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

(define (parse-string s)
  (define p (open-input-string s))
  (mtpc-parser (λ () (mtpc-lexer p))))
