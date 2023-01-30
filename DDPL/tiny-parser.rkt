#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(provide parse-string (protect-out tiny-parser tiny-lexer))

;; Instead of 1 0 true false we have NUM and BOOL
(define-tokens data-tokens (NUM BOOL ID))
(define-empty-tokens punct-tokens (EOF IF THEN ELSE WHILE DO LPAREN RPAREN SEMI PLUS ASSIGN EQ NOT OUTPUT READ IMP))

(define tiny-lexer
  (lexer
   [(eof) (token-EOF)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["while" (token-WHILE)]
   ["do" (token-DO)]
   [";" (token-SEMI)]
   ["+" (token-PLUS)]
   [":=" (token-ASSIGN)]
   ["=" (token-EQ)]
   ["not" (token-NOT)]
   ["output" (token-OUTPUT)]
   ["read" (token-READ)]
   ["true" (token-BOOL #t)]
   ["false" (token-BOOL #f)]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: alphabetic
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (tiny-lexer input-port)]))

(define tiny-parser
  (parser
   [start cmd]
   [end EOF]
   [error void]
   [tokens data-tokens punct-tokens]
   [grammar
    [cmd [(ID ASSIGN expr) (list 'ASSIGN $1 $3)]
         [(OUTPUT expr) (list 'OUTPUT $2)]
         [(IF expr THEN cmd ELSE cmd) (list 'IF $2 $4 $6)]
         [(WHILE expr DO cmd) (list 'WHILE $2 $4)]
         [(cmd SEMI cmd) (list 'SEQ $1 $3)]
         [(LPAREN cmd RPAREN) $2]]
    [expr [(LPAREN expr RPAREN) $2]
          [(NUM) $1]
          [(BOOL) $1]
          [(ID) $1]]]))

(define (parse-string s)
  (define p (open-input-string s))
  (tiny-parser (λ () (tiny-lexer p))))
