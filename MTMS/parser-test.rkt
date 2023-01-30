#lang racket

(require rackunit)
(require parser-tools/lex)
(require "parser.rkt")

(define (get-tokens a-lexer a-string)
  (define input (open-input-string a-string))
  (define (extract token)
    (if (token? token)
        (list (token-name token) (token-value token))
        token))
  (define (get-all-tokens acc)
    (let ([token (a-lexer input)])
      (if (equal? token 'EOF)
          (reverse (cons (extract token) acc))
          (get-all-tokens (cons (extract token) acc)))))
  (get-all-tokens empty))

(check-equal?
 (get-tokens mtpc-lexer "( ) if then else fi := + = ¬ ⊃")
 '(LPAREN RPAREN IF THEN ELSE FI ASSIGN PLUS EQ NOT IMP EOF)
 "Simple tokens are scanned.")

(check-equal?
 (get-tokens mtpc-lexer "abcd 124")
 '((ID abcd) (NUM 124) EOF)
 "Complex tokens are scanned.")

(define test-input2 "a := 23")

(check-equal?
 (parse-string "a := 23")
 '(ASSIGN a 23)
 "Parsing assignments")
