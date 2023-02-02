#lang racket

(require rackunit)
(require parser-tools/lex)
(require "tiny-parser.rkt")

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
 (get-tokens tiny-lexer "( ) if then else while do ; := + = not output read")
 '(LPAREN RPAREN IF THEN ELSE WHILE DO SEMI ASSIGN PLUS EQ NOT OUTPUT READ EOF)
 "Simple tokens are scanned.")

(check-equal?
 (get-tokens tiny-lexer "abcd 0 1 true false") 
 '((ID abcd) (NUM 0) (NUM 1) (BOOL #t) (BOOL #f) EOF)
 "Complex tokens are scanned.")

(check-equal?
 (parse-string "a := 0; a := 1")
 '(SEQ (ASSIGN a 0) (ASSIGN a 1))
 "Parsing assignments")

(check-equal?
 (parse-string "while true do (a := 0; a := 1)")
 '(WHILE #t (SEQ (ASSIGN a 0) (ASSIGN a 1)))
 "Parsing while")

(check-equal?
 (parse-string "while true do a := 0; a := 1")
 '(SEQ (WHILE #t (ASSIGN a 0)) (ASSIGN a 1))
 "Parsing while")

(check-equal?
 (parse-string "if true then a := 1 ; b := 2 else c := 3 ; d := 4")
 '(SEQ (IF #t (SEQ (ASSIGN a 1) (ASSIGN b 2)) (ASSIGN c 3)) (ASSIGN d 4))
 "Parsing if")

(check-equal?
 (parse-string "a := 1 = not 2 + 3 + 4")
 '(ASSIGN a (EQ 1 (PLUS (PLUS (NOT 2) 3) 4)))
 "Parsing ops")
