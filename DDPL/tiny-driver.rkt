#lang racket

(require "tiny-parser.rkt")
(require "tiny-eval.rkt")

(define (tiny-run s [input '()])
  (c-eval (parse-string s) (make-state input)))

(require rackunit)

(check-equal?
 (hash-ref (state-memory (tiny-run "a := 0; while not (a = 10) do a := a + 1")) 'a)
 10
 "while run")

(check-equal?
 (hash-ref (state-memory (tiny-run "if 1 = 2 then a := 1 else a := 2; output a")) 'a)
 2
 "If run")


(define example #<</
sum := 0; x := read;
while not (x = true) do (sum := sum + x; x := read);
output sum
/
)

(check-equal?
 (state-output (tiny-run example '(1 2 3 #t)))
 '(6)
 "Example")
