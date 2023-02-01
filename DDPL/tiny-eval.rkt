#lang racket

(struct state (memory input output) #:transparent)

(define (make-state)
  (state (hash) '() '()))

(define (e-eval e s)
  (match e
    [(? number? n) (list n s)]
    [(? boolean? b) (list b s)]
    [(? symbol? x)
     (match-let ([(state m i o) s])
       (match (hash-ref m x 'unbound)
         ['unbound 'error]
         [v (list v s)]))]
    [(list 'NOT e1)
     (match (e-eval e1 s)
       [(list v1 s1)
        (if (boolean? v1)
            (list (not v1) s1)
            'error)]
       [_ 'error])]
    [(list 'EQ e1 e2)
     (match (e-eval e1 s)
       [(list v1 s1)
        (match (e-eval e2 s1)
          [(list v2 s2)
           (list (equal? v1 v2) s2)]
          [_ 'error])]
        [_ 'error])]
    [(list 'PLUS e1 e2)
     (match (e-eval e1 s)
       [(list v1 s1)
        (match (e-eval e2 s1)
          [(list v2 s2)
           (if (and (number? v1) (number? v2))
               (list (+ v1 v2) s2)
               'error)]
          [_ 'error])]
        [_ 'error])]))

(define (c-eval c s)
  (match c
    [(list 'ASSIGN x e)
     (match (e-eval e s)
       [(list v (state m i o))
        (state (hash-set m x v) i o)]
       [_ 'error])]
    [(list 'OUTPUT e)
     (match (e-eval e s)
       [(list v (state m i o))
        (state m i (cons v o))]
       [_ 'error])]
    [(list 'SEQ c1 c2)
     (match (c-eval c1 s)
       ['error 'error]
       [s1 (c-eval c2 s1)])]))

(require rackunit)

(define whatever (make-state))

(check-equal?
 (e-eval '(PLUS 1 1) whatever)
 `(2 ,whatever)
 "1 + 1 = 2")

(check-equal?
 (e-eval '(PLUS 1 #f) whatever)
 'error
 "plus requires numbers")

(check-equal?
 (e-eval '(EQ 1 #f) whatever)
 `(#f ,whatever)
 "plus requires numbers")

(check-equal?
 (e-eval '(NOT #f) whatever)
 `(#t ,whatever)
 "not false = true")

(check-equal?
  (e-eval '(NOT 1) whatever)
 'error
 "not requires a boolean")
