(define (negate a)
(- 1 a))
(define (precise-modus-ponens-strength-formula sA sAB snotAB)
(+ (* sAB sA) (* snotAB (negate sA))))