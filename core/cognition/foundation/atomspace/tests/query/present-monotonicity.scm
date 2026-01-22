(define query
  (let* (
     (R (Predicate "R"))
     (A (Execution (Schema "A")))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (P→Q (Quote
            (Implication
              (Unquote P)
              (Unquote Q))))
     (Q∧A (And Q A))
     (Q∧A→R (Implication
              Q∧A
              R)))
    (Get
      (VariableSet P Q)
      (Present P→Q Q∧A→R))))
(Implication
  (And
    (Predicate "Qbis")
    (Execution (Schema "A"))
  )
  (Predicate "R"))
(Implication
  (Predicate "P")
  (Predicate "Q"))
(Implication
  (And
    (Predicate "Q")
    (Execution (Schema "A"))
  )
  (Predicate "R"))