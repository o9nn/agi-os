(define X (Variable "$X"))
(define E (Variable "$E"))
(define G (Glob "$G"))
(define B1 (Concept "B1"))
(define B2 (Concept "B2"))
(define B3 (Concept "B3"))
(define ball (Concept "ball"))
(define green (Concept "green"))
(define target-known-evidence
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      (Set
        (List
          (Inheritance B1 ball)
          (Inheritance B1 green))
        (List
          (Inheritance B2 ball)
          (Inheritance B2 green))
        (List
          (Inheritance B3 ball)
          (Inheritance B3 green))))))
(define target-unknown-evidence
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      E)))
(define vardecl-unknown-evidence
  (TypedVariable
    E
    (Type "SetLink")))
(define target-unknown-evidence-with-glob
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      (Set G))))