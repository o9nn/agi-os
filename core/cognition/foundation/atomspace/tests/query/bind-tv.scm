(use-modules (opencog) (opencog exec))
(define (sp x)
"
  Return (stv 0.55 0.55) and store it into its own call, that is
  Evaluation (stv 0.55 0.55)
    GroundedPredicate \"scm:sp\"
    x
"
  (let* ((tv (stv 0.55 0.55))
         (gp (GroundedPredicate "scm:sp"))
         (ev (Evaluation gp x)))
    (cog-set-tv! ev tv)
    tv))
(define GP (GroundedPredicate "scm:sp"))
(define A (Concept "A"))
(define B (Concept "B"))
(define E (Evaluation GP A))
(define X (Variable "$X"))
(Inheritance A B)
(define query
  (BindLink
    (AndLink
      E
      (InheritanceLink X B))
    E))