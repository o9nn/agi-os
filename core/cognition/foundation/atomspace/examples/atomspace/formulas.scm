(use-modules (opencog) (opencog exec))
(Concept "A" (stv 0.8 1.0))
(cog-execute! (StrengthOf (Concept "A")))
(Concept "B" (stv 0.6 0.9))
(cog-execute!
(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))
(cog-evaluate!
(FormulaPredicate
(Minus
(Number 1)
(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))
(Times (ConfidenceOf (Concept "A")) (ConfidenceOf (Concept "B")))))
(cog-evaluate!
(FormulaPredicate (Number 0.7) (Number 0.314)))
(define my-ev-link
(Evaluation
(FormulaPredicate (Number 0.7) (Number 0.314))
(List
(Concept "A")
(Concept "B"))))
(cog-evaluate! my-ev-link)
(display my-ev-link)
(cog-evaluate!
(Evaluation
(FormulaPredicate
(Minus
(Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y"))))
(List
(Concept "A")
(Concept "B"))))
(cog-evaluate!
(Evaluation
(FormulaPredicate
(Lambda (Minus
(Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y")))))
(Lambda (Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y")))))
(List
(Concept "A")
(Concept "B"))))
(cog-execute!
(FormulaPredicate
(Plus (Number 41)
(Minus
(Number 1)
(Times
(StrengthOf (Variable "$VA"))
(StrengthOf (Variable "$VB")))))
(Times
(ConfidenceOf (Variable "$VA"))
(ConfidenceOf (Variable "$VB")))))
(define the-put-result
(cog-execute!
(PutLink
(VariableList (Variable "$VA") (Variable "$VB"))
(Evaluation
(FormulaPredicate
(Minus
(Number 1)
(Times
(StrengthOf (Variable "$VA"))
(StrengthOf (Variable "$VB"))))
(Times
(ConfidenceOf (Variable "$VA"))
(ConfidenceOf (Variable "$VB"))))
(List
(Variable "$VA") (Variable "$VB")))
(Set (List (Concept "A") (Concept "B"))))))
(define evelnk (cog-outgoing-atom the-put-result 0))
(Concept "A" (stv 0.3 0.5))
(Concept "B" (stv 0.4 0.5))
(cog-evaluate! evelnk)
(Concept "A" (stv 0.1 0.99))
(Concept "B" (stv 0.1 0.99))
(cog-evaluate! evelnk)
(DefineLink
(DefinedPredicate "has a reddish color")
(FormulaPredicate
(Minus
(Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y")))))
(Concept "A" (stv 0.9 0.98))
(Concept "B" (stv 0.9 0.98))
(cog-evaluate!
(Evaluation
(DefinedPredicate "has a reddish color")
(List
(Concept "A")
(Concept "B"))))