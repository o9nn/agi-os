(use-modules (opencog) (opencog exec))
(Concept "is mostly true" (stv 0.9 0.9))
(Concept "is mostly false" (stv 0.234 0.9))
(define find-false
	(Bind
		(TypedVariable (Variable "$X") (Type 'ConceptNode))
		(And
			(Present (Variable "$X"))
			(GreaterThan (Number 0.5) (StrengthOf (Variable "$X"))))
		(Variable "$X")))
(cog-execute! find-false)
(define key (Predicate "some key"))
(cog-set-value! (Concept "thing-a") key (FloatValue 42))
(cog-set-value! (Concept "thing-b") key (FloatValue 35))
(define find-answer
	(Bind
		(TypedVariable (Variable "$X") (Type 'ConceptNode))
		(And
			(Present (Variable "$X"))
			(GreaterThan
				(Divide
					(FloatValueOf (Variable "$X") key)
					(Number 12))
				(Number 3)))
		(Variable "$X")))
(cog-execute! find-answer)