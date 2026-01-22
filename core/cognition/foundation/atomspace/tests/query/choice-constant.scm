(use-modules (opencog))
(use-modules (opencog exec))
(ListLink
	(ConceptNode "A")
	(ConceptNode "B")
)
(define query
	(GetLink
		(TypedVariableLink (VariableNode "$x") (TypeNode "ConceptNode"))
		(ListLink
			(ChoiceLink
				(ConceptNode "A")
				(ConceptNode "C")
			)
			(VariableNode "$x")
		)
	)
)