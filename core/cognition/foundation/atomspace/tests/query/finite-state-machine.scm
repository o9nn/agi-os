(use-modules (opencog))
(use-modules (opencog exec))
(SetLink
	(ConceptNode "initial state")
	(ConceptNode "green")
	(ConceptNode "yellow")
	(ConceptNode "red")
)
(ListLink
	(AnchorNode "Current State")
	(ConceptNode "initial state")
)
(ListLink
	(ConceptNode "initial state")
	(ConceptNode "green")
)
(ListLink
	(ConceptNode "green")
	(ConceptNode "yellow")
)
(ListLink
	(ConceptNode "yellow")
	(ConceptNode "red")
)
(ListLink
	(ConceptNode "red")
	(ConceptNode "green")
)
(define take-one-step
	(BindLink
		(VariableList
			(VariableNode "$curr-state")
			(VariableNode "$next-state")
		)
		(AndLink
			(ListLink
				(AnchorNode "Current State")
				(VariableNode "$curr-state")
			)
			(ListLink
				(VariableNode "$curr-state")
				(VariableNode "$next-state")
			)
		)
		(AndLink
			(ListLink
				(AnchorNode "Current State")
				(VariableNode "$next-state")
			)
			(DeleteLink
				(ListLink
					(AnchorNode "Current State")
					(VariableNode "$curr-state")
				)
			)
		)
	)
)