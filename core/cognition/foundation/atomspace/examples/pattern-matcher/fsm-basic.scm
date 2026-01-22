(use-modules (opencog))
(Set
	(Concept "initial state")
	(Concept "green")
	(Concept "yellow")
	(Concept "red")
)
(List
	(Anchor "Current State")
	(Concept "initial state")
)
(List
	(Concept "initial state")
	(Concept "green")
)
(List
	(Concept "green")
	(Concept "yellow")
)
(List
	(Concept "yellow")
	(Concept "red")
)
(List
	(Concept "red")
	(Concept "green")
)
(define take-one-step
	(Bind
		(VariableList
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			(List
				(Anchor "Current State")
				(Variable "$curr-state")
			)
			(List
				(Variable "$curr-state")
				(Variable "$next-state")
			)
		)
		(And
			(List
				(Anchor "Current State")
				(Variable "$next-state")
			)
			(Delete
				(List
					(Anchor "Current State")
					(Variable "$curr-state")
				)
			)
		)
	)
)