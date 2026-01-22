(use-modules (opencog))
(SetLink
	(ConceptNode "initial state")
	(ConceptNode "green")
	(ConceptNode "yellow")
	(ConceptNode "red")
)
(define my-trans (Concept "My FSM's Transition Rule"))
(define my-state (Anchor  "My FSM's Current State"))
(List
	my-state
	(Concept "initial state")
)
(ContextLink
	(Concept "initial state")
	(List
		my-trans
		(Concept "green")
	)
)
(ContextLink
	(Concept "green")
	(List
		my-trans
		(Concept "yellow")
	)
)
(ContextLink
	(Concept "yellow")
	(List
		my-trans
		(Concept "red")
	)
)
(ContextLink
	(Concept "red")
	(List
		my-trans
		(Concept "green")
	)
)
(define (create-fsm fsm-name fsm-state)
	(Bind
		(VariableList
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			(List
				fsm-state
				(Variable "$curr-state")
			)
			(Context
				(Variable "$curr-state")
				(List
					fsm-name
					(Variable "$next-state")
				)
			)
		)
		(And
			(List
				fsm-state
				(Variable "$next-state")
			)
			(Delete
				(List
					fsm-state
					(Variable "$curr-state")
				)
			)
		)
	)
)
(define my-fsm (create-fsm my-trans my-state))