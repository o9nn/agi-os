(define varlist
	(VariableList
		(VariableNode "$a")
		(VariableNode "$b")
	)
)
(define template
	(EvaluationLink
		(PredicateNode "something")
		(ListLink
			(VariableNode "$b")
			(VariableNode "$a")
		)
	)
)
(define arglist
	(ListLink
		(ConceptNode "one")
		(NumberNode 2.0000)
	)
)
(define answer
	(EvaluationLink
		(PredicateNode "something")
		(ListLink
			(NumberNode 2.0000)
			(ConceptNode "one")
		)
	)
)
(define free-template
	(EvaluationLink
		(PredicateNode "something")
		(ListLink
			(VariableNode "$b")
			(VariableNode "$c")
			(VariableNode "$a")
			(ListLink
				(VariableNode "$d")
			)
			(VariableNode "$a")
			(VariableNode "$a")
		)
	)
)
(define free-answer
	(EvaluationLink
		(PredicateNode "something")
		(ListLink
			(NumberNode 2.0000)
			(VariableNode "$c")
			(ConceptNode "one")
			(ListLink
				(VariableNode "$d")
			)
			(ConceptNode "one")
			(ConceptNode "one")
		)
	)
)
(define typed-varlist
	(VariableList
		(TypedVariableLink
			(VariableNode "$a")
			(TypeNode "ConceptNode")
		)
		(TypedVariableLink
			(VariableNode "$b")
			(TypeChoice
				(TypeNode "NumberNode")
				(TypeNode "AnchorNode")
			)
		)
	)
)
(define bad-arglist
	(ListLink
		(NumberNode 1.0000)
		(ConceptNode "two")
	)
)
(define bad-varlist
	(VariableList
		(TypedVariableLink
			(VariableNode "$a")
			(TypeNode "ConceptNodeNode")
		)
		(TypedVariableLink
			(VariableNode "$b")
			(TypeChoice
				(TypeNode "NumberNode")
				(TypeNode "AnchorNode")
			)
		)
	)
)