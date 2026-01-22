(EvaluationLink
	(GroundedPredicateNode "scm:do_stuff")
	(ListLink
		(ConceptNode "thing-a")
		(ConceptNode "thing-b")
	)
)
(define bindy
	(BindLink
		(VariableNode "$stuff")
		(EvaluationLink
			(QuoteLink (GroundedPredicateNode "scm:do_stuff"))
			(VariableNode "$stuff")
		)
		(VariableNode "$stuff")
	)
)
(TimesLink
	(NumberNode 3)
	(NumberNode 5)
	)
(define get-times-link
	(GetLink
		(VariableList
			(VariableNode "$a")
			(VariableNode "$b")
			)
		(QuoteLink
			(TimesLink
				(UnquoteLink (VariableNode "$a"))
				(UnquoteLink (VariableNode "$b"))
				)
			)
		)
	)