(EvaluationLink
	(PredicateNode "all-var")
	(ListLink
		(VariableNode "$var-a")
		(VariableNode "$var-b")
	)
)
(define bindy
	(BindLink
		(VariableNode "$var-a")
		(EvaluationLink
			(VariableNode "$var-a")
			(QuoteLink
				(VariableNode "$var-a")
				(VariableNode "$var-b")
			)
		)
		(VariableNode "$var-a")
	)
)