(use-modules (opencog) (opencog exec))
(Evaluation
	(Predicate "_obj")
	(ListLink
		(Concept "make")
		(Concept "pottery")))
(Evaluation
	(Predicate "from")
	(ListLink
		(Concept "make")
		(Concept "clay")))
(define make-semantic-triple
	(BindLink
		(VariableList
			(Variable "$var0")
			(Variable "$var1")
			(Variable "$verb")
		)
		(AndLink
			(Evaluation
				(Predicate "_obj")
				(ListLink
					(Variable "$verb")
					(Variable "$var0")
				)
			)
			(EvaluationLink
				(Predicate "from")
				(ListLink
					(Variable "$verb")
					(Variable "$var1")
				)
			)
		)
		(EvaluationLink
			(PredicateNode "make_from")
			(ListLink
				(VariableNode "$var0")
				(VariableNode "$var1")
			)
		)
	)
)
(cog-execute! make-semantic-triple)