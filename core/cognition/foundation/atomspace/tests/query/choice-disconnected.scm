(use-modules (opencog))
(use-modules (opencog exec))
(EvaluationLink
	(PredicateNode "this way")
	(ListLink
		(ConceptNode "this one")
		(ConceptNode "thing two")
	)
)
(EvaluationLink
	(PredicateNode "that way")
	(ListLink
		(ConceptNode "thing one")
		(ConceptNode "that too")
	)
)
(EvaluationLink
	(PredicateNode "third way")
	(ListLink
		(ConceptNode "thing one")
		(ConceptNode "thing two")
	)
)
(define (top-disco)
	(BindLink
		(ChoiceLink
			(EvaluationLink
				(PredicateNode "this way")
				(ListLink
					(VariableNode "$x")
					(ConceptNode "thing two")
				)
			)
			(EvaluationLink
				(PredicateNode "that way")
				(ListLink
					(ConceptNode "thing one")
					(VariableNode "$x")
				)
			)
		)
		(VariableNode "$x")
	)
)
(define (wrapped-disco)
	(BindLink
		(AndLink
			(ChoiceLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
				(EvaluationLink
					(PredicateNode "that way")
					(ListLink
						(ConceptNode "thing one")
						(VariableNode "$x")
					)
				)
			)
		)
		(VariableNode "$x")
	)
)