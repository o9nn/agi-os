(use-modules (opencog) (opencog exec))
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(ConceptNode "apple")
		(ConceptNode "banana")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(ConceptNode "orange")
		(ConceptNode "apple")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(ConceptNode "apple")
		(ConceptNode "grape")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(VariableNode "$var-a")
		(ConceptNode "banana")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(VariableNode "$wrong-var-a")
		(ConceptNode "apple")
	)
)
(define bindy
	(BindLink
		(TypedVariableLink
			(VariableNode "$var-a")
			(TypeNode "ConceptNode")
		)
		(EvaluationLink
			(PredicateNode "similar")
			(ListLink
				(QuoteLink (VariableNode "$var-a"))
				(VariableNode "$var-a")
			)
		)
		(VariableNode "$var-a")
	)
)
(define bother
	(BindLink
		(TypedVariableLink
			(VariableNode "$other")
			(TypeNode "ConceptNode")
		)
		(EvaluationLink
			(PredicateNode "similar")
			(ListLink
				(QuoteLink (VariableNode "$var-a"))
				(VariableNode "$other")
			)
		)
		(VariableNode "$other")
	)
)
(define bunbound
	(BindLink
		(VariableNode "$other")
		(EvaluationLink
			(PredicateNode "similar")
			(ListLink
				(VariableNode "$var-a")
				(VariableNode "$other")
			)
		)
		(VariableNode "$other")
	)
)