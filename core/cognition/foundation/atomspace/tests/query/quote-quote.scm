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
		(ConceptNode "bad banana")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(VariableNode "$wrong-var-a")
		(ConceptNode "apple")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(QuoteLink (VariableNode "$var-a"))
		(ConceptNode "banana")
	)
)
(EvaluationLink
	(PredicateNode "similar")
	(ListLink
		(QuoteLink (VariableNode "$wrong-var-a"))
		(ConceptNode "apple")
	)
)
(define bindy
	(BindLink
		(VariableNode "$var-a")
		(EvaluationLink
			(PredicateNode "similar")
			(ListLink
				(QuoteLink (QuoteLink (VariableNode "$var-a")))
				(VariableNode "$var-a")
			)
		)
		(VariableNode "$var-a")
	)
)