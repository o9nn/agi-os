(use-modules (opencog) (opencog exec))
(define put-multi-set
	(PutLink
		(LambdaLink
			(VariableList (Variable "x") (Variable "y"))
			(EvaluationLink
				(PredicateNode "relatives")
				(ListLink
					(Variable "x")
					(Variable "y")
					(Concept "mom and pop"))))
		(ListLink
			(SetLink (Concept "Jim"))
			(SetLink (Concept "Jane")))))