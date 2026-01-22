(use-modules (opencog) (opencog exec))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "pottery")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "statue")))
(Evaluation (Predicate "_obj") (List (Concept "make") (Concept "brick")))
(Evaluation (Predicate "from") (List (Concept "make") (Concept "clay")))
(define get-satisfying-set
	(GetLink
		(VariableList
			(Variable "$verb")
			(Variable "$var0")
			(Variable "$var1")
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
	)
)
(cog-execute! get-satisfying-set)
(define the-sat-set (cog-execute! get-satisfying-set))
(define reduction-rule
	(PutLink
		(VariableList
			(Variable "$verb")
			(Variable "$var0")
			(Variable "$var1"))
		(Evaluation
			(Predicate "make_from")
			(List (Variable "$var0") (Variable "$var1")))
		the-sat-set
	))
(cog-execute! reduction-rule)
(define find-and-rewrite-rule
	(PutLink
		(VariableList
			(Variable "verb")
			(Variable "thing")
			(Variable "stuff"))
		(Evaluation
			(Predicate "make_from")
			(List (Variable "thing") (Variable "stuff")))
		get-satisfying-set
	))
(cog-execute! find-and-rewrite-rule)