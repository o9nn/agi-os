(use-modules (opencog) (opencog exec))
(define single
	(Filter
		(Lambda
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(Evaluation
			(Predicate "foo")
			(List (Concept "bar") (Concept "baz"))))
)
(cog-execute! single)
(define single-set
	(Filter
		(Lambda
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
		))
)
(cog-execute! single-set)
(define single-list
	(Filter
		(Lambda
			(Variable "$x")
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(List
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
		))
)
(cog-execute! single-list)
(define single-type
	(Filter
		(Lambda
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Variable "$x"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)
(cog-execute! single-type)
(define single-signature
	(Filter
		(Lambda
			(TypedVariable (Variable "$x")
				(Signature
					(Evaluation
						(Predicate "foo")
						(List (Concept "bar") (Type "ConceptNode")))))
			(Variable "$x"))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)
(cog-execute! single-signature)
(define double-num-set
	(Filter
		(Lambda
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)
(cog-execute! double-num-set)
(define double-con-set
	(Filter
		(Lambda
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)
(cog-execute! double-con-set)
(define imply-map
	(Filter
		(Rule
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y")))
			(Evaluation
				(Predicate "reverse-foo")
				(List (Variable "$y") (Variable "$x"))))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah two")))
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Number 3)))
		))
)
(cog-execute! imply-map)
(define summation
	(Filter
		(Rule
			(VariableList
				(TypedVariable (Variable "$x") (Type "NumberNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(Evaluation
				(Predicate "foo")
				(List (Variable "$x") (Variable "$y")))
			(Plus (Variable "$y") (Variable "$x")))
		(Set
			(Evaluation
				(Predicate "foo")
				(List (Concept "bar") (Concept "ah one")))
			(Evaluation
				(Predicate "foo")
				(List (Number 2) (Number 3)))
			(Evaluation
				(Predicate "foo")
				(List (Number 10) (Times (Number 3) (Number 2))))
		))
)
(cog-execute! summation)