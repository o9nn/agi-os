(define (stv mean conf) (cog-new-stv mean conf))
(EvaluationLink (stv 1 1)
	(PredicateNode "LivesIn")
	(ListLink
		(FeatureNode "person1")
		(ConceptNode "red_house")
	)
)
(EvaluationLink (stv 1 1)
	(PredicateNode "LivesIn")
	(ListLink
		(FeatureNode "person2")
		(ConceptNode "red_house")
	)
)
(define (is-same-rule)
	(BindLink
		(VariableList
			(TypedVariableLink
				(VariableNode "$predicate")
				(TypeNode "PredicateNode")
			)
			(TypedVariableLink
				(VariableNode "$person_a")
				(TypeNode "FeatureNode")
			)
			(TypedVariableLink
				(VariableNode "$person_b")
				(TypeNode "FeatureNode")
			)
			(TypedVariableLink
				(VariableNode "$property")
				(TypeNode "ConceptNode")
			)
		)
		(AndLink
			(EvaluationLink
				(VariableNode "$predicate")
				(ListLink
					(VariableNode "$person_a")
					(VariableNode "$property")
				)
			)
			(EvaluationLink
				(VariableNode "$predicate")
				(ListLink
					(VariableNode "$person_b")
					(VariableNode "$property")
				)
			)
			(AbsentLink
				(EvaluationLink
					(PredicateNode "IsSamePerson")
					(ListLink
						(VariableNode "$person_a")
						(VariableNode "$person_b")
					)
				)
			)
		)
		(EvaluationLink
			(PredicateNode "IsSamePerson")
			(ListLink
				(VariableNode "$person_a")
				(VariableNode "$person_b")
			)
		)
	)
)
(define (transitive-rule)
	(BindLink
		(VariableList
			(TypedVariableLink
				(VariableNode "$person_a")
				(TypeNode "FeatureNode")
			)
			(TypedVariableLink
				(VariableNode "$person_b")
				(TypeNode "FeatureNode")
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "IsSamePerson")
				(ListLink
					(VariableNode "$person_a")
					(VariableNode "$person_b")
				)
			)
		)
		(OrderedLink
			(VariableNode "$person_a")
			(VariableNode "$person_b")
		)
	)
)