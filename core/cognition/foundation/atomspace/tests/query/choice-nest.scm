(use-modules (opencog))
(use-modules (opencog exec))
(MemberLink
	(ConceptNode "Tom")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Joe")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Hank")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Mary")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Phillip")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Milton")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Charlie")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Chayim")
	(ConceptNode "ways and means")
)
(MemberLink
	(ConceptNode "Stuart")
	(ConceptNode "ways and means")
)
(ListLink
	(MemberLink
		(ConceptNode "Tom")
		(ConceptNode "Senator")
	)
)
(ListLink
	(MemberLink
		(ConceptNode "Joe")
		(ConceptNode "Representative")
	)
)
(ListLink
	(MemberLink
		(ConceptNode "Hank")
		(ConceptNode "CEO")
	)
)
(ListLink
	(MemberLink
		(ConceptNode "Mary")
		(ConceptNode "Page")
	)
)
(ListLink
	(MemberLink
		(ConceptNode "Phillip")
		(ConceptNode "Secretary")
	)
)
(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Milton")
			(ConceptNode "Business")
		)
	)
)
(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Charlie")
			(ConceptNode "Industry")
		)
	)
)
(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Chayim")
			(ConceptNode "Banking")
		)
	)
)
(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Stuart")
			(ConceptNode "Diletant")
		)
	)
)
(define (nest)
	(BindLink
		(AndLink
			(MemberLink
				(VariableNode "$x")
				(ConceptNode "ways and means")
			)
			(ListLink
				(ChoiceLink
					(MemberLink
						(VariableNode "$x")
						(ConceptNode "Senator")
					)
					(MemberLink
						(VariableNode "$x")
						(ConceptNode "Representative")
					)
					(EvaluationLink
						(PredicateNode "involved")
						(ChoiceLink
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Business")
							)
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Industry")
							)
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Banking")
							)
						)
					)
				)
			)
		)
		(VariableNode "$x")
	)
)
(define (nest-bad)
	(BindLink
		(AndLink
			(MemberLink
				(VariableNode "$x")
				(ConceptNode "ways and means")
			)
			(ListLink
				(ChoiceLink
					(MemberLink
						(VariableNode "$x")
						(ConceptNode "Senator")
					)
					(MemberLink
						(VariableNode "$x")
						(ConceptNode "Representative")
					)
					(ChoiceLink
						(MemberLink
							(VariableNode "$x")
							(ConceptNode "Page")
						)
						(MemberLink
							(VariableNode "$x")
							(ConceptNode "Secretary")
						)
					)
				)
			)
		)
		(VariableNode "$x")
	)
)