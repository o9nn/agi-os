(EvaluationLink
	(PredicateNode "net-worth")
	(ListLink
		(ConceptNode "Bill Gates")
		(NumberNode "500000")
	)
)
(EvaluationLink
	(PredicateNode "net-worth")
	(ListLink
		(ConceptNode "Obama")
		(NumberNode "1000")
	)
)
(EvaluationLink
	(PredicateNode "net-worth")
	(ListLink
		(ConceptNode "Susan M. from Peoria")
		(NumberNode "200")
	)
)
(EvaluationLink
	(PredicateNode "net-worth")
	(ListLink
		(ConceptNode "George P. from Waxahachie")
		(NumberNode "310")
	)
)
(define (richer-than-person-x-cmp person-x comp-link)
	(BindLink
		(VariableList
			(VariableNode "$who")
			(TypedVariableLink
				(VariableNode "$less-wealth")
				(TypeNode "NumberNode")
			)
			(TypedVariableLink
				(VariableNode "$more-wealth")
				(TypeNode "NumberNode")
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "net-worth")
				(ListLink
					person-x
					(VariableNode "$less-wealth")
				)
			)
			(EvaluationLink
				(PredicateNode "net-worth")
				(ListLink
					(VariableNode "$who")
					(VariableNode "$more-wealth")
				)
			)
			comp-link
		)
		(VariableNode "$who")
	)
)
(define builtin-cmp
	(GreaterThanLink
		(VariableNode "$more-wealth")
		(VariableNode "$less-wealth")
	)
)
(define (builtin-than-person-x person-x)
	(richer-than-person-x-cmp person-x builtin-cmp))
(define (builtin-than-gates)
	(builtin-than-person-x (ConceptNode "Bill Gates")))
(define (builtin-than-obama)
	(builtin-than-person-x (ConceptNode "Obama")))
(define (builtin-than-george)
	(builtin-than-person-x (ConceptNode "George P. from Waxahachie")))
(define (builtin-than-susan)
	(builtin-than-person-x (ConceptNode "Susan M. from Peoria")))
(define (richer a b)
	(if (> (cog-number a) (cog-number b))
		(stv 1 1)
		(stv 0 1)
	)
)
(define scm-cmp
	(EvaluationLink
		(GroundedPredicateNode "scm:richer")
		(ListLink
			(VariableNode "$more-wealth")
			(VariableNode "$less-wealth")
		)
	)
)
(define (scm-than-person-x person-x)
	(richer-than-person-x-cmp person-x scm-cmp))
(define (scm-than-gates)
	(scm-than-person-x (ConceptNode "Bill Gates")))
(define (scm-than-obama)
	(scm-than-person-x (ConceptNode "Obama")))
(define (scm-than-george)
	(scm-than-person-x (ConceptNode "George P. from Waxahachie")))
(define (scm-than-susan)
	(scm-than-person-x (ConceptNode "Susan M. from Peoria")))