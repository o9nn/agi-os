(use-modules (opencog))
(use-modules (opencog exec))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea one"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea two"))
(AssociativeLink (ConceptNode "idea two") (ConceptNode "idea one"))
(AssociativeLink (ConceptNode "idea two") (ConceptNode "idea three"))
(AssociativeLink (ConceptNode "idea three") (ConceptNode "idea one"))
(AssociativeLink (ConceptNode "idea three") (ConceptNode "idea four"))
(AssociativeLink (ConceptNode "idea four") (ConceptNode "idea one"))
(AssociativeLink (ConceptNode "idea four") (ConceptNode "idea five"))
(AssociativeLink (ConceptNode "idea five") (ConceptNode "idea one"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea three"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea four"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea five"))
(define  one->x
	(AssociativeLink
		(ConceptNode "idea one")
		(VariableNode "$x")
	)
)
(define x->one
	(AssociativeLink
		(VariableNode "$x")
		(ConceptNode "idea one")
	)
)
(define (wrapper core)
	(BindLink
		(VariableNode "$x")
		(AndLink core)
		(VariableNode "$x")
	)
)
(define (five-arcs)
	(wrapper (list one->x x->one))
)
(define (one-arc-one)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
		)
	)
)
(define (one-arc-three)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
		)
	)
)
(define (zero-arcs)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
		)
	)
)
(define (four-arcs)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			)
		)
	)
)
(define (two-arcs)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			)
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
			)
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea five"))
			)
		)
	)
)
(define (four-not)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
			)
		)
	)
)
(define (two-or)
	(wrapper
		(list one->x x->one
			(OrLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea two"))
			)
		)
	)
)
(define (three-nor)
	(wrapper
		(list one->x x->one
			(NotLink
				(OrLink
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea two"))
				)
			)
		)
	)
)
(define (two-fancy)
	(wrapper
		(list one->x x->one
			(AndLink
				(NotLink
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
				)
				(NotLink
					(OrLink
						(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
						(IdenticalLink (VariableNode "$x") (ConceptNode "idea five"))
					)
				)
			)
		)
	)
)