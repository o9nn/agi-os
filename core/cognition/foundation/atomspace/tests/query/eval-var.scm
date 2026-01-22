(use-modules (opencog))
(use-modules (opencog exec))
(define (truf x)
	(cond
		((equal? x (ConceptNode "good")) (cog-new-stv 1 1))
		((equal? x (ConceptNode "bad")) (cog-new-stv 0 1))
		(else (throw 'whats-up-jack "you done it wrong"))
	)
)
(define (konsekwens x)
	(ImplicationLink x x)
)
(ContextLink
	(ConceptNode "situation")
	(EvaluationLink
		(GroundedPredicateNode "scm: truf")
		(ListLink (ConceptNode "good"))
	)
	(ExecutionOutputLink
		(GroundedSchemaNode "scm: konsekwens")
		(ListLink (PredicateNode "acceptance"))
	)
)
(ContextLink
	(ConceptNode "predicament")
	(EvaluationLink
		(GroundedPredicateNode "scm: truf")
		(ListLink (ConceptNode "bad"))
	)
	(ExecutionOutputLink
		(GroundedSchemaNode "scm: konsekwens")
		(ListLink (PredicateNode "rejection"))
	)
)
(define (do-cond condi)
	(BindLink
		(VariableList
			(VariableNode "$cxt")
			(VariableNode "$condition")
			(VariableNode "$action")
		)
		(AndLink
			(ContextLink
				(VariableNode "$cxt")
				(VariableNode "$condition")
				(VariableNode "$action")
			)
			condi
		)
		(VariableNode "$action")
	)
)
(define (do-things) (do-cond (VariableNode "$condition")))
(define (do-nthings) (do-cond (NotLink (VariableNode "$condition"))))
(define (do-nnthings) (do-cond (NotLink (NotLink (VariableNode "$condition")))))