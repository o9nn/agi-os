(use-modules (opencog))
(use-modules (opencog exec))
(define query 
	(EvaluationLink
		(PredicateNode "visibility")
		(ListLink (VariableNode "$x"))))
(define golem
	(PutLink query (ConceptNode "item 42")))
(define destroy
	(BindLink query (DeleteLink query))
)
(define create
	(BindLink (AbsentLink query) golem)
)
(define room-state (AnchorNode "Room List"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))
(StateLink room-state room-empty)
(define is-visible
	(BindLink
		query
		(PutLink (StateLink room-state (VariableNode "$x")) room-nonempty)
	))
(define is-invisible
	(BindLink
		(AbsentLink query)
		(PutLink (StateLink room-state (VariableNode "$x")) room-empty)
	))
(define (show-room-state)
   (car (cog-chase-link 'StateLink 'ConceptNode room-state)))