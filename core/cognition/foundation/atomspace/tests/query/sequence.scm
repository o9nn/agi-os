(use-modules (opencog))
(use-modules (opencog exec))
(define green-light  (ConceptNode "green light"))
(define red-light  (ConceptNode "red light"))
(define num-green 0)
(define num-red 0)
(define (stop-go atom)
	(cond
		((equal? atom green-light) (begin (set! num-green (+ 1 num-green)) (stv 1 1)))
		((equal? atom red-light) (begin (set! num-red (+ 1 num-red)) (stv 0 1)))
		(else (throw 'not-a-stoplight "stop-go" "you're busted"))
	)
)
(define (off-road)
	(SequentialAndLink
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink
				(ConceptNode "corn field")
			)
		)
	)
)
(define (traffic-lights)
	(SequentialAndLink
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink red-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink
				(ConceptNode "traffic ticket")
			)
		)
	)
)
(define (all-green)
	(SequentialAndLink
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
	)
)
(define (anti-green)
	(SequentialAndLink
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(NotLink
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink green-light)
			)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink green-light)
		)
		(EvaluationLink
			(GroundedPredicateNode "scm: stop-go")
			(ListLink (ConceptNode "traffic ticket"))
		)
	)
)
(define drag-race
	(SatisfactionLink
		(VariableList)
		(SequentialOrLink
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink red-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink green-light)
			)
			(EvaluationLink
				(GroundedPredicateNode "scm: stop-go")
				(ListLink
					(ConceptNode ".... And they're off!"))))))