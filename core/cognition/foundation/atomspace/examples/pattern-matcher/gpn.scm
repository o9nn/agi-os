(use-modules (opencog))
(define (rand-ok atom)
	(define r (random 2))
	(simple-format #t "Random number: ~A " r)
	(if (< 0 r)
		(begin
			(simple-format #t "Picked ~A\n" (cog-name atom))
			(stv 1 1)
		)
		(begin
			(simple-format #t "Did not pick ~A\n" (cog-name atom))
			(stv 0 1)
		)
	)
)
(define sometimes
	(Evaluation
		(GroundedPredicate "scm: rand-ok")
		(List (Concept "something"))))
(Evaluation
	(Predicate "is-a")
	(List (Concept "Aristotle") (Concept "logician")))
(Evaluation
	(Predicate "is-a")
	(List (Concept "CS Pierce") (Concept "logician")))
(define find-logicians
	(Bind
		(Variable "$person")
		(And
			(Evaluation
				(Predicate "is-a")
				(List (Variable "$person") (Concept "logician")))
			(Evaluation
				(GroundedPredicate "scm: rand-ok")
				(List (Variable "$person"))))
		(Variable "$person")))
(cog-execute! find-logicians)
(cog-execute! find-logicians)
(cog-execute! find-logicians)