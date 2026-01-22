(use-modules (opencog) (opencog exec))
(State (Concept "stop light") (Concept "red light"))
(define exclude-red
	(Meet
		(Variable "x")
		(And
			(State (Concept "stop light") (Variable "x"))
			(Exclusive (Concept "red light") (Variable "x")))))
(define not-red
	(Meet
		(Variable "x")
		(And
			(State (Concept "stop light") (Variable "x"))
			(Not (Equal (Concept "red light") (Variable "x"))))))