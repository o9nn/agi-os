(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "or-more-test")
(test-begin tname)
(State (List (Concept "alice") (Predicate "hungry")) (Concept "TRUE"))
(State (Concept "alice") (Concept "at home"))
(define who-is-hungry-1?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type 'Concept))
			(TypedVariable (Variable "y") (Type 'Concept)))
		(Present
			(State (Variable "x") (Variable "y"))
			(State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE"))
		)))
(test-assert "alice at home"
	(equal? (cog-execute! who-is-hungry-1?)
		(Set (List (Concept "alice") (Concept "at home")))))
(define who-is-hungry-2?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type 'Concept))
			(TypedVariable (Variable "y") (Type 'Concept)))
		(Or
			(Present (State (Variable "x") (Variable "y")))
			(Present (State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE")))
			)))
(test-assert "alice y at home"
	(equal? (cog-execute! who-is-hungry-2?)
		(Set
			(List (Concept "alice") (Concept "at home"))
			(List (Concept "alice") (Variable "y")))))
(define who-is-hungry-3?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type 'Concept))
			(TypedVariable (Variable "y") (Type 'Concept)))
		(Choice
			(Present (State (Variable "x") (Variable "y")))
			(Present (State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE")))
			)))
(test-assert "alice why at home"
	(equal? (cog-execute! who-is-hungry-3?)
		(Set
			(List (Concept "alice") (Concept "at home"))
			(List (Concept "alice") (Variable "y")))))
(State (List (Concept "alice") (Predicate "hungry")) (Concept "TRUE"))
(State (List (Concept "alice") (Predicate "thirsty")) (Concept "TRUE"))
(State (List (Concept "bob") (Predicate "hungry")) (Concept "FALSE"))
(State (List (Concept "bob") (Predicate "thirsty")) (Concept "FALSE"))
(State (List (Concept "charlie") (Predicate "hungry")) (Concept "FALSE"))
(State (List (Concept "charlie") (Predicate "thirsty")) (Concept "TRUE"))
(State (Concept "charlie") (Concept "at home"))
(State (List (Concept "at home") (Predicate "is cozy")) (Concept "TRUE"))
(define whos-on-first?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type 'Concept))
			(TypedVariable (Variable "y") (Type 'Concept)))
		(Choice
			(Present
				(State (Variable "x") (Variable "y"))
				(State (List (Variable "y") (Predicate "is cozy")) (Concept "TRUE"))
				(State (List (Variable "x") (Predicate "hungry")) (Concept "FALSE"))
				(State (List (Variable "x") (Predicate "thirsty")) (Concept "TRUE")))
			(Present
				(State (List (Variable "x") (Predicate "hungry")) (Concept "FALSE"))
				(State (List (Variable "x") (Predicate "thirsty")) (Concept "FALSE")))
			(Present
				(State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE")))
			)))
(test-assert "whats on first"
	(equal? (cog-execute! whos-on-first?)
		(Set
			(List (Concept "alice") (Variable "y"))
			(List (Concept "bob") (Variable "y"))
			(List (Concept "charlie") (Concept "at home")))))
(test-end tname)
(opencog-test-end)