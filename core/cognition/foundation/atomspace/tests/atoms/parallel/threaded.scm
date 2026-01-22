(use-modules (opencog) (opencog exec))
(Inheritance (Concept "rock") (Concept "mineral"))
(Inheritance (Concept "flower") (Concept "plant"))
(Inheritance (Concept "cat") (Concept "animal"))
(define pexec
	(ExecuteThreaded
		(Set
			(Meet
				(TypedVariable (Variable "X") (Type 'Concept))
				(Inheritance (Variable "X") (Concept "mineral")))
			(Meet
				(TypedVariable (Variable "X") (Type 'Concept))
				(Inheritance (Variable "X") (Concept "plant"))))
	))
(define pmany
	(ExecuteThreaded
		(Number 2)
		(Set
			(map
				(lambda (n)
					(Query
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "mineral"))
						(List (Number n) (Variable "X"))))
				(iota 50))
			(map
				(lambda (n)
					(Query
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "plant"))
						(List (Number n) (Variable "X"))))
				(iota 50))
	)))