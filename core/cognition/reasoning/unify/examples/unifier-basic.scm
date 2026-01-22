(use-modules (opencog) (opencog exec))
(use-modules (opencog unify))
(Inheritance (Concept "A") (Concept "B"))
(define joiner
	(Unifier
		(Inheritance (Variable "$X") (Concept "B"))
		(Inheritance (Concept "A") (Variable "$Y"))
		(List (Variable "$X") (Variable "$Y"))))
(cog-execute! joiner)
(define join-and-make
	(Unifier
		(Inheritance (Variable "$X") (Concept "B"))
		(Inheritance (Concept "A") (Variable "$Y"))
		(Evaluation
			(Predicate "put it together")
			(List (Variable "$X") (Variable "$Y")))))
(cog-execute! join-and-make)
(define ident
	(Bind
		(Identical
			(Inheritance (Variable "$X") (Concept "B"))
			(Inheritance (Concept "A") (Variable "$Y")))
		(Evaluation
			(Predicate "put it together")
			(List (Variable "$X") (Variable "$Y")))))
(cog-execute! ident)