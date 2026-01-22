(use-modules (opencog))
(define human
	(Get
		(Inheritance (Variable "$H") (Concept "human"))))
(define human-implies-animal
	(Bind
		(Inheritance (Variable "$H") (Concept "human"))
		(Inheritance (Variable "$H") (Concept "animal"))))
(define human-implies-animal-stv
	(Bind
		(Inheritance (Variable "$H") (Concept "human"))
		(ExecutionOutput
			(GroundedSchema "scm: modify-stv")
			(List
				(Inheritance (Variable "$H") (Concept "human"))
				(Inheritance (Variable "$H") (Concept "animal"))))))
(define (scale-tv-strength val tv)
	(SimpleTruthValue (* val (cog-tv-mean tv)) (cog-tv-confidence tv)))
(define (modify-stv atom1 atom2)
	(cog-set-tv! atom2 (scale-tv-strength 0.3 (cog-tv atom1)))
	atom2
)
(InheritanceLink (stv 1 0.99)
	(Concept "Ben")
	(Concept "human"))
(InheritanceLink (stv 1 0.99)
	(Concept "Linas")
	(Concept "human"))