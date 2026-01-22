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
(cog-execute! pexec)
(define pmany
(ExecuteThreaded
(Number 3)
(Set
(map
(lambda (n)
(Meet
(TypedVariable (Variable "X") (Type 'Concept))
(Inheritance (Variable "X") (Concept "mineral"))))
(iota 5))
(map
(lambda (n)
(Meet
(TypedVariable (Variable "X") (Type 'Concept))
(Inheritance (Variable "X") (Concept "plant"))))
(iota 5))
)))
(cog-execute! pmany)