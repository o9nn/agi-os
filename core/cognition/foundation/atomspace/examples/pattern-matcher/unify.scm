(use-modules (opencog) (opencog exec))
(Inheritance (Concept "A") (Concept "B"))
(Inheritance (Concept "A") (Concept "C"))
(Inheritance (Concept "B") (Concept "C"))
(define unifier
(Get
(VariableList (Variable "$X") (Variable "$Y"))
(Identical
(Inheritance (Concept "A") (Variable "$Y"))
(Inheritance (Variable "$X") (Concept "B")))))
(cog-execute! unifier)
(define implicit-vars
(Get
(Identical
(Inheritance (Concept "A") (Variable "$Y"))
(Inheritance (Variable "$X") (Concept "B")))))
(cog-execute! implicit-vars)
(define three-way
(Get
(VariableList (Variable "$X") (Variable "$Y") (Variable "$Z"))
(And
(Identical
(Inheritance (Concept "A") (Variable "$Y"))
(Inheritance (Variable "$X") (Concept "B")))
(Identical
(Inheritance (Concept "B") (Variable "$Z"))
(Inheritance (Variable "$Y") (Concept "C")))
)))
(cog-execute! three-way)