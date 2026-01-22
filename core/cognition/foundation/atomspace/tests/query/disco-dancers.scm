(use-modules (opencog))
(use-modules (opencog exec))
(Concept "alice")
(Concept "bob")
(define variables (VariableList
(TypedVariable
(Variable "person1")
(Type "ConceptNode"))
(TypedVariable
(Variable "person2")
(Type "ConceptNode"))))
(define target (Not (Identical
(Variable "person1")
(Variable "person2") )))
(define (get-dancers) (Get variables target))