(use-modules (opencog) (opencog exec))
(Inheritance (Concept "mouse") (Concept "animal"))
(Inheritance (Concept "mouse") (Concept "mammal"))
(define get-mouse
(GetLink (Variable "x")
(And
(Present (Inheritance (Variable "x") (Concept "animal")))
(Satisfaction (Variable "y")
(And
(Present (Inheritance (Variable "x") (Variable "y")))
(Equal (Variable "y") (Concept "mammal"))
))
))
)
(Inheritance (Concept "snail") (Concept "animal"))
(Inheritance (Concept "snail") (Concept "gastropod"))
(define get-snail
(GetLink (TypedVariable (Variable "x") (Type 'ConceptNode))
(And
(Present (Inheritance (Variable "x") (Concept "animal")))
(Satisfaction (TypedVariable (Variable "y") (Type 'ConceptNode))
(And
(Present (Inheritance (Variable "x") (Variable "y")))
(Not (Equal (Variable "y") (Concept "animal")))
(Not (Equal (Variable "y") (Concept "mammal")))
(Not (Equal (Variable "y") (Concept "cephalopod")))
))
))
)
(Inheritance (Concept "squid") (Concept "animal"))
(Inheritance (Concept "squid") (Concept "cephalopod"))
(define get-mouse-pair
(GetLink (VariableList
(TypedVariable (Variable "x") (Type 'ConceptNode))
(TypedVariable (Variable "y") (Type 'ConceptNode)))
(And
(Present (Inheritance (Variable "x") (Concept "animal")))
(Present (Inheritance (Variable "x") (Concept "mammal")))
(Present (Inheritance (Variable "y") (Concept "animal")))
(Not (Equal (Variable "x") (Variable "y")))
(Satisfaction (Variable "z")
(And
(Present (Inheritance (Variable "x") (Variable "z")))
(Present (Inheritance (Variable "y") (Variable "z")))
))
))
)
*unspecified*