(use-modules (opencog) (opencog exec))
(Inheritance (Concept "B") (Concept "foo"))
(define query
(Bind
(TypedVariable (Variable "$C-7a4842c1") (Type "Concept"))
(And
(Present
(Inheritance (Concept "A") (Concept "B"))
(Inheritance (Concept "B") (Variable "$C-7a4842c1")))
(Not (Identical (Variable "$C-7a4842c1") (Concept "A"))))
(Execution
(Schema "scm: fc-deduction-formula")
(List
(Inheritance (Concept "A") (Variable "$C-7a4842c1"))
(Inheritance (Concept "A") (Concept "B"))
(Inheritance (Concept "B") (Variable "$C-7a4842c1"))))))
(define expected
(Set
(Execution
(Schema "scm: fc-deduction-formula")
(List
(Inheritance (Concept "A") (Concept "foo"))
(Inheritance (Concept "A") (Concept "B"))
(Inheritance (Concept "B") (Concept "foo"))))))