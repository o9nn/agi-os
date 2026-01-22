(Concept "A" (stv 1 1))
(Concept "B")
(define query
(Get
(TypedVariable
(Variable "$C")
(Type 'Concept))
(Present (Variable "$C"))))
(define true-query
(Get
(TypedVariable
(Variable "$C")
(Type 'Concept))
(And
(Present (Variable "$C"))
(IsTrue (Variable "$C")))))