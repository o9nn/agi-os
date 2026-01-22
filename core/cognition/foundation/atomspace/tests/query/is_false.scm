(Concept "A" (stv 0 1))
(Concept "B")
(define query
(Get
(TypedVariable
(Variable "$C")
(Type 'Concept))
(Present (Variable "$C"))))
(define false-query
(Get
(TypedVariable
(Variable "$C")
(Type 'Concept))
(And
(Present (Variable "$C"))
(IsFalse (Variable "$C")))))