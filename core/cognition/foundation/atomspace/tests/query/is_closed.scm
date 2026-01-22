(Inheritance
(Concept "A")
(Concept "B"))
(Inheritance
(Concept "A")
(Variable "$freevar"))
(define query
(Get
(TypedVariable
(Variable "$I")
(Type 'Inheritance))
(Present (Variable "$I"))))
(define closed-query
(Get
(TypedVariable
(Variable "$I")
(Type 'Inheritance))
(And
(Present (Variable "$I"))
(IsClosed (Variable "$I")))))