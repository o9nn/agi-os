(Evaluation
(Predicate "has-color")
(List
(Concept "apple")
(Concept "green")))
(Evaluation
(Predicate "has-color")
(List
(Concept "banana")
(Concept "yellow")))
(Evaluation
(Predicate "has-color")
(List
(Concept "strawberry")
(Concept "red")))
(define find-fruit
(Get
(Evaluation
(Predicate "has-color")
(List
(Variable "$fruit")
(Choice
(Concept "red")
(Concept "green"))))))
(define find-fruit
(Get
(Choice
(Evaluation
(Predicate "has-color")
(List
(Variable "$fruit")
(Concept "red")))
(Evaluation
(Predicate "has-color")
(List
(Variable "$fruit")
(Concept "green"))))))
(cog-execute! find-fruit)