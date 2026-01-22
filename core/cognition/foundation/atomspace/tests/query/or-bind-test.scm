(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "or-link-bind-link-test")
(test-begin tname)
(Evaluation
(Predicate "Attends college")
(List
(Concept "John")))
(define get-student
(Get
(Or
(Evaluation
(Predicate "Attends school")
(List
(Variable "$X")))
(Evaluation
(Predicate "Attends college")
(List
(Variable "$X"))))))
(test-assert "get John"
(equal?  (cog-execute! get-student) (Set (Concept "John"))))
(define rewrite-student
(Bind
(Or
(Evaluation
(Predicate "Attends school")
(List
(Variable "$X")))
(Evaluation
(Predicate "Attends college")
(List
(Variable "$X"))))
(Evaluation
(Predicate "Is student")
(List
(Variable "$X")))))
(test-assert "John is student"
(equal?  (cog-execute! rewrite-student) (Set
(Evaluation
(Predicate "Is student")
(List (Concept "John"))))))
(test-end tname)
(opencog-test-end)