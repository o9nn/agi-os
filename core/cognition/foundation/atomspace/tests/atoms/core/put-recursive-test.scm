(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "put-recursive-test")
(test-begin tname)
(Define
(DefinedSchema "is-a relation")
(Lambda
(VariableList (Variable "this") (Variable "that"))
(SequentialOr
(Inheritance (Variable "this") (Variable "that"))
(SequentialAnd
(Inheritance (Variable "this") (Variable "middle"))
(Put (DefinedSchema "is-a relation")
(List (Variable "middle") (Variable "that")))))))
(define is-it
(Put
(DefinedSchema "is-a relation")
(List (Concept "human") (Concept "chordate"))))
(cog-execute! is-it)
(test-assert "no crash" #t)
(test-end tname)
(opencog-test-end)