(use-modules (opencog))
(define (say-hello atom)
(display "Hello, ")
(display (cog-name atom))
(display "!")
(newline)
atom
)
(define say-hello-to-linas
(ExecutionOutput
(GroundedSchema "scm: say-hello")
(List (Concept "Linas"))))
(Evaluation
(Predicate "is-a")
(List (Concept "Linas") (Concept "human"))
)
(Evaluation
(Predicate "is-a")
(List (Concept "Ben") (Concept "human"))
)
(define find-humans
(Bind
(Variable "$person")
(Evaluation
(Predicate "is-a")
(List (Variable "$person") (Concept "human")))
(ExecutionOutput
(GroundedSchema "scm: say-hello")
(List (Variable "$person")))))
(cog-execute! find-humans)