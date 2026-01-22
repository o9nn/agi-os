(use-modules (opencog) (opencog exec))
(define (truf x)
(format #t "Perform condition check on: ~A\n" x)
(cond
((equal? x (Concept "good")) (SimpleTruthValue 1 1))
((equal? x (Concept "bad")) (SimpleTruthValue 0 1))
(else (throw 'whats-up-jack "you done it wrong"))
)
)
(define (konsekwens x)
(format #t "Take action on the atom: ~A\n" x)
(Implication x x)
)
(ContextLink
(Concept "situation")
(Evaluation
(GroundedPredicate "scm: truf")
(List (Concept "good"))
)
(ExecutionOutput
(GroundedSchema "scm: konsekwens")
(List (Concept "acceptance"))
)
)
(ContextLink
(Concept "predicament")
(Evaluation
(GroundedPredicate "scm: truf")
(List (Concept "bad"))
)
(ExecutionOutput
(GroundedSchema "scm: konsekwens")
(List (Concept "rejection"))
)
)
(define do-things
(Bind
(VariableList
(Variable "$cxt")
(Variable "$condition")
(Variable "$action")
)
(And
(ContextLink
(Variable "$cxt")
(Variable "$condition")
(Variable "$action")
)
(Variable "$condition")
)
(Variable "$action")
)
)