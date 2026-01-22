(use-modules (opencog) (opencog exec))
(State (List (Concept "asdf") (Predicate "truthiness"))
(Number 0.5))
(State (List (Concept "qwerty") (Predicate "truthiness"))
(Number 0.5))
(State (List (Concept "qwerty") (Predicate "truthiness"))
(Number 0.6))
(DefineLink
(DefinedSchema "get property")
(Lambda
(VariableList (Variable "$atom") (Variable "$property"))
(Get
(Variable "$n")
(State (List (Variable "$atom") (Variable "$property"))
(Variable "$n"))
)))
(define exo
(ExecutionOutput
(DefinedSchema "get property")
(List (Concept "qwerty") (Predicate "truthiness"))
))