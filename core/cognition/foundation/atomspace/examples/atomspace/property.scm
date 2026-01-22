(use-modules (opencog) (opencog exec))
(Concept "asdf")
(Concept "qwerty")
(Predicate "truthiness")
(State (List (Concept "asdf") (Predicate "truthiness"))
(Number 0.5))
(State (List (Concept "qwerty") (Predicate "truthiness"))
(Number 0.5))
(cog-execute!
(Get
(State (List (Concept "qwerty") (Predicate "truthiness"))
(Variable "$n"))))
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
(cog-execute!
(ExecutionOutput
(DefinedSchema "get property")
(List (Concept "qwerty") (Predicate "truthiness"))
))