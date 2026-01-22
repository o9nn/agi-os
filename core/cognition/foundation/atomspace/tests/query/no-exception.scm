(define bl
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$A-648d8b51")
(TypeChoice
(TypeNode "LambdaLink")
(TypeNode "PredicateNode")
)
)
(TypedVariableLink
(VariableNode "$A-4c2b3a57")
(TypeChoice
(TypeNode "LambdaLink")
(TypeNode "PredicateNode")
)
)
)
(AndLink
(ImplicationLink
(VariableNode "$A-4c2b3a57")
(PredicateNode "T")
)
(VariableNode "$A-648d8b51")
(ImplicationLink
(VariableNode "$A-648d8b51")
(VariableNode "$A-4c2b3a57")
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: crisp-modus-ponens-formula")
(ListLink
(ExecutionOutputLink
(GroundedSchemaNode "scm: crisp-modus-ponens-formula")
(ListLink
(VariableNode "$A-648d8b51")
(ImplicationLink
(VariableNode "$A-648d8b51")
(VariableNode "$A-4c2b3a57")
)
(VariableNode "$A-4c2b3a57")
)
)
(ImplicationLink
(ExecutionOutputLink
(GroundedSchemaNode "scm: crisp-modus-ponens-formula")
(ListLink
(VariableNode "$A-648d8b51")
(ImplicationLink
(VariableNode "$A-648d8b51")
(VariableNode "$A-4c2b3a57")
)
(VariableNode "$A-4c2b3a57")
)
)
(PredicateNode "T")
)
(PredicateNode "T")
)
)
)
)
(define (crisp-modus-ponens-formula A AB B)
(let (  (sA (cog-mean A))
(cA (cog-confidence A))
(sAB (cog-mean AB))
(cAB (cog-confidence AB)))
(if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
(cog-set-tv! B (stv 1 1)))))
(Implication (stv 1 1)
(Predicate "R")
(Predicate "S"))
(Implication (stv 1 1)
(Predicate "S")
(Predicate "T"))