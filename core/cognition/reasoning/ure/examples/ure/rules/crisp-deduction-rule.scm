(define crisp-deduction-rule
(BindLink
(VariableSet
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")
)
(AndLink
(PresentLink
(ImplicationLink
(VariableNode "$A")
(VariableNode "$B")
)
(ImplicationLink
(VariableNode "$B")
(VariableNode "$C")
)
)
(NotLink
(IdenticalLink
(VariableNode "$A")
(VariableNode "$C")
)
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: crisp-deduction")
(ListLink
(ImplicationLink
(VariableNode "$A")
(VariableNode "$C"))
(ImplicationLink
(VariableNode "$A")
(VariableNode "$B"))
(ImplicationLink
(VariableNode "$B")
(VariableNode "$C"))))))
(define (crisp-deduction AC AB BC)
(let
((sAB (cog-mean AB))
(cAB (cog-confidence AB))
(sBC (cog-mean BC))
(cBC (cog-confidence BC)))
(if (and (>= sAB 0.5) (>= cAB 0.5) (>= sBC 0.5) (>= cBC 0.5))
(cog-set-tv! AC (stv 1 1)))))
(define crisp-deduction-rule-name
(DefinedSchemaNode "crisp-deduction-rule"))
(DefineLink
crisp-deduction-rule-name
crisp-deduction-rule)