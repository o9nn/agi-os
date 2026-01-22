(define and-elimination-rule
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$B"))
(AndLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")))
(ExecutionOutputLink
(GroundedSchemaNode "scm: and-elimination-formula")
(ListLink
(AndLink
(VariableNode "$A")
(VariableNode "$B"))
(VariableNode "$A")
(VariableNode "$B")))))
(define (and-elimination-formula AB A B)
(cog-set-tv!
A
(and-elimination-side-effect-free-formula AB))
(cog-set-tv!
B
(and-elimination-side-effect-free-formula AB))
)
(define (and-elimination-side-effect-free-formula AB)
(let
((sAB (cog-mean AB))
(cAB (cog-confidence AB)))
(stv (expt sAB 0.5) (/ cAB 1.42))))
(define and-elimination-rule-name (DefinedSchemaNode "and-elimination-rule"))
(DefineLink and-elimination-rule-name and-elimination-rule)