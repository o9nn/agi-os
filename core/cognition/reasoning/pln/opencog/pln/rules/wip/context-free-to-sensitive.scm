(define context-free-to-sensitive-rule
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$C"))
(AndLink
(VariableNode "$C")
(VariableNode "$A"))
(ExecutionOutputLink
(GroundedSchemaNode "scm: context-free-to-sensitive-formula")
(ListLink
(ContextLink
(VariableNode "$C")
(VariableNode "$A"))
(AndLink
(VariableNode "$C")
(VariableNode "$A"))))))
(define (context-free-to-sensitive-formula Context CA)
(cog-set-tv! Context
(cog-new-stv (cog-mean CA) (cog-confidence CA))))
(define context-free-to-sensitive-rule-name
(DefinedSchemaNode "context-free-to-sensitive-rule"))
(DefineLink
context-free-to-sensitive-rule-name
context-free-to-sensitive-rule)