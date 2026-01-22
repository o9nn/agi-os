(define plus-zero-rule
(DefinedSchemaNode "plus-zero-rule-name"))
(DefineLink
plus-zero-rule
(BindLink
(GlobNode "$op")
(ChoiceLink
(PlusLink
(GlobNode "$op")
(NumberNode 0))
(PlusLink
(NumberNode 0)
(GlobNode "$op")))
(ReductToLink
(ChoiceLink
(PlusLink
(GlobNode "$op")
(NumberNode 0))
(PlusLink
(NumberNode 0)
(GlobNode "$op")))
(PlusLink (GlobNode "$op")))))