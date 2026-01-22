(define deduction-ab-substitute-1
(BindLink
(VariableSet
(TypedVariableLink
(VariableNode "$C")
(TypeNode "ConceptNode")))
(AndLink
(InheritanceLink
(ConceptNode "Animal")
(VariableNode "$C")
)
(NotLink
(IdenticalLink
(ConceptNode "Cat")
(VariableNode "$C")
)
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: fc-deduction")
(ListLink
(InheritanceLink
(ConceptNode "Cat")
(ConceptNode "Animal"))
(InheritanceLink
(ConceptNode "Animal")
(VariableNode "$C"))
(InheritanceLink
(ConceptNode "Cat")
(VariableNode "$C"))))))
(define deduction-ab-substitute-2
(BindLink
(VariableSet
(TypedVariableLink
(VariableNode "$A")
(TypeNode "ConceptNode")))
(AndLink
(InheritanceLink
(VariableNode "$A")
(ConceptNode  "Cat")
)
(NotLink
(IdenticalLink
(VariableNode "$A")
(ConceptNode  "Animal")
)
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: fc-deduction")
(ListLink
(InheritanceLink
(VariableNode "$A")
(ConceptNode  "Cat"))
(InheritanceLink
(ConceptNode  "Cat")
(ConceptNode  "Animal"))
(InheritanceLink
(VariableNode "$A")
(ConceptNode  "Animal"))))))