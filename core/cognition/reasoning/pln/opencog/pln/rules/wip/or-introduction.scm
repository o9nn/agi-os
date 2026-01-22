(define or-introduction-rule
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$A")
(TypeChoice
(TypeNode "PredicateNode")
(TypeNode "ConceptNode")))
(TypedVariableLink
(VariableNode "$B")
(TypeChoice
(TypeNode "PredicateNode")
(TypeNode "ConceptNode"))))
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(NotLink
(IdenticalLink
(VariableNode "$A")
(VariableNode "$B"))))
(ExecutionOutputLink
(GroundedSchemaNode "scm: or-introduction-formula")
(ListLink
(VariableNode "$A")
(VariableNode "$B")))))
(define (or-introduction-formula A B)
(cog-set-tv!
(OrLink A B)
(or-introduction-side-effect-free-formula A B))
)
(define (or-introduction-side-effect-free-formula A B)
(let
((sA (cog-mean A))
(sB (cog-mean B))
(cA (cog-confidence A))
(cB (cog-confidence B)))
(stv (- (+ sA sB) (* sA sB)) (min cA cB))))
(define or-introduction-rule-name
(DefinedSchemaNode "or-introduction-rule"))
(DefineLink
or-introduction-rule-name
or-introduction-rule)