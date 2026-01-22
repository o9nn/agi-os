(EvaluationLink
(ConceptNode "arkle")
(ConceptNode "barkle")
(ConceptNode "curry"))
(EvaluationLink
(ConceptNode "glib")
(ConceptNode "blab"))
(define bnd
(BindLink
(AndLink
(VariableNode "$lnk")
(EvaluationLink
(VariableNode "$a")
(VariableNode "$b"))
(IdenticalLink
(VariableNode "$lnk")
(EvaluationLink
(VariableNode "$a")
(VariableNode "$b")
)
))
(VariableNode "$lnk")
)
)