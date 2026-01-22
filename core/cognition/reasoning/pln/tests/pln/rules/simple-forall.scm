(ForAllLink (stv 1 1)
(TypedVariableLink
(VariableNode "$X")
(TypeNode "ConceptNode"))
(EvaluationLink
(PredicateNode "is-concept")
(VariableNode "$X")
)
)
(ConceptNode "A")
(ForAllLink (stv 1 1)
(TypedVariableLink
(VariableNode "$X")
(TypeNode "PredicateNode"))
(EvaluationLink
(PredicateNode "is-predicate")
(VariableNode "$X")
)
)
(PredicateNode "P")
(ForAllLink (stv 1 1)
(VariableList
(TypedVariableLink
(VariableNode "$X")
(TypeNode "ConceptNode"))
(TypedVariableLink
(VariableNode "$Y")
(TypeNode "PredicateNode")))
(EvaluationLink
(PredicateNode "are-concept-and-predicate")
(ListLink
(VariableNode "$X")
(VariableNode "$Y"))
)
)
(ForAllLink (stv 1 1)
(TypedVariableLink
(VariableNode "$X")
(TypeNode "ConceptNode"))
(ImplicationLink
(EvaluationLink
(PredicateNode "P")
(VariableNode "$X"))
(EvaluationLink
(PredicateNode "P")
(VariableNode "$X"))))