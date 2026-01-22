(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(ConceptNode "baseball")
)
)
(DefineLink
(DefinedPredicate "Does Tom like X?")
(SatisfactionLink
(VariableNode "$X")
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(VariableNode "$X")
)
)
)
)
(MemberLink
(ConceptNode "baseball")
(ConceptNode "Does Tom like X?")
)