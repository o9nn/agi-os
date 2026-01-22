(use-modules (opencog))
(use-modules (opencog exec))
(MemberLink
(ConceptNode "Tom")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Joe")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Hank")
(ConceptNode "ways and means")
)
(ListLink
(MemberLink
(ConceptNode "Tom")
(ConceptNode "Senator")
)
)
(ListLink
(MemberLink
(ConceptNode "Joe")
(ConceptNode "Representative")
)
)
(ListLink
(MemberLink
(ConceptNode "Hank")
(ConceptNode "CEO")
)
)
(define (embed)
(BindLink
(AndLink
(MemberLink
(VariableNode "$x")
(ConceptNode "ways and means")
)
(ListLink
(ChoiceLink
(MemberLink
(VariableNode "$x")
(ConceptNode "Senator")
)
(MemberLink
(VariableNode "$x")
(ConceptNode "Representative")
)
)
)
)
(VariableNode "$x")
)
)