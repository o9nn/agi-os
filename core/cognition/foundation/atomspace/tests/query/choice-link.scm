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
(MemberLink
(ConceptNode "Tom")
(ConceptNode "Senator")
)
(MemberLink
(ConceptNode "Joe")
(ConceptNode "Representative")
)
(MemberLink
(ConceptNode "Hank")
(ConceptNode "CEO")
)
(define (basic)
(BindLink
(AndLink
(MemberLink
(VariableNode "$x")
(ConceptNode "ways and means")
)
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
(VariableNode "$x")
)
)