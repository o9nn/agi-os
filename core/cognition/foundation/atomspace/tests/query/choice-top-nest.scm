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
(ConceptNode "Mary")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Phillip")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Milton")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Charlie")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Chayim")
(ConceptNode "ways and means")
)
(MemberLink
(ConceptNode "Stuart")
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
(MemberLink
(ConceptNode "Mary")
(ConceptNode "Page")
)
(MemberLink
(ConceptNode "Phillip")
(ConceptNode "Secretary")
)
(EvaluationLink
(PredicateNode "involved")
(ListLink
(ConceptNode "Milton")
(ConceptNode "Business")
)
)
(EvaluationLink
(PredicateNode "involved")
(ListLink
(ConceptNode "Charlie")
(ConceptNode "Industry")
)
)
(EvaluationLink
(PredicateNode "involved")
(ListLink
(ConceptNode "Chayim")
(ConceptNode "Banking")
)
)
(EvaluationLink
(PredicateNode "involved")
(ListLink
(ConceptNode "Stuart")
(ConceptNode "Diletant")
)
)
(define (top-nest)
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
(EvaluationLink
(PredicateNode "involved")
(ChoiceLink
(ListLink
(VariableNode "$x")
(ConceptNode "Business")
)
(ListLink
(VariableNode "$x")
(ConceptNode "Industry")
)
(ListLink
(VariableNode "$x")
(ConceptNode "Banking")
)
)
)
)
)
(VariableNode "$x")
)
)
(define (top-nest-bad)
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
(ChoiceLink
(MemberLink
(VariableNode "$x")
(ConceptNode "Page")
)
(MemberLink
(VariableNode "$x")
(ConceptNode "Secretary")
)
)
)
)
(VariableNode "$x")
)
)