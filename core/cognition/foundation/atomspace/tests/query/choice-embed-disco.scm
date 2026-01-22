(use-modules (opencog))
(use-modules (opencog exec))
(MemberLink
(ConceptNode "ways and means")
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(MemberLink
(ConceptNode "ways and means")
(EvaluationLink
(PredicateNode "that way")
(ListLink
(ConceptNode "thing one")
(ConceptNode "that too")
)
)
)
(MemberLink
(ConceptNode "ways and means")
(EvaluationLink
(PredicateNode "third way")
(ListLink
(ConceptNode "thing one")
(ConceptNode "thing two")
)
)
)
(define (embed-disco)
(BindLink
(MemberLink
(ConceptNode "ways and means")
(ChoiceLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
(EvaluationLink
(PredicateNode "that way")
(ListLink
(ConceptNode "thing one")
(VariableNode "$x")
)
)
)
)
(VariableNode "$x")
)
)