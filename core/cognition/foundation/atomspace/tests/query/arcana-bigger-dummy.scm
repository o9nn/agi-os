(use-modules (opencog))
(use-modules (opencog exec))
(define stfu
(SetLink
(ListLink
(ImplicationLink
(ListLink
(ConceptNode "I")
(ConceptNode "love")
(ConceptNode "you"))
(ConceptNode "blrable"))
(ConceptNode "blrable"))))
(define bigger-dummy
(GetLink
(VariableList
(TypedVariable (VariableNode "$whole") (Type "ImplicationLink"))
(VariableNode "$impl"))
(Identical
(VariableNode "$whole")
(ImplicationLink
(ListLink
(ConceptNode "I")
(ConceptNode "love")
(ConceptNode "you"))
(VariableNode "$impl"))
)
))