(use-modules ((opencog exec)))
(InheritanceLink (ConceptNode "Ben") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Linas") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Sparky") (ConceptNode "dog"))
(define is-human
(GetLink (InheritanceLink (VariableNode "$H") (ConceptNode "human"))))
(define is-something
(GetLink
(VariableList
(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
(VariableNode "$B"))
(InheritanceLink (VariableNode "$A") (VariableNode "$B"))))
(define is-nothing
(GetLink
(TypedVariableLink (VariableNode "$H") (TypeChoice))
(InheritanceLink (VariableNode "$H") (ConceptNode "human"))))
(define is-query
(GetLink
(VariableNode "$B")
(InheritanceLink (VariableNode "$H") (VariableNode "$B"))))
(define g-take-contain
(GetLink
(VariableList
(TypedVariableLink
(VariableNode "$X")
(TypeNode "ConceptNode")
)
(TypedVariableLink
(VariableNode "$Z")
(TypeNode "ConceptNode")
)
)
(AndLink
(EvaluationLink
(PredicateNode "take")
(ListLink
(VariableNode "$X")
(ConceptNode "treatment-1")
)
)
(EvaluationLink
(PredicateNode "contain")
(ListLink
(ConceptNode "treatment-1")
(VariableNode "$Z")
)
)
)
)
)
(EvaluationLink (stv 1 1)
(PredicateNode "take")
(ListLink
(ConceptNode "John")
(ConceptNode "treatment-1")))
(EvaluationLink (stv 1 1)
(PredicateNode "contain")
(ListLink
(ConceptNode "treatment-1")
(ConceptNode "compound-A")))