(SetLink
(ConceptNode "x")
(ConceptNode "y")
(ConceptNode "z")
(ConceptNode "w")
)
(SetLink
(ConceptNode "p")
(ConceptNode "q")
(ConceptNode "r")
(SetLink
(ConceptNode "s")
(ConceptNode "t")
(ConceptNode "u"))
)
(SetLink
(ConceptNode "a")
(ConceptNode "b")
(ConceptNode "c")
(SetLink
(ConceptNode "a")
(ConceptNode "b")
(ConceptNode "c"))
)
(define (exhaust)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d"))
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d"))
)
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
)
)
)
(define (exhaust-2)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
(VariableNode "$e")
(VariableNode "$f")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(SetLink
(VariableNode "$d")
(VariableNode "$e")
(VariableNode "$f"))
))
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
(VariableNode "$e")
(VariableNode "$f"))
)
)
(define (exhaust-3)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
(VariableNode "$e")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(SetLink
(VariableNode "$c")
(VariableNode "$d")
(VariableNode "$e"))
))
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
(VariableNode "$e")
)
)
)
(define (exhaust-4)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(SetLink
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d"))
))
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(VariableNode "$d")
)
)
)
(define (exhaust-5)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c"))
))
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c")
)
)
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(ConceptNode "a")
(ConceptNode "a")
)
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(ConceptNode "b")
(ConceptNode "b")
)
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(ConceptNode "c")
(ConceptNode "c")
)
)
(define (exhaust-eq-12)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b")
(VariableNode "$c1")
(VariableNode "$c2")
(VariableNode "$e")
(VariableNode "$f")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c1")
(SetLink
(VariableNode "$c2")
(VariableNode "$e")
(VariableNode "$f"))
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(VariableNode "$c1")
(VariableNode "$c2")
)
)
)
(ListLink
(VariableNode "$a")
(VariableNode "$b")
(VariableNode "$c1")
(VariableNode "$c2")
(VariableNode "$e")
(VariableNode "$f")
)
)
)
(define (exhaust-eq-6)
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$a") (TypeNode "ConceptNode"))
(VariableNode "$b1")
(VariableNode "$b2")
(VariableNode "$c1")
(VariableNode "$c2")
(VariableNode "$f")
)
(AndLink
(SetLink
(VariableNode "$a")
(VariableNode "$b1")
(VariableNode "$c1")
(SetLink
(VariableNode "$c2")
(VariableNode "$b2")
(VariableNode "$f"))
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(VariableNode "$b1")
(VariableNode "$b2")
)
)
(EvaluationLink
(PredicateNode "equal")
(ListLink
(VariableNode "$c1")
(VariableNode "$c2")
)
)
)
(ListLink
(VariableNode "$a")
(VariableNode "$b1")
(VariableNode "$b2")
(VariableNode "$c1")
(VariableNode "$c2")
(VariableNode "$f")
)
)
)