(use-modules (opencog))
(use-modules (opencog exec))
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
)
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
)
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
)
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(MemberLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
)
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(MemberLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
(SubsetLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(ConceptNode "this one")
(ConceptNode "thing two")
)
)
)
)
(define (repeat-same)
(BindLink
(VariableNode "$x")
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
)
(VariableNode "$x")
)
)
(define (repeat-different)
(BindLink
(VariableNode "$x")
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(MemberLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
)
(VariableNode "$x")
)
)
(define (repeat-diff-thrice)
(BindLink
(VariableNode "$x")
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(MemberLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(SubsetLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
)
(VariableNode "$x")
)
)
(define (repeat-thrice)
(BindLink
(VariableNode "$x")
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
)
(VariableNode "$x")
)
)
(define (repeat-once)
(BindLink
(VariableNode "$x")
(ListLink
(ListLink
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
(EvaluationLink
(PredicateNode "this way")
(ListLink
(VariableNode "$x")
(ConceptNode "thing two")
)
)
)
)
(VariableNode "$x")
)
)