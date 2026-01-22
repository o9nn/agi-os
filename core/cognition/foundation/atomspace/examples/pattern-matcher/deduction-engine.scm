(use-modules (opencog))
(use-modules (opencog exec))
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(ConceptNode "baseball")
)
)
(RuleLink
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(VariableNode "$X")))
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Bill")
(VariableNode "$X"))))
(BindLink
(VariableNode "$X")
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(VariableNode "$X")))
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Bill")
(VariableNode "$X"))))
(define implication
(PutLink
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Bill")
(VariableNode "$Y")))
(GetLink
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Tom")
(VariableNode "$X"))))))
(cog-execute! implication)
(DefineLink
(DefinedPredicateNode "Does Bill like X?")
(SatisfactionLink
(VariableNode "$X")
(EvaluationLink
(PredicateNode "likes")
(ListLink
(ConceptNode "Bill")
(VariableNode "$X")))))
(MemberLink
(ConceptNode "baseball")
(DefinedPredicateNode "Does Bill like X?")
)
(cog-evaluate! (DefinedPredicateNode "Does Bill like X?"))
(define get-impl
(GetLink
(VariableList
(TypedVariableLink (VariableNode "$fpred") (TypeNode "PredicateNode"))
(TypedVariableLink (VariableNode "$tpred") (TypeNode "PredicateNode"))
(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
(TypedVariableLink (VariableNode "$B") (TypeNode "ConceptNode"))
(TypedVariableLink (VariableNode "$V") (TypeNode "VariableNode"))
)
(QuoteLink
(RuleLink
(UnquoteLink
(EvaluationLink
(VariableNode "$fpred")
(ListLink
(VariableNode "$A")
(VariableNode "$V"))))
(UnquoteLink
(EvaluationLink
(VariableNode "$tpred")
(ListLink
(VariableNode "$B")
(VariableNode "$V"))))))))
(define pg-impl
(PutLink
(VariableList
(VariableNode "$tp")
(VariableNode "$fp")
(VariableNode "$aaa")
(VariableNode "$bbb")
(VariableNode "$vvv")
)
(QuoteLink
(PutLink
(UnquoteLink
(EvaluationLink
(VariableNode "$tp")
(ListLink
(VariableNode "$bbb")
(VariableNode "$vvv"))))
(GetLink
(UnquoteLink
(EvaluationLink
(VariableNode "$fp")
(ListLink
(VariableNode "$aaa")
(VariableNode "$vvv")))))))
get-impl))
(define b-impl
(BindLink
(VariableList
(TypedVariableLink (VariableNode "$fpred") (TypeNode "PredicateNode"))
(TypedVariableLink (VariableNode "$tpred") (TypeNode "PredicateNode"))
(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
(TypedVariableLink (VariableNode "$B") (TypeNode "ConceptNode"))
(TypedVariableLink (VariableNode "$V") (TypeNode "VariableNode"))
)
(QuoteLink
(RuleLink
(UnquoteLink
(EvaluationLink
(VariableNode "$fpred")
(ListLink
(VariableNode "$A")
(VariableNode "$V"))))
(UnquoteLink
(EvaluationLink
(VariableNode "$tpred")
(ListLink
(VariableNode "$B")
(VariableNode "$V"))))))
(BindLink
(VariableNode "$V")
(EvaluationLink
(VariableNode "$fpred")
(ListLink
(VariableNode "$A")
(VariableNode "$V")))
(EvaluationLink
(VariableNode "$tpred")
(ListLink
(VariableNode "$B")
(VariableNode "$V"))))))
*unspecified*