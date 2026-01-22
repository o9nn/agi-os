(define imp
(SatisfactionLink
(TypedVariable
(VariableNode "$A")
(TypeNode "PredicateNode"))
(AndLink
(VariableNode "$A")
(RuleLink
(VariableNode "$A")
(QuoteLink
(InheritanceLink
(VariableNode "$x")
(ConceptNode "criminal")))))))
(define (sat-imp) (cog-evaluate! imp))