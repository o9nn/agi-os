(RuleLink (stv .99 .99)
    (AndLink
        (InheritanceLink
            (VariableNode "$x")
            (ConceptNode "American"))
        (InheritanceLink
            (VariableNode "$y")
            (ConceptNode "weapon"))
        (EvaluationLink
            (PredicateNode "sell")
            (ListLink
                (VariableNode "$x")
                (VariableNode "$y")
                (VariableNode "$z")))
        (InheritanceLink
            (VariableNode "$z")
            (ConceptNode "hostile")))
    (InheritanceLink
        (VariableNode "$x")
        (ConceptNode "criminal")))
(define (query_rule_bad)
    (BindLink (stv 1 1)
        (VariableNode "$x")
        (InheritanceLink
            (QuoteLink
                (VariableNode "$x")
            )
            (ConceptNode "criminal")
        )
        (InheritanceLink
            (VariableNode "$x")
            (ConceptNode "criminal")
        )
    )
)
(define query_rule_good
    (BindLink (stv 1 1)
        (VariableNode "$zzz")
        (InheritanceLink
            (VariableNode "$zzz")
            (ConceptNode "criminal")
        )
        (InheritanceLink
            (VariableNode "$zzz")
            (ConceptNode "criminal")
        )
    )
)