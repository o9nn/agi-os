(use-modules (opencog))
(use-modules (opencog exec))
(InheritanceLink (ConceptNode "dog") (ConceptNode "cat"))
(InheritanceLink (ConceptNode "cat") (ConceptNode "dog"))
(InheritanceLink (ConceptNode "cat") (ConceptNode "mammal"))
(InheritanceLink (ConceptNode "dog") (ConceptNode "mammal"))
(InheritanceLink (ConceptNode "mammal") (ConceptNode "animal"))
(define pln-rule-deduction
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "ConceptNode")
            )
            (TypedVariableLink
                (VariableNode "$B")
                (TypeNode "ConceptNode")
            )
            (TypedVariableLink
                (VariableNode "$C")
                (TypeNode "ConceptNode")
            )
        )
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$B")
            )
            (InheritanceLink
                (VariableNode "$B")
                (VariableNode "$C")
            )
            (NotLink
                (EvaluationLink
                    (GroundedPredicateNode "scm: cog-equal?")
                    (ListLink
                        (VariableNode "$A")
                        (VariableNode "$C")
                    )
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-xxx")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
                (ListLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
    )
)
(define (cog-equal? atom-1 atom-2)
    (if (equal? atom-1 atom-2)
        (stv 1 1)
        (stv 0 1)
    )
)
(define (pln-xxx a b c) (QuoteLink a b c))
(define pln-alt
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "ConceptNode")
            )
            (TypedVariableLink
                (VariableNode "$B")
                (TypeNode "ConceptNode")
            )
            (TypedVariableLink
                (VariableNode "$C")
                (TypeNode "ConceptNode")
            )
        )
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$B")
            )
            (InheritanceLink
                (VariableNode "$B")
                (VariableNode "$C")
            )
            (NotLink
                (IdenticalLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-xxx")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
                (ListLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
    )
)