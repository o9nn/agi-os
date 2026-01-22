(define fc-deduction-rule
    (BindLink
        (VariableSet
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$B")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$C")
                (TypeNode "ConceptNode")))
        (AndLink
            (PresentLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B")
                )
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
            )
            (NotLink
                (IdenticalLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm-eager: fc-deduction")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C"))
            )
        )
    )
)
(define (fc-deduction AC AB BC)
    (let (  (sAB (cog-mean AB))
            (cAB (cog-confidence AB))
            (sBC (cog-mean BC))
            (cBC (cog-confidence BC)))
        (if (and (>= sAB 0.5) (>= cAB 0.5) (>= sBC 0.5) (>= cBC 0.5))
            (cog-set-tv! AC (stv 1 1)))))
(define fc-deduction-rule-name
    (DefinedSchemaNode "fc-deduction-rule"))
(DefineLink
    fc-deduction-rule-name
    fc-deduction-rule)