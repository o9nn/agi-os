(define decontextualize-inheritance-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (ContextLink
            (VariableNode "$C")
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$B")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (InheritanceLink
                    (AndLink
                        (VariableNode "$C")
                        (VariableNode "$A"))
                    (AndLink
                        (VariableNode "$C")
                        (VariableNode "$B")))
                (ContextLink
                    (VariableNode "$C")
                    (InheritanceLink
                        (VariableNode "$A")
                        (VariableNode "$B")))))))
(define decontextualize-evaluation-rule
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "PredicateNode"))
            (VariableNode "$B")
            (VariableNode "$C"))
        (ContextLink
            (VariableNode "$C")
            (EvaluationLink
                (VariableNode "$A")
                (ListLink
                    (VariableNode "$B"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (EvaluationLink
                    (VariableNode "$A")
                    (ListLink
                        (AndLink
                            (VariableNode "$C")
                            (VariableNode "$B"))))
                (ContextLink
                    (VariableNode "$C")
                    (EvaluationLink
                        (VariableNode "$A")
                        (ListLink
                            (VariableNode "$B"))))))))
(define decontextualize-subset-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$C"))
        (ContextLink
            (VariableNode "$C")
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (SubsetLink
                    (VariableNode "$C")
                    (VariableNode "$A"))
                (ContextLink
                    (VariableNode "$C")
                    (VariableNode "$A"))))))
(define (context-formula Relation Context)
    (cog-set-tv! Relation (cog-tv Context)))
(define contextualize-inheritance-rule-name
  (DefinedSchemaNode "contextualize-inheritance-rule"))
(DefineLink
  contextualize-inheritance-rule-name
  contextualize-inheritance-rule)