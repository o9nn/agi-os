(define contextualize-inheritance-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (InheritanceLink
            (AndLink
                (VariableNode "$C")
                (VariableNode "$A"))
            (AndLink
                (VariableNode "$C")
                (VariableNode "$B")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (ContextLink
                    (VariableNode "$C")
                    (InheritanceLink
                        (VariableNode "$A")
                        (VariableNode "$B")))
                (InheritanceLink
                    (AndLink
                        (VariableNode "$C")
                        (VariableNode "$A"))
                    (AndLink
                        (VariableNode "$C")
                        (VariableNode "$B")))))))
(define contextualize-evaluation-rule
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "PredicateNode"))
            (VariableNode "$B")
            (VariableNode "$C"))
        (EvaluationLink
            (VariableNode "$A")
            (ListLink
                (AndLink
                    (VariableNode "$C")
                    (VariableNode "$B"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (ContextLink
                    (VariableNode "$C")
                    (EvaluationLink
                        (VariableNode "$A")
                        (ListLink
                            (VariableNode "$B"))))
                (EvaluationLink
                    (VariableNode "$A")
                    (ListLink
                        (AndLink
                            (VariableNode "$C")
                            (VariableNode "$B"))))))))
(define contextualize-subset-rule
    (BindLink
        (VariableList
            (VariableNode "$C")
            (VariableNode "$A"))
        (SubsetLink
            (VariableNode "$C")
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: context-formula")
            (ListLink
                (ContextLink
                    (VariableNode "$C")
                    (VariableNode "$A"))
                (SubsetLink
                    (VariableNode "$C")
                    (VariableNode "$A"))))))
(define create-and-as-1st-arg-of-inheritance-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$C"))
            (InheritanceLink
                (VariableNode "$B")
                (VariableNode "$C")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: create-and-inside-inheritance-formula")
            (ListLink
                (InheritanceLink
                    (AndLink
                        (VariableNode "$A")
                        (VariableNode "$B"))
                    (VariableNode "$C"))
                (AndLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (AndLink
                    (InheritanceLink
                        (VariableNode "$A")
                        (VariableNode "$C"))
                    (InheritanceLink
                        (VariableNode "$B")
                        (VariableNode "$C")))
               (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
               (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))))
(define create-and-as-2nd-arg-of-inheritance-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (InheritanceLink
                (VariableNode "$A")
                (VariableNode "$C")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: create-and-inside-inheritance-formula")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (AndLink
                        (VariableNode "$B")
                        (VariableNode "$C")))
                (AndLink
                        (VariableNode "$B")
                        (VariableNode "$C"))
                (AndLink
                    (InheritanceLink
                        (VariableNode "$A")
                        (VariableNode "$B"))
                    (InheritanceLink
                        (VariableNode "$A")
                        (VariableNode "$C")))
               (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
               (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$C"))))))
(define (create-and-inside-inheritance-formula outInh outAnd inAnd inEmbedInh1 inEmbedInh2)
    (cog-set-tv! outInh (cog-tv inAnd)))                  
(define (context-formula Context Relation)
    (cog-set-tv! Context (cog-tv Relation)))
(define contextualize-inheritance-rule-name
  (DefinedSchemaNode "contextualize-inheritance-rule"))
(DefineLink
  contextualize-inheritance-rule-name
  contextualize-inheritance-rule)