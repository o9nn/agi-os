(define crisp-modus-ponens-rule
    (BindLink
        (VariableSet
            (TypedVariable
                (VariableNode "$A")
                (TypeNode "PredicateNode"))
            (TypedVariable
                (VariableNode "$B")
                (TypeNode "PredicateNode")))
        (PresentLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: crisp-modus-ponens")
            (ListLink
                (VariableNode "$B")
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (VariableNode "$A")
                ))))
(define (crisp-modus-ponens B AB A)
    (let
        ((sA (cog-mean A))
         (cA (cog-confidence A))
         (sAB (cog-mean AB))
         (cAB (cog-confidence AB)))
      (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
          (cog-set-tv! B (stv 1 1)))))
(define crisp-modus-ponens-rule-name
  (DefinedSchemaNode "crisp-modus-ponens-rule"))
(DefineLink
  crisp-modus-ponens-rule-name
  crisp-modus-ponens-rule)