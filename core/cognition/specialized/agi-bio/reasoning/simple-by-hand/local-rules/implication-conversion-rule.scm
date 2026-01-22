(define pln-rule-intensional-implication-conversion
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B"))
        (IntensionalImplicationLink
            (VariableNode "$A")
            (VariableNode "$B"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-formula-implication-conversion")
            (ListLink
                (IntensionalImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))))))
(define (pln-formula-implication-conversion iiAB iAB)
    (cog-set-tv!
        iAB
        (pln-forumula-implication-conversion-side-effect-free iiAB iAB)))
(define (pln-forumula-implication-conversion-side-effect-free iiAB iAB)
    (stv (cog-stv-strength iiAB) (* .7 (cog-stv-confidence iiAB))))
(cog-name-rule "pln-rule-intensional-implication-conversion")