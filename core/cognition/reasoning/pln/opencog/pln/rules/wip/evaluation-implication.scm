(load "formulas.scm")
(define evaluation-implication-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C"))
        (AndLink
            (EvaluationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$C")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: evaluation-implication-formula")
            (ListLink
                (EvaluationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (EvaluationLink
                    (VariableNode "$C")
                    (VariableNode "$B"))))))
(define (evaluation-implication-formula AB AC CB)
    (define A (gar AB))
    (define B (gdr AB))
    (define C (gdr AC))
    (cog-set-tv!
        CB
        (stv
            (simple-deduction-strength-formula 
                (cog-mean B)
                (cog-mean A)
                (cog-mean C)
                (cog-mean AB)
                (cog-mean AC))
            (*
                (* 0.9 0.9)
                (min
                    (cog-confidence B)
                    (cog-confidence A)
                    (cog-confidence C)
                    (cog-confidence AC)
                    (* 0.9 (cog-confidence AB)))))))
(define evaluation-implication-rule-name (DefinedSchemaNode "evaluation-implication-rule"))
(DefineLink evaluation-implication-rule-name evaluation-implication-rule)