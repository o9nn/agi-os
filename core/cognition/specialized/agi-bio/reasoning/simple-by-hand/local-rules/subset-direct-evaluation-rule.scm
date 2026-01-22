(define pln-rule-subset-direct-evaluation
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B"))
        (AndLink
            (VariableNode "$A")
            (VariableNode "$B")
            (NotLink
                (EqualLink
                    (VariableNode "$A")
                    (VariableNode "$B"))))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-formula-subset-direct-evaluation")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")
                (SubsetLink
                    (VariableNode "$A")
                    (VariableNode "$B"))))))
(define (pln-formula-subset-direct-evaluation A B AB)
    (cog-set-tv!
        AB (pln-formula-subset-direct-evaluation-side-effect-free A B))
    )
(define (pln-formula-subset-direct-evaluation-side-effect-free A B)
    (load "rule-helpers.scm")
    (let*
         ((membersA (get-set-members A))
          (membersB (get-set-members B))
          (display-var "membersB")
          (intersectionAB (lset-intersection equal? membersA membersB))
          (sizeA (length membersA))
          (size-intersection (length intersectionAB)))
         (if (> sizeA 0)
            (stv (/ size-intersection sizeA) 1)
            (stv 0 1))))
(cog-name-rule "pln-rule-subset-direct-evaluation")
(define (subset-direct-evaluation A  B)
    (pln-formula-subset-direct-evaluation A B (SubsetLink A B)))
(define (inverse-subset-direct-evaluation A  B)
    (pln-formula-subset-direct-evaluation B  A (SubsetLink B A)))