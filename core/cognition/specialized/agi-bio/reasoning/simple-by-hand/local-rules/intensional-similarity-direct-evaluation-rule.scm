(define pln-rule-intensional-similarity-direct-evaluation
    (BindLink
        (VariableList
                (VariableNode "$A")
                (VariableNode "$B")
        )
        (AndLink
            (VariableNode "$A")
            (VariableNode "$B")
            (NotLink
                (EqualLink
                    (VariableNode "$A")
                    (VariableNode "$B")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-formula-intensional-similarity-direct-evaluation")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")
                (IntensionalSimilarityLink
                    (VariableNode "$A")
                    (VariableNode "$B")
                )
            )
        )
    )
)
(define (pln-formula-intensional-similarity-direct-evaluation A B AB)
    (display "in main formula\n")
    (cog-set-tv!
        AB (pln-formula-intensional-similarity-direct-evaluation-side-effect-free A B AB)
    )
)
(define (pln-formula-intensional-similarity-direct-evaluation-side-effect-free A B AB)
    (load-from-path "rules/attraction-rule.scm")
    (define AttractionLinksA)
    (define AttractionLinksB)
    (display "in 2nd formula\n")
    (display-atom "A" A)
    (display-atom "B" B)
    (let*
        ((superA (cog-get-supersets A))
         (superB (cog-get-supersets B))
         (superA-length (length superA))
         (superB-length (length superB))
         (superUnion-length (length (lset-union equal? superA superB)))
         (superIntersection (lset-intersection equal? superA superB))
         (superIntersection-length (length superIntersection))
        )
        (display-label "superA-length" superA-length)
        (display-label "superB-length" superB-length)
        (display-label "superUnion-length" superUnion-length)
        (display-label "superIntersection" superIntersection)
        (set! attractionLinksA
            (map (make-attraction-link
                 (make-list superIntersection-length A) superIntersection)
            )
        )
        (set! attractionLinksB
            (map (make-attraction-link
                 (make-list superIntersection-length B) superIntersection)
            )
        )
        (stv 1 1)
     )
)
(define (make-attraction-link A B)
(define (cog-get-supersets A)
    (display "in cog-get-supersets")
    (display-atom "arg" A)
(let ((result
    (cog-outgoing-set
        (cog-bind
            (BindLink
                (VariableList
                    (VariableNode "$B")
                )
                (ChoiceLink
                    (SubsetLink
                        A
                        (VariableNode "$B")
                    )
                    (SubsetLink
                        (SetLink
                            A
                        )
                        (VariableNode "$B")
                    )
                )
                (VariableNode "$B")
            )
        )
    )
    )) result )
)
(define (cog-get-supersets2 A)
    (cog-bind
        (BindLink
            (VariableNode "$B")
            (OrLink
                (MemberLink
                    A
                    (VariableNode "$B"))
                (SubsetLink
                    A
                    (VariableNode "$B")))
            (VariableNode "$B"))))
(define (intensional-similarity-direct-evaluation A B)
    (pln-formula-intensional-similarity-direct-evaluation A B
        (IntensionalSimilarityLink A B))
)
(define pln-rule-intensional-similarity-direct-evaluation-name
  (Node "pln-rule-intensional-similarity-direct-evaluation"))
(DefineLink
  pln-rule-intensional-similarity-direct-evaluation-name
  pln-rule-intensional-similarity-direct-evaluation)