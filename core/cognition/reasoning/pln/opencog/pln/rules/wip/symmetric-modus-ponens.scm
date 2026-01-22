(load "formulas.scm")
(define (gen-symmetric-modus-ponens-rule link-type)
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$B"))
(link-type
(VariableNode "$A")
(VariableNode "$B"))
(ExecutionOutputLink
(GroundedSchemaNode "scm: symmetric-modus-ponens-formula")
(ListLink
(VariableNode "$A")
(link-type
(VariableNode "$A")
(VariableNode "$B"))
(VariableNode "$B")))))
(define symmetric-modus-ponens-similarity-rule
(gen-symmetric-modus-ponens-rule SimilarityLink))
(define symmetric-modus-ponens-intensional-similarity-rule
(gen-symmetric-modus-ponens-rule IntensionalSimilarityLink))
(define symmetric-modus-ponens-extensional-similarity-rule
(gen-symmetric-modus-ponens-rule ExtensionalSimilarityLink))
(define (symmetric-modus-ponens-formula A AB B)
(let
((sA (cog-mean A))
(cA (cog-confidence A))
(sAB (cog-mean AB))
(cAB (cog-confidence AB))
(snotAB 0.2)
(cnotAB 1))
(cog-set-tv!
B
(stv
(+ (* sA sAB) (* (* snotAB (negate sA)) (+ 1 sAB)))
(min (min cAB cnotAB) cA)))))