""
(define gene-strength .00001)
(define gene-confidence .9)
(define gene-concept-strength .001)
(define gene-concept-confidence .9)
""
(define long-gene (GeneNode "TBK1"))
(define long-set (SetLink long-gene))
(define target (GeneNode "LY96"))
(PredicateNode "LongLived" (stv .15  .8))
(define long-gene-implies-ll (IntensionalImplicationLink
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: make-overexpression-predicate")
            (ListLink
                long-gene))
    (PredicateNode "LongLived") (stv .5 .7)))
(display-var "long-gene-implies-ll" long-gene-implies-ll)
(define gene-similarity-variant-rule
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "GeneNode"))
            (TypedVariableLink
                (VariableNode "$Y")
                (TypeNode "GeneNode"))
            (VariableNode "$P"))
        (AndLink
            (ChoiceLink
                (IntensionalSimilarityLink
                    (VariableNode "$X")
                    (VariableNode "$Y"))
                (IntensionalSimilarityLink
                    (VariableNode "$Y")
                    (VariableNode "$X")))
            (IntensionalImplicationLink
                (ExecutionOutputLink
                    (GroundedSchemaNode "scm: make-contains-significant-variant-predicate")
                    (ListLink
                        (VariableNode "$X")))
                (VariableNode "$P")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: create-variant-implies-phenotype")
            (ListLink
                (VariableNode "$X")
                (VariableNode "$Y")
                (IntensionalSimilarityLink
                    (VariableNode "$X")
                    (VariableNode "$Y"))
                (VariableNode "$P")))))
(define gene-similarity-variant-implication
    (ImplicationLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "GeneNode"))
            (TypedVariableLink
                (VariableNode "$Y")
                (TypeNode "GeneNode"))
            (VariableNode "$P"))
        (AndLink
            (ChoiceLink
                (IntensionalSimilarityLink
                    (VariableNode "$X")
                    (VariableNode "$Y"))
                (IntensionalSimilarityLink
                    (VariableNode "$Y")
                    (VariableNode "$X")))
            (IntensionalImplicationLink
                (ExecutionOutputLink
                    (GroundedSchemaNode "scm: make-contains-significant-variant-predicate")
                    (ListLink
                        (VariableNode "$X")))
                (VariableNode "$P")))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: create-variant-implies-phenotype")
            (ListLink
                (VariableNode "$X")
                (VariableNode "$Y")
                (IntensionalSimilarityLink
                    (VariableNode "$X")
                    (VariableNode "$Y"))
                (VariableNode "$P")))))
(define (create-variant-implies-phenotype X Y XY P)
    (ImplicationLink
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: make-contains-significant-variant-predicate")
            (ListLink Y))
        P
        (stv (cog-stv-strength XY) (* .8 cog-stv-confidence XY))))
(define los (lifespan-observation-increased-members))
(define known-longevity-genes (list
    (GeneNode "CETP" (stv .0001 .9))))
(define long-genes (append los known-longevity-genes))