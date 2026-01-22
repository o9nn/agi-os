#!
Simple example of bio-inference using PLN by hand.
Usage:
cd to this file's directory, run guile, and then in guile scheme:
scheme@(guile-user)> (load "simple-inference.scm")
Background Knowledge:
    (IntensionalImplicationLink
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: make-over-expression-predicate")
            (GeneNode "PLAU"))
        (PredicateNode "LongLived") (stv .2 .7))
    (MemberLink (stv 1 1)
        (GeneNode "PLAU")
        (ConceptNode "GO_A"))
    (MemberLink (stv 1 1)
        (GeneNode "L")
        (ConceptNode "GO_A"))
    (MemberLink (stv 1 1)
        (GeneNode "L")
        (ConceptNode "GO_B"))
    (MemberLink (stv 1 1)
        (GeneNode "PLAU")
        (ConceptNode "GO_B"))
    (MemberLink (stv 1 1)
        (GeneNode "PLAU")
        (ConceptNode "GO_C"))
    (MemberLink (stv 1 1)
        (GeneNode "Q")
        (ConceptNode "GO_B"))
    (MemberLink (stv 1 1)
        (GeneNode "L")
        (ConceptNode "GO_D"))
And our target conclusion is:
    IntensionalImplicationLink
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: make-overexpression-predicate")
            (GeneNode "L"))
        (PredicateNode "LongLived")
In other words, we want to infer a relationship between Gene L and LongLived
though it's association with Gene PLAU, which is already known to be related
to longevity.
!#
(use-modules (opencog))
(use-modules (opencog rule-engine))
(load "utilities.scm")
(load "local-rules/rule-helpers.scm")
(load "background-knowledge.scm")
(load "pln-config.scm")
(load "substitute.scm")
(load "cog-create-intensional-links.scm")
(define gene-memberlinks
    (cog-filter
        'MemberLink
        (append-map cog-incoming-set (cog-get-atoms 'GeneNode))
    )
)
(display-var "gene-memberlinks")
(define m2s (map cog-apply-rule
    (make-list (length gene-memberlinks) "pln-rule-member-to-subset")
    gene-memberlinks
    (make-list (length gene-memberlinks) #t))
)
(set! m2s (map (lambda(x) (list-ref (cog-outgoing-set x) 0)) m2s))
(display-var "m2s")
#!
        (SubsetLink (stv 1 0.99999982)
           (SetLink
              (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
           )
           (ConceptNode "GO_C" (stv 0.001 0.89999998))
        )
        (SubsetLink (stv 1 0.99999982)
           (SetLink
              (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
           )
           (ConceptNode "GO_B" (stv 0.001 0.89999998))
        )
        (SubsetLink (stv 1 0.99999982)
           (SetLink
              (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
           )
           (ConceptNode "GO_A" (stv 0.001 0.89999998))
        )
        (SubsetLink (stv 1 0.99999982)
           (SetLink
              (GeneNode "L" (stv 9.9999997e-06 0.89999998))
           )
           (ConceptNode "GO_B" (stv 0.001 0.89999998))
        )
        (SubsetLink (stv 1 0.99999982)
           (SetLink
              (GeneNode "L" (stv 9.9999997e-06 0.89999998))
           )
           (ConceptNode "GO_A" (stv 0.001 0.89999998))
        )
!#
#! The following steps 2-6 occur in the cog-create-intensional-links command:
(2) Get the supersets of {L} and of {PLAU} (IOW the categories they are members
    of), and the union and intersection of the supersets
    superA:
    ((ConceptNode "GO_A" (stv 0.001 0.89999998))
     (ConceptNode "GO_B" (stv 0.001 0.89999998))
    )
    superB:
    ((ConceptNode "GO_A" (stv 0.001 0.89999998))
     (ConceptNode "GO_B" (stv 0.001 0.89999998))
     (ConceptNode "GO_C" (stv 0.001 0.89999998))
    )
    superIntersection:
    ((ConceptNode "GO_A" (stv 0.001 0.89999998))
     (ConceptNode "GO_B" (stv 0.001 0.89999998))
    )
    superUnion-length: 3
(3) For each common relationship (IOW for each relationship in the supersets
    intersection), create the same inverse relationship.
    (SubsetLink (stv 0.5 0.99999982)
       (ConceptNode "GO_A" (stv 0.001 0.89999998))
       (SetLink
          (GeneNode "L" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0.5 0.99999982)
       (ConceptNode "GO_B" (stv 0.001 0.89999998))
       (SetLink
          (GeneNode "L" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0.5 0.99999982)
       (ConceptNode "GO_A" (stv 0.001 0.89999998))
       (SetLink
          (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0.5 0.99999982)
       (ConceptNode "GO_B" (stv 0.001 0.89999998))
       (SetLink
          (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
       )
    )
(4) For each inverse relationship (LinkType A B), create (LinkType (Not A) b)
    (SubsetLink (stv 0 0.99999982)
       (NotLink
          (ConceptNode "GO_A" (stv 0.001 0.89999998))
       )
       (SetLink
          (GeneNode "L" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0 0.99999982)
       (NotLink
          (ConceptNode "GO_B" (stv 0.001 0.89999998))
       )
       (SetLink
          (GeneNode "L" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0 0.99999982)
       (NotLink
          (ConceptNode "GO_A" (stv 0.001 0.89999998))
       )
       (SetLink
          (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
       )
    )
    (SubsetLink (stv 0 0.99999982)
       (NotLink
          (ConceptNode "GO_B" (stv 0.001 0.89999998))
       )
       (SetLink
          (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
       )
    )
(5) Apply the AttractionRule to make AttractionLinks for L and PLAU for each
    common relationship (IOW for each relationship in the supersets
    intersection).
        (AttractionLink (stv 0.5 0.99999982)
           (ConceptNode "GO_A" (stv 0.001 0.89999998))
           (SetLink
              (GeneNode "L" (stv 9.9999997e-06 0.89999998))
           )
        )
        (AttractionLink (stv 0.5 0.99999982)
           (ConceptNode "GO_B" (stv 0.001 0.89999998))
           (SetLink
              (GeneNode "L" (stv 9.9999997e-06 0.89999998))
           )
        )
        (AttractionLink (stv 0.5 0.99999982)
           (ConceptNode "GO_A" (stv 0.001 0.89999998))
           (SetLink
              (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
           )
        )
        (AttractionLink (stv 0.5 0.99999982)
           (ConceptNode "GO_B" (stv 0.001 0.89999998))
           (SetLink
              (GeneNode "PLAU" (stv 9.9999997e-06 0.89999998))
           )
        )
(6) Create IntensionalSimilarityLink via direct evaluation based on
    AttractionLinks and # of members in the union of supersets
    tv.s = average(ASSOC(A,L) AND ASSOC(B,L))
           over all relationships in the union of supersets
!#
(define is-l-plau (cog-create-intensional-links
                    (SetLink (GeneNode "L")) (SetLink (GeneNode "PLAU")))
)
(display-var "is-l-plau")
(define is2-l-plau (cog-bind pln-rule-singleton-similarity))
(display-var "is2-l-plau")
(define IE (cog-bind gene-similarity2overexpression-equivalence))
(display-var "IE")
(define II (cog-bind pln-rule-intensional-equivalence-transformation))
(display-var "II")
(define to-long-life (cog-bind pln-rule-deduction-intensional-implication))
(display-var "to-long-life")
(define grounded-conversion-rule
    (substitute pln-rule-intensional-implication-conversion
        (list (cons (VariableNode "$B") (PredicateNode "LongLived")))))
(define conclusion (cog-bind grounded-conversion-rule))
(display-var "conclusion")