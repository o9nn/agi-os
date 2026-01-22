(define gene-similarity2overexpression-equivalence
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$X")
(TypeNode "GeneNode"))
(TypedVariableLink
(VariableNode "$Y")
(TypeNode "GeneNode")))
(IntensionalSimilarityLink
(VariableNode "$X")
(VariableNode "$Y"))
(ExecutionOutputLink
(GroundedSchemaNode "scm: create-overexpression-equivalence")
(ListLink
(VariableNode "$X")
(VariableNode "$Y")
(IntensionalSimilarityLink
(VariableNode "$X")
(VariableNode "$Y"))))))
#!
(define gene-similarity-implies-overexpression-equivalence
(ImplicationLink
(VariableList
(TypedVariableLink
(VariableNode "$X")
(TypeNode "GeneNode"))
(TypedVariableLink
(VariableNode "$Y")
(TypeNode "GeneNode")))
(IntensionalSimilarityLink
(VariableNode "$X")
(VariableNode "$Y"))
(ExecutionOutputLink
(GroundedSchemaNode "scm: create-overexpression-equivalence")
(ListLink
(VariableNode "$X")
(VariableNode "$Y")
(IntensionalSimilarityLink
(VariableNode "$X")
(VariableNode "$Y"))))))
!#
(define (create-overexpression-equivalence X Y XY)
(IntensionalEquivalenceLink
(ExecutionOutputLink
(GroundedSchemaNode "scm: make-overexpression-predicate")
(ListLink
X))
(ExecutionOutputLink
(GroundedSchemaNode "scm: make-overexpression-predicate")
(ListLink
Y))
(stv (cog-stv-strength XY) (cog-stv-confidence XY)))
)
(cog-name-rule "gene-similarity2overexpression-equivalence")