(define not-introduction-rule
  (BindLink
     (VariableList
        (TypedVariableLink
           (VariableNode "$A")
           (TypeChoice
              (TypeNode "PredicateNode")
              (TypeNode "ConceptNode"))))
     (VariableNode "$A")
     (ExecutionOutputLink
        (GroundedSchemaNode "scm: not-introduction-formula")
        (ListLink
           (VariableNode "$A")))))
(define (not-introduction-formula A)
  (cog-set-tv!
   (NotLink A)
   (not-introduction-side-effect-free-formula A))
)
(define (negate x)
  (- 1 x))
(define (not-introduction-side-effect-free-formula A)
  (let ((sA (cog-mean A))
        (cA (cog-confidence A)))
    (stv (negate sA) cA)))
(define not-introduction-rule-name
  (DefinedSchemaNode "not-introduction-rule"))
(DefineLink
   not-introduction-rule-name
   not-introduction-rule)