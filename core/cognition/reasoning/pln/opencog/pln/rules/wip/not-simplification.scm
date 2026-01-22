(define not-simplification-rule
  (BindLink
   (VariableList
    (VariableNode "$A"))
   (NotLink
    (NotLink
     (VariableNode "$A")))
   (ExecutionOutputLink
    (GroundedSchemaNode "scm: not-simplification-formula")
    (ListLink
     (NotLink
      (NotLink
       (VariableNode "$A")))
     (VariableNode "$A")))))
(define (not-simplification-formula NNA A)
  (cog-set-tv!
   A
   (not-simplification-side-effect-free-formula NNA A))
)
(define (not-simplification-side-effect-free-formula NNA A)
  (let 
      ((sNNA (cog-mean NNA))
       (cNNA (cog-confidence NNA)))
    (stv (- 1 sNNA) cNNA)))
(define not-simplification-rule-name
  (DefinedSchemaNode "not-simplification-rule"))
(DefineLink
  not-simplification-rule-name
  not-simplification-rule)