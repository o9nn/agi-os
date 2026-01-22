(define implication-and-lambda-factorization-variables
  (VariableSet
     (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableNode")
           (TypeNode "VariableSet")
           (TypeNode "VariableList")))
     (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableNode")
           (TypeNode "VariableSet")
           (TypeNode "VariableList")))
     (VariableNode "$A1")
     (VariableNode "$A2")))
(define implication-and-lambda-factorization-body
  (PresentLink
     (AndLink
        (QuoteLink (LambdaLink
           (Unquote (VariableNode "$TyVs-one"))
           (Unquote (VariableNode "$A1"))))
        (QuoteLink (LambdaLink
           (Unquote (VariableNode "$TyVs-two"))
           (Unquote (VariableNode "$A2")))))))
(define implication-and-lambda-factorization-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm-eager: implication-and-lambda-factorization")
     (ListLink
        (VariableNode "$TyVs-one")
        (VariableNode "$TyVs-two")
        (VariableNode "$A1")
        (VariableNode "$A2"))))
(define implication-and-lambda-factorization-rule
  (BindLink
     implication-and-lambda-factorization-variables
     implication-and-lambda-factorization-body
     implication-and-lambda-factorization-rewrite))
(define (implication-and-lambda-factorization var1 var2 a1 a2)
  (let ((and-lamb (AndLink (LambdaLink var1 a1) (LambdaLink var2 a2)))
        (lamb (LambdaLink (Variable "$flat") (cog-new-flattened-link 'AndLink a1 a2))))
    (cog-set-tv! lamb (cog-tv and-lamb))
    (cog-set-tv! (ImplicationLink and-lamb lamb) (stv 1 1))))
(define implication-and-lambda-factorization-rule-name
  (DefinedSchemaNode "implication-and-lambda-factorization-rule"))
(DefineLink implication-and-lambda-factorization-rule-name
  implication-and-lambda-factorization-rule)