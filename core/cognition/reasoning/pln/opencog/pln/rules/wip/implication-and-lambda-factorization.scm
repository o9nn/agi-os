(define implication-and-lambda-factorization-vardecl
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableNode")
           (TypeNode "VariableList")))
     (TypedVariableLink
        (VariableNode "$A1")
        (TypeNode "EvaluationLink"))
     (TypedVariableLink
        (VariableNode "$A2")
        (TypeNode "EvaluationLink"))))
(define implication-and-lambda-factorization-pattern
  (LocalQuoteLink
     (AndLink
        (QuoteLink (LambdaLink
           (UnquoteLink (VariableNode "$TyVs"))
           (UnquoteLink (VariableNode "$A1"))))
        (QuoteLink (LambdaLink
           (UnquoteLink (VariableNode "$TyVs"))
           (UnquoteLink (VariableNode "$A2")))))))
(define implication-and-lambda-factorization-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-and-lambda-factorization-formula")
     (ImplicationLink
        (AndLink
           (QuoteLink (LambdaLink
              (UnquoteLink (VariableNode "$TyVs"))
              (UnquoteLink (VariableNode "$A1"))))
           (QuoteLink (LambdaLink
              (UnquoteLink (VariableNode "$TyVs"))
              (UnquoteLink (VariableNode "$A2")))))
        (QuoteLink (LambdaLink
           (UnquoteLink (VariableNode "$TyVs"))
           (UnquoteLink (AndLink
              (VariableNode "$A1")
              (VariableNode "$A2"))))))))
(define implication-and-lambda-factorization-rule
  (BindLink
     implication-and-lambda-factorization-vardecl
     implication-and-lambda-factorization-pattern
     implication-and-lambda-factorization-rewrite))
(define (implication-and-lambda-factorization-formula Impl)
  (cog-set-tv! Impl (stv 1 1)))
(define implication-and-lambda-factorization-rule-name
  (DefinedSchemaNode "implication-and-lambda-factorization-rule"))
(DefineLink implication-and-lambda-factorization-rule-name
  implication-and-lambda-factorization-rule)