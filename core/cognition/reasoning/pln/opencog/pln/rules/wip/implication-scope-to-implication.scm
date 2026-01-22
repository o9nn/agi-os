(define implication-scope-to-implication-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableList")))
     (VariableNode "$P")
     (VariableNode "$Q")))
(define implication-scope-to-implication-body
  (Quote (ImplicationScopeLink
     (Unquote (VariableNode "$TyVs"))
     (Unquote (VariableNode "$P"))
     (Unquote (VariableNode "$Q")))))
(define implication-scope-to-implication-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-scope-to-implication-formula")
     (ListLink
        (Implication
           (Quote (Lambda
              (Unquote (VariableNode "$TyVs"))
              (Unquote (VariableNode "$P"))))
           (Quote (Lambda
              (Unquote (VariableNode "$TyVs"))
              (Unquote (VariableNode "$Q")))))
        implication-scope-to-implication-body)))
(define implication-scope-to-implication-rule
  (BindLink
     implication-scope-to-implication-variables
     implication-scope-to-implication-body
     implication-scope-to-implication-rewrite))
(define (implication-scope-to-implication-formula Impl ImplSc)
  (cog-merge-hi-conf-tv! Impl (cog-tv ImplSc)))
(define implication-scope-to-implication-rule-name
  (DefinedSchemaNode "implication-scope-to-implication-rule"))
(DefineLink implication-scope-to-implication-rule-name
  implication-scope-to-implication-rule)