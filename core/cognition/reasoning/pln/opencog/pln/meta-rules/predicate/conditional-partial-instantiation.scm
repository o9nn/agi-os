(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(define conditional-partial-instantiation-meta-rule
  (let* ((V1 (Variable "$V1"))
         (V2 (Variable "$V2"))
         (V3 (Variable "$V3"))
         (V1Type (Variable "$V1Type"))
         (V2Type (Variable "$V2Type"))
         (V3Type (Variable "$V3Type"))
         (VariableT (Type "VariableNode"))
         (TypeChoiceT (Type "TypeChoice"))
         (TypeT (Type "TypeNode"))
         (VariableTypeT (TypeChoice TypeChoiceT TypeT))
         (TypedV1 (TypedVariable V1 VariableT))
         (TypedV2 (TypedVariable V2 VariableT))
         (TypedV3 (TypedVariable V3 VariableT))
         (TypedV1Type (TypedVariable V1Type VariableTypeT))
         (TypedV2Type (TypedVariable V2Type VariableTypeT))
         (TypedV3Type (TypedVariable V3Type VariableTypeT))
         (P (Variable "$P"))
         (Q (Variable "$Q"))
         (meta-vardecl (VariableList
                         TypedV1 TypedV2 TypedV3
                         TypedV1Type TypedV2Type TypedV3Type
                         P Q))
         (implication (Quote
                        (ImplicationScope
                          (Unquote (VariableList
                            (TypedVariable V1 V1Type)
                            (TypedVariable V2 V2Type)
                            (TypedVariable V3 V3Type)))
                          (Unquote P)
                          (Unquote Q))))
         (meta-precondition (Evaluation
                              (GroundedPredicate "scm: gt-zero-confidence")
                              implication))
         (meta-pattern (And (Present implication) meta-precondition))
         (produced-vardecl (VariableList
                             (TypedVariable V2 V2Type)
                             (TypedVariable V3 V3Type)))
         (produced-pattern (And V2 V3))
         (produced-rewrite (ExecutionOutput
                            (GroundedSchema "scm: conditional-partial-instantiation-formula")
                            (Unquote
                              (List
                                (Quote
                                  (ImplicationScope
                                    (Unquote
                                      (TypedVariable V1 V1Type))
                                    (Unquote P)
                                    (Unquote Q)))
                                implication))))
         (meta-rewrite (Quote (Bind
                          (Unquote produced-vardecl)
                          (Unquote produced-pattern)
                          produced-rewrite
                          ))))
    (Bind
      meta-vardecl
      meta-pattern
      meta-rewrite)))
(define (conditional-partial-instantiation-formula PImpl Impl)
  (cog-set-tv! PImpl (cog-tv Impl)))
(define conditional-partial-instantiation-meta-rule-name
  (DefinedSchemaNode "conditional-partial-instantiation-meta-rule"))
(DefineLink conditional-partial-instantiation-meta-rule-name
  conditional-partial-instantiation-meta-rule)