(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(define universal-total-instantiation-forall-1ary-meta-rule
  (let* ((X (Variable "$X"))
         (XType (Variable "$XType"))
         (VariableT (Type "VariableNode"))
         (TypeChoiceT (Type "TypeChoice"))
         (TypeT (Type "TypeNode"))
         (XTypeVardeclT (TypeChoice TypeChoiceT TypeT))
         (P (Variable "$P"))
         (meta-vardecl (VariableList
                         (TypedVariable X VariableT)
                         (TypedVariable XType XTypeVardeclT)
                         P))
         (forall (Quote
                   (ForAll
                     (Unquote
                       (TypedVariable
                         X
                         XType))
                     (Unquote P))))
         (meta-precondition (Evaluation
                              (GroundedPredicate "scm: gt-zero-confidence")
                              forall))
         (meta-pattern (And (Present forall) meta-precondition))
         (produced-vardecl (TypedVariable X XType))
         (produced-pattern X)
         (produced-rewrite (ExecutionOutput
                            (GroundedSchema "scm: universal-total-instantiation-forall-formula")
                            (Unquote
                              (List
                                P
                                forall))))
         (meta-rewrite (Quote (Bind
                          (Unquote produced-vardecl)
                          (Unquote produced-pattern)
                          produced-rewrite
                          ))))
    (Bind
      meta-vardecl
      meta-pattern
      meta-rewrite)))
(define (universal-total-instantiation-forall-formula Pinst Forall)
  (let* ((Forall-tv (cog-tv Forall)))
    (if (< 0 (cog-tv-confidence Forall-tv))
        (cog-merge-hi-conf-tv! Pinst Forall-tv))))
(define universal-total-instantiation-forall-1ary-meta-rule-name
  (DefinedSchemaNode "universal-total-instantiation-forall-1ary-meta-rule"))
(DefineLink universal-total-instantiation-forall-1ary-meta-rule-name
  universal-total-instantiation-forall-1ary-meta-rule)