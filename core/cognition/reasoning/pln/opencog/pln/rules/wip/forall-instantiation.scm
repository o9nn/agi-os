(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(load-from-path "opencog/pln/rules/wip/instantiation.scm")
(define forall-total-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableList")))
     (VariableNode "$B")))
(define forall-instantiation-body
  (QuoteLink (ForAllLink
     (UnquoteLink (VariableNode "$TyVs"))
     (UnquoteLink (VariableNode "$B")))))
(define forall-total-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: forall-total-instantiation-formula")
     (ListLink
        (VariableNode "$TyVs")
        (VariableNode "$B"))))
(define forall-total-instantiation-rule
  (BindLink
     forall-total-instantiation-variables
     forall-instantiation-body
     forall-total-instantiation-rewrite))
(define (forall-total-instantiation-formula SV B)
  (cog-set-tv!
   (let* (
          (SV-type (cog-type SV))
          (terms (cond ((cog-subtype? 'TypedVariableLink SV-type)
                        (select-substitution-term SV))
                       ((cog-subtype? 'VariableList SV-type)
                        (select-substitution-terms SV))
                       (cog-logger-error
                        (string-append "Wrong type for ~a, "
                                       "should be a TypedVariableLink "
                                       "or a VariableList") SV))))
     (cog-execute! (PutLink (LambdaLink SV B) terms)))
   (stv 1 1)))
(define forall-total-instantiation-rule-name
  (DefinedSchemaNode "forall-total-instantiation-rule"))
(DefineLink forall-total-instantiation-rule-name
  forall-total-instantiation-rule)
(define forall-partial-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeNode "VariableList"))
     (VariableNode "$B")))
(define forall-partial-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: forall-partial-instantiation-formula")
     (ListLink
        (VariableNode "$TyVs")
        (VariableNode "$B"))))
(define forall-partial-instantiation-rule
  (BindLink
     forall-partial-instantiation-variables
     forall-instantiation-body
     forall-partial-instantiation-rewrite))
(define (forall-partial-instantiation-formula TyVs B)
  (cog-set-tv!
   (let* (
          (TyV-and-remain (select-rm-rnd-outgoing TyVs))
          (TyV (car TyV-and-remain))
          (TyVs-remain (cadr TyV-and-remain))
          (TyVs-remain-len (length (cog-outgoing-set TyVs-remain)))
          (term (select-substitution-term TyV))
          (B-inst (cog-execute! (PutLink (LambdaLink TyV B) term)))
          (TyVs-remain (if (= TyVs-remain-len 1)
                           (gar TyVs-remain)
                           TyVs-remain)))
     (if (> TyVs-remain-len 0)
         (ForAllLink TyVs-remain B-inst)
         B-inst))
   (stv 1 1)))
(define forall-partial-instantiation-rule-name
  (DefinedSchemaNode "forall-partial-instantiation-rule"))
(DefineLink forall-partial-instantiation-rule-name
  forall-partial-instantiation-rule)