(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(load-from-path "opencog/pln/rules/wip/instantiation.scm")
(define implication-total-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableList")))
     (VariableNode "$P")
     (VariableNode "$Q")))
(define implication-instantiation-body
  (QuoteLink (ImplicationScopeLink
     (UnquoteLink (VariableNode "$TyVs"))
     (UnquoteLink (VariableNode "$P"))
     (UnquoteLink (VariableNode "$Q")))))
(define implication-total-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-total-instantiation-formula")
     (ListLink
        implication-instantiation-body)))
(define implication-total-instantiation-rule
  (BindLink
     implication-total-instantiation-variables
     implication-instantiation-body
     implication-total-instantiation-rewrite))
(define (implication-total-instantiation-formula Impl)
  (let* ((Impl-outgoings (cog-outgoing-set Impl))
         (Impl-s (cog-mean Impl))
         (Impl-c (cog-confidence Impl))
         (TyVs (car Impl-outgoings))
         (P (cadr Impl-outgoings))
         (P-s (cog-mean P))
         (P-c (cog-confidence P))
         (P-s (if (and (< 0.99 P-s) (<= P-c 0)) 0.25 P-s))
         (Q (caddr Impl-outgoings))
         (terms (if (= 0 Impl-c)
                    #f
                    (select-conditioned-substitution-terms TyVs P))))
    (if terms
        (let* (
               (Pput (PutLink (LambdaLink TyVs P) terms))
               (Pinst (cog-execute! Pput))
               (Pinst-s (cog-mean Pinst))
               (Pinst-c (cog-confidence Pinst))
               (Qput (PutLink (LambdaLink TyVs Q) terms))
               (Qinst (cog-execute! Qput))
               (Qinst-s (* Impl-s Pinst-s))
               (Qinst-c (* Impl-c Pinst-c (- 1 P-s))))
          (if (< 0 Qinst-c)
              (cog-merge-hi-conf-tv! Qinst (stv Qinst-s Qinst-c)))))))
(define implication-total-instantiation-rule-name
  (DefinedSchemaNode "implication-total-instantiation-rule"))
(DefineLink implication-total-instantiation-rule-name
  implication-total-instantiation-rule)
(define implication-partial-instantiation-variables
  (VariableList
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeNode "VariableList"))
     (VariableNode "$P")
     (VariableNode "$Q")))
(define implication-partial-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-partial-instantiation-formula")
     (ListLink
        implication-instantiation-body)))
(define implication-partial-instantiation-rule
  (BindLink
     implication-partial-instantiation-variables
     implication-instantiation-body
     implication-partial-instantiation-rewrite))
(define (implication-partial-instantiation-formula Impl)
  (let* (
         (Impl-outgoings (cog-outgoing-set Impl))
         (TyVs (car Impl-outgoings))
         (P (cadr Impl-outgoings))
         (Q (caddr Impl-outgoings))
         (TyVs-outgoings (cog-outgoing-set TyVs))
         (TyVs-outgoings-len (length TyVs-outgoings))
         (terms (select-conditioned-substitution-terms TyVs P))
         (rnd-index (random TyVs-outgoings-len))
         (TyV (list-ref TyVs-outgoings rnd-index))
         (TyVs-remain-list (rm-list-ref TyVs-outgoings rnd-index))
         (TyVs-remain-len (length TyVs-remain-list))
         (TyVs-remain (apply cog-new-link 'VariableList TyVs-remain-list)))
    (if terms
        (cog-set-tv!
         (let* (
                (term (list-ref (cog-outgoing-set terms) rnd-index))
                (P-put (PutLink (LambdaLink TyV P) term))
                (Q-put (PutLink (LambdaLink TyV Q) term))
                (P-inst (cog-execute! P-put))
                (Q-inst (cog-execute! Q-put))
                (TyVs-remain (if (= TyVs-remain-len 1)
                                 (gar TyVs-remain)
                                 TyVs-remain)))
           (extract-hypergraph P-put)
           (extract-hypergraph Q-put)
           (if (> TyVs-remain-len 0)
               (ImplicationScopeLink TyVs-remain P-inst Q-inst)
               Q-inst))
         (cog-tv Impl)))))
(define implication-partial-instantiation-rule-name
  (DefinedSchemaNode "implication-partial-instantiation-rule"))
(DefineLink implication-partial-instantiation-rule-name
  implication-partial-instantiation-rule)