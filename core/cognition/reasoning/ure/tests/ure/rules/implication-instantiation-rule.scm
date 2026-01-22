(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(load-from-path "tests/ure/rules/instantiation.scm")
(define implication-full-instantiation-variables
  (VariableSet
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeChoice
           (TypeNode "TypedVariableLink")
           (TypeNode "VariableSet")
           (TypeNode "VariableList")))  
     (VariableNode "$P")
     (VariableNode "$Q")))
(define implication-instantiation-body
  (PresentLink
    (ImplicationScopeLink
      (VariableNode "$TyVs")
      (VariableNode "$P")
      (VariableNode "$Q"))))
(define implication-full-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm-eager: implication-full-instantiation")
     (ListLink
        implication-instantiation-body)))
(define implication-full-instantiation-rule
  (BindLink
     implication-full-instantiation-variables
     implication-instantiation-body
     implication-full-instantiation-rewrite))
(define (implication-full-instantiation Impl)
  (let* (
         (Impl-outgoings (cog-outgoing-set Impl))
         (TyVs (car Impl-outgoings))
         (P (cadr Impl-outgoings))
         (Q (caddr Impl-outgoings))
         (terms (select-conditioned-substitution-terms TyVs P)))
    (if terms
        (let* ((put (PutLink (LambdaLink TyVs Q) terms))
               (inst (cog-execute! put)))
          (extract-hypergraph put)
          (cog-set-tv! inst (cog-tv Impl))))))
(define implication-full-instantiation-rule-name
  (DefinedSchemaNode "implication-full-instantiation-rule"))
(DefineLink implication-full-instantiation-rule-name
  implication-full-instantiation-rule)
(define implication-partial-instantiation-variables
  (VariableSet
     (TypedVariableLink
        (VariableNode "$TyVs")
        (TypeNode "VariableSet")
        (TypeNode "VariableList"))
     (VariableNode "$P")
     (VariableNode "$Q")))
(define implication-partial-instantiation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm-eager: implication-partial-instantiation")
     (ListLink
        implication-instantiation-body)))
(define implication-partial-instantiation-rule
  (BindLink
     implication-partial-instantiation-variables
     implication-instantiation-body
     implication-partial-instantiation-rewrite))
(define (implication-partial-instantiation Impl)
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