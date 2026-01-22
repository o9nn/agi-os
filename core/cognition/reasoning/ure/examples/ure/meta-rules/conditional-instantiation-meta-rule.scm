(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(define conditional-full-instantiation-meta-variables
  (VariableSet
     (TypedVariable
        (Variable "$TyVs")
        (TypeChoice
           (Type "TypedVariableLink")
           (Type "VariableList")))
     (Variable "$P")
     (Variable "$Q")))
(define conditional-full-instantiation-meta-body
  (let* ((implication (Quote (ImplicationScope
                         (Unquote (Variable "$TyVs"))
                         (Unquote (Variable "$P"))
                         (Unquote (Variable "$Q")))))
         (precondition (Evaluation
                         (GroundedPredicate "scm: true-enough")
                         implication)))
  (And
    (Present implication)
    precondition)))
(define conditional-full-instantiation-meta-rewrite
  (let* ((TyVs (Variable "$TyVs"))
         (P (Variable "$P"))
         (Q (Variable "$Q"))
         (implication (Quote (ImplicationScope
                         (Unquote TyVs)
                         (Unquote P)
                         (Unquote Q)))))
    (Quote (Bind
      (Unquote TyVs)
      (And
        (Unquote (Present P))
        (Evaluation (GroundedPredicate "scm: true-enough") (Unquote P)))
      (ExecutionOutput
        (GroundedSchema "scm: conditional-full-instantiation")
        (Unquote
          (ListLink
            Q
            implication
            P)))))))
(define conditional-full-instantiation-meta-rule
  (BindLink
     conditional-full-instantiation-meta-variables
     conditional-full-instantiation-meta-body
     conditional-full-instantiation-meta-rewrite))
(define (conjunction-fuzzy-eval an)
  (let* ((outg (cog-outgoing-set an))
         (min-s-atom (min-element-by-key outg cog-mean))
         (min-c-atom (min-element-by-key outg cog-confidence))
         (min-s (cog-mean min-s-atom))
         (min-c (cog-confidence min-s-atom)))
    (stv min-s min-c)))
(define (true-enough-bool a)
  (let ((s (cog-mean a)) (c (cog-confidence a)))
    (and (> s 0.5) (> c 0.5))))
(define (true-enough a)
  (bool->tv (true-enough-bool a)))
(define (conditional-full-instantiation Q Impl P)
  (if (and (true-enough-bool Impl) (true-enough-bool P))
      (cog-set-tv! Q (stv 1 1))))
(define conditional-full-instantiation-meta-rule-name
  (DefinedSchemaNode "conditional-full-instantiation-meta-rule"))
(DefineLink conditional-full-instantiation-meta-rule-name
  conditional-full-instantiation-meta-rule)