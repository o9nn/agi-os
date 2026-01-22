(define implication-introduction-vardecl
  (VariableList
     (TypedVariableLink
        (VariableNode "$P")
        (TypeChoice
           (TypeNode "PredicateNode")
           (TypeNode "LambdaLink")))
     (TypedVariableLink
        (VariableNode "$Q")
        (TypeChoice
           (TypeNode "PredicateNode")
           (TypeNode "LambdaLink")))))
(define implication-introduction-pattern
  (AndLink
     (VariableNode "$P")
     (VariableNode "$Q")
     (EvaluationLink
        (GroundedPredicateNode "scm: implication-introduction-precondition")
        (ListLink
           (VariableNode "$P")
           (VariableNode "$Q")))
     (NotLink
        (IdenticalLink
           (VariableNode "$P")
           (VariableNode "$Q")))))
(define implication-introduction-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: implication-introduction-formula")
     (ListLink
        (ImplicationLink
           (VariableNode "$P")
           (VariableNode "$Q"))
        (VariableNode "$P")
        (VariableNode "$Q"))))
(define implication-introduction-rule
  (BindLink
     implication-introduction-vardecl
     implication-introduction-pattern
     implication-introduction-rewrite))
(define (implication-introduction-precondition P Q)
  (bool->tv (< 0 (cog-tv-confidence (implication-introduction-stv-formula P Q)))))
(define (implication-introduction-stv-formula P Q)
  (let* (
         (P-s (cog-mean P))
         (P-c (cog-confidence P))
         (Q-s (cog-mean Q))
         (Q-c (cog-confidence Q))
         (Impl-s Q-s)
         (Impl-c (if (< 0.9 (* Q-s Q-c))
                        Q-c
                        (* P-c Q-c))))
    (stv Impl-s Impl-c)))
(define (implication-introduction-formula Impl P Q)
  (let ((Impl-tv (implication-introduction-stv-formula P Q)))
    (if (< 0 (cog-tv-confidence Impl-tv))
        (cog-merge-hi-conf-tv! Impl Impl-tv))))
(define implication-introduction-rule-name
  (DefinedSchemaNode "implication-introduction-rule"))
(DefineLink implication-introduction-rule-name
  implication-introduction-rule)