(define implication-introduction-variables
  (VariableSet
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
(define implication-introduction-body
  (AndLink
     (PresentLink
        (VariableNode "$P")
        (VariableNode "$Q"))
     (NotLink
        (IdenticalLink
           (VariableNode "$P")
           (VariableNode "$Q")))))
(define implication-introduction-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm-eager: implication-introduction")
     (ListLink
        (VariableNode "$P")
        (VariableNode "$Q"))))
(define implication-introduction-rule
  (BindLink
     implication-introduction-variables
     implication-introduction-body
     implication-introduction-rewrite))
(define (implication-introduction P Q)
  (let* (
         (P-s (cog-mean P))
         (P-c (cog-confidence P))
         (Q-s (cog-mean Q))
         (Q-c (cog-confidence Q))
         (Impl-s Q-s)
         (Impl-c (if (< 0.9 (* Q-s Q-c))
                        Q-c
                        (* P-c Q-c))))
    (if (< 0 Impl-c)
        (cog-merge-hi-conf-tv!
         (ImplicationLink
            P
            Q)
         (cog-new-stv Impl-s Impl-c)))))
(define implication-introduction-rule-name
  (DefinedSchemaNode "implication-introduction-rule"))
(DefineLink implication-introduction-rule-name
  implication-introduction-rule)