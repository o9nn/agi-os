(define (gen-contraposition-rule link-type)
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (AB (link-type A B))
         (NBNA (link-type (Not B) (Not A)))
         (vardecl (VariableList A B))
         (clause AB)
         (precondition (Evaluation
                         (GroundedPredicate "scm: gt-zero-confidence")
                         AB))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm: contraposition")
                    (List
                      NBNA
                      AB
                      A
                      B))))
    (Bind
      vardecl
      (And
        (Present clause)
        precondition)
      rewrite)))
(define (gen-crisp-contraposition-scope-rule scope-link-type)
  (let* ((V (Variable "$V"))
         (P (Variable "$P"))
         (Q (Variable "$Q"))
         (PQ (Quote (scope-link-type
               (Unquote V)
               (Unquote P)
               (Unquote Q))))
         (NQNP (Quote (scope-link-type
                 (Unquote V)
                 (Unquote (Not Q))
                 (Unquote (Not P)))))
         (vardecl (VariableList
                    (TypedVariable V (TypeChoice
                                       (TypeNode "VariableNode")
                                       (TypeNode "VariableList")
                                       (TypeNode "TypedVariableLink")))
                    P Q))
         (clause PQ)
         (precondition (Evaluation
                         (GroundedPredicate "scm: crisp-contraposition-scope-precondition")
                         PQ))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm: crisp-contraposition-scope")
                    (List
                      NQNP
                      PQ))))
    (Bind
      vardecl
      (And
        clause
        precondition)
      rewrite)))
(define (crisp-contraposition-scope-precondition PQ)
  (bool->tv (and (< 0.999 (cog-mean PQ)) (< 0.999 (cog-confidence PQ)))))
(define (contraposition conclusion . premises)
  (if (= (length premises) 3)
    (let*
        ((NBNA conclusion)
         (AB (list-ref premises 0))
         (A (list-ref premises 1))
         (B (list-ref premises 2))
         (sAB (cog-mean AB))
         (cAB (cog-confidence AB))
         (sA (cog-mean A))
         (cA (cog-confidence A))
         (sB (cog-mean B))
         (cB (cog-confidence B)))
      (if (and (< 0.999 sAB) (< 0.999 cAB))
          (cog-merge-hi-conf-tv! NBNA (stv sAB cAB))
          (if (> 1 sB)
              (let* ((sNBNA ((+ 1 (- sA) (- sB) (* sAB sA)) / (+ 1 (- sB))))
                     (cNBNA (min cAB cA cB)))
                (cog-merge-hi-conf-tv! NBNA (stv sNBNA cNBNA))))))))
(define (crisp-contraposition-scope conclusion . premises)
  (if (= (length premises) 1)
    (let*
        ((NQNP conclusion)
         (PQ (list-ref premises 0))
         (sPQ (cog-mean PQ))
         (cPQ (cog-confidence PQ)))
      (if (and (< 0.999 sPQ) (< 0.999 cPQ))
          (cog-merge-hi-conf-tv! NQNP (stv sPQ cPQ))))))
(define crisp-contraposition-implication-scope-rule
  (gen-crisp-contraposition-scope-rule ImplicationScopeLink))
(define crisp-contraposition-implication-scope-rule-name
  (DefinedSchemaNode "crisp-contraposition-implication-scope-rule"))
(DefineLink crisp-contraposition-implication-scope-rule-name
  crisp-contraposition-implication-scope-rule)
(define contraposition-implication-rule
  (gen-contraposition-rule ImplicationLink))
(define contraposition-implication-rule-name
  (DefinedSchemaNode "contraposition-implication-rule"))
(DefineLink contraposition-implication-rule-name
  contraposition-implication-rule)
(define contraposition-inheritance-rule
  (gen-contraposition-rule InheritanceLink))
(define contraposition-inheritance-rule-name
  (DefinedSchemaNode "contraposition-inheritance-rule"))
(DefineLink contraposition-inheritance-rule-name
  contraposition-inheritance-rule)