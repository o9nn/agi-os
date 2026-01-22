(define member-deduction-rule
  (let* ((X (Variable "$X"))
         (A (Variable "$A"))
         (B (Variable "$B"))
         (CptT (Type 'Concept)))
    (Bind
      (VariableSet
        X
        (TypedVariable A CptT)
        (TypedVariable B CptT))
      (And
        (Present
          (Member X A)
          (Subset A B))
        (Evaluation
          (GroundedPredicate "scm: absolutely-true")
          (Member X A))
        (Evaluation
          (GroundedPredicate "scm: absolutely-true")
          (Subset A B)))
      (ExecutionOutput
        (GroundedSchema "scm: member-deduction")
        (List
          (Member X B)
          (Subset A B)
          (Member X A))))))
(define (member-deduction conclusion . premises)
  (if (= (length premises) 2)
      (let* ((SubAB (car premises))
             (MemXA (cadr premises))
             (tv-s (* (cog-mean SubAB) (cog-mean MemXA)))
             (tv-c (min (cog-confidence SubAB) (cog-confidence MemXA))))
        (if (and (= 1 tv-s) (= 1 tv-c))
            (cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c))))))
(define member-deduction-rule-name
  (DefinedSchemaNode "member-deduction-rule"))
(DefineLink member-deduction-rule-name
  member-deduction-rule)