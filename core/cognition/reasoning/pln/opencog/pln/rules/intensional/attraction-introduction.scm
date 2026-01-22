(define subset-attraction-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (CT (TypeInh "ConceptNode")))
    (BindLink
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT))
      (Present
        (Subset A B)
        (Subset (Not A) B))
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: attraction-introduction")
        (ListLink
          (Attraction A B)
          (Subset A B)
          (Subset (Not A) B))))))
(define (attraction-introduction conclusion . premises)
  (if (= (length premises) 2)
      (let* ((ATT conclusion)
             (SAB (car premises))
             (SNAB (cadr premises))
             (ATTs (max 0 (- (cog-mean SAB) (cog-mean SNAB))))
             (ATTc (min (cog-confidence SAB) (cog-confidence SNAB)))
             (ATTtv (stv ATTs ATTc)))
        (if (< 0 ATTc) (cog-merge-hi-conf-tv! ATT ATTtv)))))
(define subset-attraction-introduction-rule-name
  (DefinedSchemaNode "subset-attraction-introduction-rule"))
(DefineLink subset-attraction-introduction-rule-name
  subset-attraction-introduction-rule)