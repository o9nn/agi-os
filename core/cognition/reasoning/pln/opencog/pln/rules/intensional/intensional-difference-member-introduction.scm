(define intensional-difference-member-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (X (Variable "$X"))
         (CT (TypeInh "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT)
        (TypedVariable X CT))
      (And
        (Present
          (IntensionalDifference A B)
          (Attraction A X)
          (Attraction B X)))
      (ExecutionOutput
        (GroundedSchema "scm: intensional-difference-member-introduction")
        (List
          (Member X (IntensionalDifference A B))
          (IntensionalDifference A B)
          (Attraction A X)
          (Attraction B X))))))
(define (intensional-difference-member-introduction conclusion . premises)
  (define (get-strength A-at B-at)
    (min (cog-mean A-at) (- 1 (cog-mean B-at))))
  (define (get-confidence A-at B-at)
    (min (cog-confidence A-at) (cog-confidence B-at)))
  (let* ((MembLink conclusion)
         (A-at (cadr premises))
         (B-at (caddr premises))
         (TVs (get-strength A-at B-at))
         (TVc (get-confidence A-at B-at))
         (TV (stv TVs TVc)))
    (if (< 0 TVc) (cog-merge-hi-conf-tv! MembLink TV))))
(define intensional-difference-member-introduction-rule-name
  (DefinedSchemaNode "intensional-difference-member-introduction-rule"))
(DefineLink intensional-difference-member-introduction-rule-name
  intensional-difference-member-introduction-rule)