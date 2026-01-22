(define (gen-replacement-rule TYPE)
  (define (synonymous A B)
    (Evaluation (Predicate "synonymous") (List A B)))
  (let* ([semi-open (Interval (Number 0) (Number -1))]
	 [LG (Glob "$LG")]
	 [RG (Glob "$RG")]
	 [A (Variable "$A")]
	 [B (Variable "$B")])
    (Bind
      (VariableList
        (TypedVariable LG semi-open)
	(TypedVariable RG semi-open)
	A
	B)
      (Present
        (synonymous A B)
	(TYPE LG A RG))
      (ExecutionOutput
        (GroundedSchema "scm: replacement")
        (List
	  (synonymous
            (TYPE LG A RG)
	    (TYPE LG B RG))
	  (synonymous A B)
	  (TYPE LG A RG))))))
(define (replacement conclusion . premises)
  conclusion)
(define evaluation-replacement-rule-name
  (DefinedSchema "evaluation-replacement-rule"))
(Define
  (DefinedSchema "evaluation-replacement-rule")
  (gen-replacement-rule EvaluationLink))
(define similarity-replacement-rule-name
  (DefinedSchema "similarity-replacement-rule"))
(Define
  (DefinedSchema "similarity-replacement-rule")
  (gen-replacement-rule SimilarityLink))
(define list-replacement-rule-name
  (DefinedSchema "list-replacement-rule"))
(Define
  (DefinedSchema "list-replacement-rule")
  (gen-replacement-rule ListLink))
(define set-replacement-rule-name
  (DefinedSchema "set-replacement-rule"))
(Define
  (DefinedSchema "set-replacement-rule")
  (gen-replacement-rule SetLink))
(define rrb (Concept "replacement-rb"))
(ure-add-rules rrb (list evaluation-replacement-rule-name
			 similarity-replacement-rule-name
			 list-replacement-rule-name
			 set-replacement-rule-name))