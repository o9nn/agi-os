(define evaluation-to-member-0-rule
	(BindLink
		(VariableList
			(TypedVariableLink
				(VariableNode "$A")
				(TypeNode "ConceptNode"))
			(TypedVariableLink
				(VariableNode "$D")
				(TypeNode "PredicateNode")))
		(EvaluationLink
			(VariableNode "$D")
			(VariableNode "$A"))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: evaluation-to-member-0-formula")
				(ListLink
					(MemberLink
						(VariableNode "$A")
						(SatisfyingSetScopeLink
							(VariableNode "$X")
							(EvaluationLink
								(VariableNode "$D")
								(VariableNode "$X"))))
					(EvaluationLink
						(VariableNode "$D")
						(VariableNode "$A"))))))
(define (evaluation-to-member-0-formula MAXDX DA)
	(cog-set-tv! MAXDX
		(evaluation-to-member-side-effect-free-formula
			MAXDX
			DA)))
(define evaluation-to-member-1-rule
	(BindLink
		(VariableList
			(VariableNode "$A")
			(TypedVariableLink
				(VariableNode "$D")
				(TypeNode "PredicateNode")))
		(EvaluationLink
			(VariableNode "$D")
			(ListLink
				(VariableNode "$A")))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: evaluation-to-member-1-formula")
				(ListLink
					(MemberLink
						(VariableNode "$A")
						(SatisfyingSetScopeLink
							(VariableNode "$X")
							(EvaluationLink
								(VariableNode "$D")
								(ListLink
									(VariableNode "$X")))))
					(EvaluationLink
						(VariableNode "$D")
						(ListLink
							(VariableNode "$A")))))))
(define (evaluation-to-member-1-formula MAXDX DA)
	(cog-set-tv! MAXDX
		(evaluation-to-member-side-effect-free-formula
			MAXDX
			DA)))
(define evaluation-to-member-2-rule
	(BindLink
		(VariableList
			(VariableNode "$A")
			(VariableNode "$B")
			(TypedVariableLink
				(VariableNode "$D")
				(TypeNode "PredicateNode")))
		(EvaluationLink
			(VariableNode "$D")
			(ListLink
				(VariableNode "$A")
				(VariableNode "$B")))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: evaluation-to-member-2-formula")
				(ListLink
					(MemberLink
						(VariableNode "$A")
						(SatisfyingSetScopeLink
							(VariableNode "$X")
							(EvaluationLink
								(VariableNode "$D")
								(ListLink
									(VariableNode "$X")
									(VariableNode "$B")))))
					(MemberLink
						(VariableNode "$B")
						(SatisfyingSetScopeLink
							(VariableNode "$Y")
							(EvaluationLink
								(VariableNode "$D")
								(ListLink
									(VariableNode "$A")
									(VariableNode "$Y")))))
					(EvaluationLink
						(VariableNode "$D")
						(ListLink
							(VariableNode "$A")
							(VariableNode "$B")))))))
(define (evaluation-to-member-2-formula MAXDXB MBXDAX DAB)
  (List
    (cog-set-tv! MAXDXB
		(evaluation-to-member-side-effect-free-formula
			MAXDXB
			DAB))
	(cog-set-tv! MBXDAX 
		(evaluation-to-member-side-effect-free-formula
			MBXDAX
			DAB))))
(define (evaluation-to-member-side-effect-free-formula MD ED)
	(stv
		(cog-mean ED)
		(cog-confidence ED)))
(define evaluation-to-member-0-rule-name
  (DefinedSchemaNode "evaluation-to-member-0-rule"))
(DefineLink
  evaluation-to-member-0-rule-name
  evaluation-to-member-0-rule)
(define evaluation-to-member-1-rule-name
  (DefinedSchemaNode "evaluation-to-member-1-rule"))
(DefineLink
  evaluation-to-member-1-rule-name
  evaluation-to-member-1-rule)
(define evaluation-to-member-2-rule-name
  (DefinedSchemaNode "evaluation-to-member-2-rule"))
(DefineLink
  evaluation-to-member-2-rule-name
  evaluation-to-member-2-rule)