(load "formulas.scm")
(define member-to-evaluation-0-rule
(BindLink
(VariableList
(VariableNode "$B")
(TypedVariableLink
(VariableNode "$D")
(TypeNode "PredicateNode")))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(VariableNode "$X-M2E"))))
(ExecutionOutputLink
(GroundedSchemaNode "scm: member-to-evaluation-formula")
(ListLink
(EvaluationLink
(VariableNode "$D")
(VariableNode "$B"))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(VariableNode "$X-M2E"))))))))
(define member-to-evaluation-1-rule
(BindLink
(VariableList
(VariableNode "$B")
(TypedVariableLink
(VariableNode "$D")
(TypeNode "PredicateNode")))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X-M2E")))))
(ExecutionOutputLink
(GroundedSchemaNode "scm: member-to-evaluation-formula")
(ListLink
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$B")))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X-M2E")))))))))
(define member-to-evaluation-2-1-rule
(BindLink
(VariableList
(VariableNode "$B")
(VariableNode "$C")
(TypedVariableLink
(VariableNode "$D")
(TypeNode "PredicateNode")))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X-M2E")
(VariableNode "$C")))))
(ExecutionOutputLink
(GroundedSchemaNode "scm: member-to-evaluation-formula")
(ListLink
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$B")
(VariableNode "$C")))
(MemberLink
(VariableNode "$B")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X-M2E")
(VariableNode "$C")))))))))
(define member-to-evaluation-2-2-rule
(BindLink
(VariableList
(VariableNode "$B")
(VariableNode "$C")
(TypedVariableLink
(VariableNode "$D")
(TypeNode "PredicateNode")))
(MemberLink
(VariableNode "$C")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$B")
(VariableNode "$X-M2E")))))
(ExecutionOutputLink
(GroundedSchemaNode "scm: member-to-evaluation-formula")
(ListLink
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$B")
(VariableNode "$C")))
(MemberLink
(VariableNode "$C")
(SatisfyingSetScopeLink
(VariableNode "$X-M2E")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$B")
(VariableNode "$X-M2E")))))))))
(define (member-to-evaluation-formula EVAL MEM)
(cog-set-tv! EVAL (cog-tv MEM)))
(define member-to-evaluation-0-rule-name (DefinedSchemaNode "member-to-evaluation-0-rule"))
(DefineLink member-to-evaluation-0-rule-name member-to-evaluation-0-rule)
(define member-to-evaluation-1-rule-name (DefinedSchemaNode "member-to-evaluation-1-rule"))
(DefineLink member-to-evaluation-1-rule-name member-to-evaluation-1-rule)
(define member-to-evaluation-2-1-rule-name (DefinedSchemaNode "member-to-evaluation-2-1-rule"))
(DefineLink member-to-evaluation-2-1-rule-name member-to-evaluation-2-1-rule)
(define member-to-evaluation-2-2-rule-name (DefinedSchemaNode "member-to-evaluation-2-2-rule"))
(DefineLink member-to-evaluation-2-2-rule-name member-to-evaluation-2-2-rule)