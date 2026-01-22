(use-modules (opencog))
(use-modules (opencog exec))
(define (evaluation-to-member-2-rule-loose)
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
(ScopeLink
(VariableNode "$X")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X")
(VariableNode "$B")))))
(MemberLink
(VariableNode "$B")
(ScopeLink
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
(define (evaluation-to-member-2-rule)
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$A")
(TypeNode "ConceptNode"))
(TypedVariableLink
(VariableNode "$B")
(TypeNode "ConceptNode"))
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
(ScopeLink
(VariableNode "$X")
(EvaluationLink
(VariableNode "$D")
(ListLink
(VariableNode "$X")
(VariableNode "$B")))))
(MemberLink
(VariableNode "$B")
(ScopeLink
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
(define (evaluation-to-member-2-formula MAXDXB MBXDAX DAB) MAXDXB)