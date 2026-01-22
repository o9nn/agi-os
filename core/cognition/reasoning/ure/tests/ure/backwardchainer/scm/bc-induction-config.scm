(load "rules/implication-scope-direct-evaluation-rule.scm")
(load "meta-rules/conditional-full-instantiation-meta-rule.scm")
(MemberLink (stv 1 1)
implication-scope-direct-evaluation-rule-name
(ConceptNode "URE")
)
(MemberLink (stv 1 1)
conditional-full-instantiation-meta-rule-name
(ConceptNode "URE")
)
(ExecutionLink
(SchemaNode "URE:maximum-iterations")
(ConceptNode "URE")
(NumberNode "100")
)
(EvaluationLink (stv 0 1)
(PredicateNode "URE:attention-allocation")
(ConceptNode "URE")
)