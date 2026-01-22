(load-from-path "tests/ure/meta-rules/conditional-partial-instantiation-meta-rule.scm")
(MemberLink (stv 1 1)
   conditional-partial-instantiation-meta-rule-name
   (ConceptNode "URE")
)
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "20")
)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "URE")
)