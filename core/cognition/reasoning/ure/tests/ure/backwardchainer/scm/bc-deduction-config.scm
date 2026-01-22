(load-from-path "tests/ure/rules/bc-deduction-rule.scm")
(MemberLink (stv 1 1)
   bc-deduction-rule-name
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