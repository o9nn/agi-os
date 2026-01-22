(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")
(MemberLink (stv 1 1)
   conditional-full-instantiation-meta-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-1ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-2ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-3ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-4ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-5ary-rule-name
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