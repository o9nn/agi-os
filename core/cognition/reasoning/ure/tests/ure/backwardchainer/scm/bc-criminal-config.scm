(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(load-from-path "tests/ure/rules/bc-deduction-rule.scm")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")
(define rule-confidence 0.5)
(MemberLink (stv 1 rule-confidence)
   conditional-full-instantiation-meta-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 0.6 rule-confidence)
   bc-deduction-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 rule-confidence)
   fuzzy-conjunction-introduction-1ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 rule-confidence)
   fuzzy-conjunction-introduction-2ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 rule-confidence)
   fuzzy-conjunction-introduction-3ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 rule-confidence)
   fuzzy-conjunction-introduction-4ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 rule-confidence)
   fuzzy-conjunction-introduction-5ary-rule-name
   (ConceptNode "URE")
)
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "500")
)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "URE")
)
(ExecutionLink
   (SchemaNode "URE:complexity-penalty")
   (ConceptNode "URE")
   (NumberNode "1")
)