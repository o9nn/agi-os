(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")
(define rbs (ConceptNode "URE"))
(ure-add-rules rbs (list conditional-full-instantiation-meta-rule-name
			 fuzzy-conjunction-introduction-2ary-rule-name))
(ure-set-maximum-iterations rbs 20)