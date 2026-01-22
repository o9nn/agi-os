(use-modules (opencog))
(use-modules (opencog ure))
(define conjunction-rule-base (ConceptNode "conjunction-rule-base"))
(define (conj-bc target) (cog-bc conjunction-rule-base target))
(add-to-load-path "../../../opencog/pln/rules")
(load-from-path "propositional/fuzzy-conjunction-introduction.scm")
(define rules
(list
fuzzy-conjunction-introduction-1ary-rule-name
fuzzy-conjunction-introduction-2ary-rule-name
fuzzy-conjunction-introduction-3ary-rule-name
fuzzy-conjunction-introduction-4ary-rule-name
fuzzy-conjunction-introduction-5ary-rule-name
)
)
(ure-add-rules conjunction-rule-base rules)
(ure-set-num-parameter conjunction-rule-base "URE:maximum-iterations" 2)