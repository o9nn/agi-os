(use-modules (opencog))
(use-modules (opencog ure))
(define propositional-rule-base (ConceptNode "propositional-rule-base"))
(define (prop-bc target) (cog-bc propositional-rule-base target))
(define pln-rules-dir "../../../opencog/pln/rules/")
(define (append-rule-dir basename) (string-append pln-rules-dir basename))
(define rule-basenames
(list "propositional/fuzzy-conjunction-introduction.scm"
"propositional/fuzzy-disjunction-introduction.scm"
"wip/negation-introduction.scm"
)
)
(define rule-files (map append-rule-dir rule-basenames))
(for-each load rule-files)
(define rules
(list
fuzzy-conjunction-introduction-1ary-rule-name
fuzzy-conjunction-introduction-2ary-rule-name
fuzzy-conjunction-introduction-3ary-rule-name
fuzzy-conjunction-introduction-4ary-rule-name
fuzzy-conjunction-introduction-5ary-rule-name
fuzzy-disjunction-introduction-1ary-rule-name
fuzzy-disjunction-introduction-2ary-rule-name
fuzzy-disjunction-introduction-3ary-rule-name
fuzzy-disjunction-introduction-4ary-rule-name
fuzzy-disjunction-introduction-5ary-rule-name
negation-introduction-rule-name
)
)
(ure-add-rules propositional-rule-base rules)
(ure-set-num-parameter propositional-rule-base "URE:maximum-iterations" 20)