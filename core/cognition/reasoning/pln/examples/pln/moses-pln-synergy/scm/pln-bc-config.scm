(use-modules (opencog))
(use-modules (opencog ure))
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target) (cog-bc pln-rbs target))
(add-to-load-path "../../../../opencog/pln/rules/")
(add-to-load-path "../../../../opencog/pln/meta-rules/")
(add-to-load-path "../../../opencog/pln/rules/")
(define rule-filenames
(list "predicate/conditional-partial-instantiation.scm"
"wip/implication-scope-to-implication.scm"
"wip/and-lambda-distribution.scm"
"wip/closed-lambda-introduction.scm"
"wip/implication-introduction.scm"
"wip/implication-implicant-distribution.scm"
"wip/implication-and-lambda-factorization.scm"
"term/deduction.scm"
"wip/equivalence-to-implication.scm"
"wip/implication-implicant-disjunction.scm"
"predicate/conditional-total-instantiation.scm"
)
)
(for-each load-from-path rule-filenames)
(define rules
(list
conditional-partial-instantiation-meta-rule-name
implication-scope-to-implication-rule-name
closed-lambda-introduction-rule-name
implication-introduction-rule-name
implication-implicant-distribution-rule-name
implication-and-lambda-factorization-rule-name
deduction-implication-rule-name
conditional-total-instantiation-meta-rule-name
equivalence-to-implication-rule-name
)
)
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 200000)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)
(ure-set-num-parameter pln-rbs "URE:complexity-penalty" 1)
(ure-set-num-parameter pln-rbs "URE:BC:maximum-bit-size" 100000)