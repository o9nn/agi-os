(use-modules (opencog))
(use-modules (opencog ure))
(load-from-path "opencog/pln/pln-utils.scm")
(load-from-path "opencog/ure/ure-utils.scm")
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target) (cog-bc pln-rbs target))
(add-to-load-path "../../../../opencog/pln/rules/")
(add-to-load-path "../../../opencog/pln/rules/")
(define rule-filenames
  (list "wip/implication-instantiation.scm"
        "wip/implication-scope-to-implication.scm"
        "wip/and-lambda-distribution.scm"
        "wip/closed-lambda-evaluation.scm"
        "wip/implication-introduction.scm"
        "wip/implication-implicant-distribution.scm"
        "wip/implication-and-lambda-factorization.scm"
        "term/deduction.scm"
        "wip/equivalence-to-implication.scm"
        "wip/implication-implicant-disjunction.scm"
        )
  )
(for-each load-from-path rule-filenames)
(define rules
  (list implication-partial-instantiation-rule-name
        implication-scope-to-implication-rule-name
        and-lambda-distribution-rule-name
        closed-lambda-evaluation-rule-name
        implication-introduction-rule-name
        implication-implicant-distribution-rule-name
        implication-and-lambda-factorization-rule-name
        deduction-implication-rule-name
        implication-total-instantiation-rule-name
        equivalence-to-implication-rule-name
        implication-implicant-disjunction-rule-name
        )
  )
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 1000000)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)