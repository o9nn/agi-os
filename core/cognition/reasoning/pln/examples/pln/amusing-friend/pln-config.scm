(use-modules (opencog))
(use-modules (opencog ure))
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target) (cog-bc pln-rbs target))
(define pln-rules-dir "../../../opencog/pln/rules/")
(define (append-rule-dir basename) (string-append pln-rules-dir basename))
(define rule-basenames
  (list "wip/implication-instantiation.scm"
        "wip/implication-scope-to-implication.scm"
        "wip/equivalence-to-implication.scm"
        "wip/predicate-lambda-evaluation.scm"
        "wip/inversion.scm"
        "wip/implication-implicant-conjunction.scm"
        "wip/and-lambda-factorization-double-implication.scm"
        "term/deduction.scm"
        "wip/implication-to-implication-scope.scm"
        "wip/equivalence-scope-distribution.scm"
        "wip/and-introduction.scm"
        )
  )
(define rule-files (map append-rule-dir rule-basenames))
(use-modules (opencog logger))
(for-each load rule-files)
(define rules
  (list implication-scope-to-implication-rule-name
        implication-total-instantiation-rule-name
        equivalence-to-implication-rule-name
        predicate-lambda-evaluation-rule-name
        inversion-implication-rule-name
        implication-implicant-conjunction-rule-name
        and-lambda-factorization-double-implication-rule-name
        deduction-implication-rule-name
        implication-to-implication-scope-rule-name
        equivalence-scope-distribution-rule-name
        and-introduction-grounded-evaluation-rule-name
        )
  )
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 50000)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)