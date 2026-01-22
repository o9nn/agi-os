(use-modules (opencog))
(use-modules (opencog ure))
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target) (cog-bc pln-rbs target))
(add-to-load-path "../../../opencog/pln/rules/")
(add-to-load-path "../../../opencog/pln/meta-rules/")
(define rule-filenames
  (list "predicate/conditional-total-instantiation.scm"
        "wip/implication-scope-to-implication.scm"
        "wip/predicate-lambda-evaluation.scm"
        "wip/inversion.scm"
        "wip/implication-implicant-conjunction.scm"
        "term/deduction.scm"
        )
  )
(for-each load-from-path rule-filenames)
(define rules
  (list implication-scope-to-implication-rule-name
        conditional-total-instantiation-meta-rule-name
        predicate-lambda-evaluation-rule-name
        inversion-implication-rule-name
        implication-implicant-conjunction-rule-name
        deduction-implication-rule-name
        )
  )
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 100000)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)
(ure-set-num-parameter pln-rbs "URE:complexity-penalty" 1)
(ure-set-num-parameter pln-rbs "URE:BC:maximum-bit-size" 20000)