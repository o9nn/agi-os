(use-modules (opencog))
(use-modules (opencog ure))
(load-from-path "utilities.scm")
(load-from-path "av-tv.scm")
(load-from-path "opencog/ure/ure-utils.scm")
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target) (cog-bc pln-rbs target))
(add-to-load-path "../../../opencog/pln/rules")
(add-to-load-path "../../../opencog/pln/meta-rules")
(define rule-filenames
  (list
        "predicate/conditional-total-instantiation.scm"
        "propositional/fuzzy-conjunction-introduction.scm"
        "propositional/contraposition.scm"
  )
)
(for-each load-from-path rule-filenames)
(define rules
  (list
        conditional-total-instantiation-implication-scope-meta-rule-name
        conditional-total-instantiation-implication-meta-rule-name
        conditional-total-instantiation-inheritance-meta-rule-name
        fuzzy-conjunction-introduction-3ary-rule-name
        crisp-contraposition-implication-scope-rule-name
  )
)
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 500)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)
(ure-set-num-parameter pln-rbs "URE:complexity-penalty" 1)
(ure-set-num-parameter pln-rbs "URE:BC:maximum-bit-size" 50000)