(use-modules (opencog))
(use-modules (opencog rule-engine))
(load-from-path "utilities.scm")
(load-from-path "av-tv.scm")
(load-from-path "opencog/rule-engine/rule-engine-utils.scm")
(define pln-rbs (ConceptNode "PLN"))
(InheritanceLink
   pln-rbs
   (ConceptNode "URE")
)
(define (pln-fc source) (cog-fc source pln-rbs))
(define (pln-bc target) (cog-bc target pln-rbs))
(add-to-load-path "/usr/local/share/opencog/scm/opencog/pln/")
(define rule-files (list
                         "rules/deduction-rule.scm"
                         "rules/implication-instantiation-rule.scm"
                         "rules/modus-ponens-rule.scm"
                         "rules/attraction-rule.scm"))
(for-each load-from-path rule-files)
(define local-rule-files (list  "local-rules/member-to-subset.scm"
                                "local-rules/subset-direct-evaluation-rule.scm"
                                "local-rules/singleton-similarity-rule.scm"
                                "local-rules/implication-conversion-rule.scm"
                                "local-rules/gene-similarity2overexpression-equivalence-rule.scm"
                                "local-rules/equivalence-transformation-rule.scm"))
(for-each load local-rule-files)
(define rules (list (list pln-rule-member-to-subset-name 1)
                    (list pln-rule-subset-direct-evaluation-name 1)
                    (list pln-rule-singleton-similarity-name 1)
                    (list pln-rule-intensional-implication-conversion-name 1)
                    (list gene-similarity2overexpression-equivalence-name 1)
                    (list deduction-intensional-implication-rule-name 1)
                    (list implication-full-instantiation-rule-name 1)
                    (list modus-ponens-implication-rule-name 1)
                    (list attraction-rule-name 1)
                    (list pln-rule-intensional-equivalence-transformation-name
                        1)
              )
)
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 20)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)