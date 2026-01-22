(use-modules (opencog))
(use-modules (opencog ure))
(define pln-rbs (ConceptNode "PLN"))
(define (pln-bc . args)
(apply cog-bc (cons pln-rbs args)))
(add-to-load-path "../../../opencog/pln/")
(define rule-filenames
(list "rules/propositional/modus-ponens.scm"
"rules/propositional/contraposition.scm"
"rules/term/deduction.scm"
"meta-rules/predicate/conditional-total-instantiation.scm"
)
)
(for-each load-from-path rule-filenames)
(define rules
(list
modus-ponens-inheritance-rule-name
modus-ponens-implication-rule-name
modus-ponens-subset-rule-name
contraposition-inheritance-rule-name
contraposition-implication-rule-name
crisp-contraposition-implication-scope-rule-name
deduction-inheritance-rule-name
deduction-implication-rule-name
deduction-subset-rule-name
conditional-total-instantiation-implication-scope-meta-rule-name
conditional-total-instantiation-implication-meta-rule-name
conditional-total-instantiation-inheritance-meta-rule-name
)
)
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" piter)
(ure-set-num-parameter pln-rbs "URE:complexity-penalty" 0.1)
(ure-set-num-parameter pln-rbs "URE:BC:MM:compressiveness" 0.5)
(ure-set-num-parameter pln-rbs "URE:BC:MM:complexity-penalty" 0.5)