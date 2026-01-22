(use-modules (opencog))
(use-modules (opencog ure))
(define ppc-rbs (ConceptNode "post-process-corpus-rule-base"))
(define (ppc-bc . args)
(apply cog-bc (cons ppc-rbs args)))
(add-to-load-path "../../../opencog/pln/")
(define rule-filenames
(list "meta-rules/predicate/conditional-total-instantiation.scm"
"rules/propositional/fuzzy-conjunction-introduction.scm"
)
)
(for-each load-from-path rule-filenames)
(define rules
(list
conditional-total-instantiation-implication-scope-meta-rule-name
fuzzy-conjunction-introduction-2ary-rule-name
)
)
(ure-add-rules ppc-rbs rules)
(ure-set-num-parameter ppc-rbs "URE:maximum-iterations" 100)