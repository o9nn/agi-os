(use-modules (opencog))
(use-modules (opencog ure))
(define pln-rbs (ConceptNode "PLN"))
(define (pln-fc source) (cog-fc pln-rbs source))
(define (pln-bc target vardecl) (cog-bc pln-rbs target #:vardecl vardecl))
(add-to-load-path "../../../opencog/pln/meta-rules/")
(define rule-filenames
  (list "predicate/universal-total-instantiation.scm"))
(for-each load-from-path rule-filenames)
(define rules
  (list
    universal-total-instantiation-forall-1ary-meta-rule-name))
(ure-add-rules pln-rbs rules)
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 10)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)
(ure-set-num-parameter pln-rbs "URE:complexity-penalty" 1)