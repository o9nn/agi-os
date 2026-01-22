(use-modules (opencog pln))
(pln-load-rule 'fuzzy-conjunction-introduction-2ary)
(pln-load-rule 'conditional-total-instantiation-implication-scope-meta)
(use-modules (opencog logger))
(use-modules (opencog ure))
(ure-logger-set-level! "debug")
(load "kb.scm")
(define what (Variable "$what"))
(define vardecl (TypedVariable what (Type "ConceptNode")))
(define sources (Set
                  song-3-composed-by-author-2
                  marry-like-song-3
                  listener-like-song-from-same-author))
(define target (Evaluation like (List marry what)))
(define fc-results
  (pln-fc sources #:maximum-iterations 20 #:fc-retry-exhausted-sources #t))