(use-modules (opencog) (opencog exec))
(use-modules (opencog ure))
(define P (Predicate "P" (stv 0.1 1)))
(define Q (Predicate "Q" (stv 0.6 1)))
(ImplicationLink (stv 1 1) P Q)
(define rbs (ConceptNode "contraposition-rbs"))
(ure-add-rule rbs contraposition-implication-rule-name)
(define target
(ImplicationLink
  (NotLink Q)
  (NotLink P))
)