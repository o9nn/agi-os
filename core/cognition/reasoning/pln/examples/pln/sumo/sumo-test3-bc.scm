(ure-logger-set-level! "debug")
(primitive-load "Merge.scm")
(load "pln-config3.scm")
(MemberLink (stv 1.000000 1.000000)
  (ConceptNode "Number3-1" (stv 0.010000 1.000000))
  (ConceptNode "NonnegativeRealNumber" (stv 0.010000 1.000000))
)
(define target
  (NotLink
    (MemberLink
      (ConceptNode "Number3-1" (stv 0.010000 1.000000))
      (ConceptNode "NegativeRealNumber" (stv 0.010000 1.000000))
    )
  )
)
(pln-bc target)