(use-modules (opencog)
             (opencog miner))
(define AB
(Inheritance
  (Concept "A")
  (Concept "B")))
(define BC
(Inheritance
  (Concept "B")
  (Concept "C")))
(define DE
(Inheritance
  (Concept "D")
  (Concept "E")))
(define EF
(Inheritance
  (Concept "E")
  (Concept "F")))
(define results (cog-mine (list AB BC DE EF)
                          #:minimum-support 2
                          #:initial-pattern (conjunct-pattern 2)
                          #:conjunction-expansion #f
                          #:maximum-variables 4
                          #:maximum-spcial-conjuncts 2
                          #:surprisingness 'none))