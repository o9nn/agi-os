(use-modules (opencog)
             (opencog miner))
(define AB
(Inheritance
  (Concept "A")
  (Concept "B")))
(define AC
(Inheritance
  (Concept "A")
  (Concept "C")))
(define results-as (cog-mine (cog-atomspace)
                             #:minsup 2
                             #:conjunction-expansion #f
                             #:surprisingness 'none))
(define results-lst (cog-mine (list AB AC)
                              #:minsup 2
                              #:conjunction-expansion #f
                              #:surprisingness 'none))
(define results-mf (cog-mine (list AB AC)
                             #:minfreq 1
                             #:conjunction-expansion #f
                             #:surprisingness 'none))