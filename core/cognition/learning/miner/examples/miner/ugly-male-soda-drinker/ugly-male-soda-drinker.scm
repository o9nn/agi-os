(use-modules (opencog)
(opencog miner))
(use-modules (opencog ure))
(ure-logger-set-level! "debug")
(ure-logger-set-timestamp! #f)
(load "kb.scm")
(define results (cog-mine (cog-atomspace)
#:minsup 5
#:maximum-iterations 100
#:conjunction-expansion #t
#:maximum-conjuncts 3
#:maximum-variables 2
#:maximum-cnjexp-variables 1
#:surprisingness 'nisurp))