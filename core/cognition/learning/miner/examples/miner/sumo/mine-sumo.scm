(use-modules (opencog miner))
(use-modules (opencog ure))
(use-modules (opencog logger))
(use-modules (opencog randgen))
(ure-logger-set-level! "debug")
(cog-randgen-set-seed! 0)
(define (run-sumo-miner kb ms mi mc mv)
(clear)
(load kb)
(let* (
(scope? (lambda (x) (cog-subtype? 'ScopeLink (cog-type x))))
(db
(filter (lambda (x) (not (scope? x))) (cog-get-all-roots)))
(db-cpt (fill-db-cpt (Concept "sumo-db") db))
(msg-1 (cog-logger-info "Run pattern miner over ~a" kb))
(results (cog-mine db-cpt
#:minsup ms
#:maximum-iterations mi
#:conjunction-expansion #t
#:max-conjuncts mc
#:max-variables mv
#:surprisingness 'nisurp))
(msg-2 (cog-logger-info "Results from mining ~a:\n~a" kb results)))
*unspecified*)
)
(for-each (lambda (args) (apply run-sumo-miner args))
(list
(list "scm/Geography.scm" 5 500 2 2)
)
)