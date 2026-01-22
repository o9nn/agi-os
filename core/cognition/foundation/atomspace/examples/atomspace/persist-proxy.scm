$ guile
scheme@(guile-user)>
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog cogserver))
(start-cogserver)
$ guile
scheme@(guile-user)>
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog persist-cog))
(define sto (CogStorageNode "cog://localhost:17001"))
(cog-open sto)
(cog-set-value!
	(WriteThruProxy "wthru mirror")
	(*-proxy-parts-*)
	(List
		(RocksStorageNode "rocks:///tmp/foo.rdb")
		(RocksStorageNode "rocks:///tmp/bar.rdb")))
(cog-set-value! sto (*-set-proxy-*) (WriteThruProxy "wthru mirror"))
(cog-set-value! sto (*-proxy-open-*) (VoidValue))
(store-atom (Concept "foo" (stv 0.3 0.6)))
(cog-set-value! (Concept "foo") (Predicate "bar") (FloatValue 1 2 3))
(store-value (Concept "foo") (Predicate "bar"))
(cog-set-value! (Concept "foo") (Predicate "fizz") (FloatValue 4 5 6))
(store-atom (Concept "foo"))
(cog-close sto)
$ guile
scheme@(guile-user)>
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog cogserver))
(start-cogserver)
$ guile
scheme@(guile-user)>
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog persist-cog))
(define sto (CogStorageNode "cog://localhost:17001"))
(cog-open sto)
(cog-set-value!
	(ReadThruProxy "rthru balance")
	(*-proxy-parts-*)
	(List
		(RocksStorageNode "rocks:///tmp/foo.rdb")
		(RocksStorageNode "rocks:///tmp/bar.rdb")))
(cog-set-value! sto (*-set-proxy-*) (ReadThruProxy "rthru balance"))
(cog-set-value! sto (*-proxy-open-*) (VoidValue))
(fetch-atom (Concept "foo"))
(cog-prt-atomspace)
(cog-keys (Concept "foo"))
(cog-value (Concept "foo") (Predicate "fizz"))
(cog-close sto)