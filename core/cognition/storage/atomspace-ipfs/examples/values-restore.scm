(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog persist-ipfs))
(ipfs-open "ipfs:///")
(ipfs-fetch-atom "bafyreifhaduwgp46ho6odbjcdjgd6v3r2hxztonufgtzqgaebowrnhia5a")
(define c (ConceptNode "example concept"))
(cog-keys c)
(for-each
	(lambda (key)
		(format #t "Key ~A   has value ~A\n" key (cog-value c key)))
	(cog-keys c))
(define e
	(ipfs-fetch-atom "bafyreiaak6j7psknn5id7d456jaxaqxq7xjczmi7boj4zg6pqhgr5oeuuu"))
(cog-keys e)
(for-each
	(lambda (key)
		(format #t "Key ~A   has value ~A\n" key (cog-value e key)))
	(cog-keys e))
(ipfs-close)