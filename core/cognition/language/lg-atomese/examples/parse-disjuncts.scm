(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(cog-execute!
	(LgParseDisjuncts
		(PhraseNode "this is a test.")
		(LgDictNode "en")
		(NumberNode 1)))
(cog-prt-atomspace)