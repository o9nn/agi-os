(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog persist) (opencog persist-file))
(define fsn
(FileStorage "/usr/local/share/link-grammar/demo-atomese/atomese-dict.scm"))
(cog-open fsn)
(define pda
(LgParseDisjuncts
(PhraseNode "level playing field")
(LgDictNode "demo-atomese")
(NumberNode 4)
(cog-atomspace)
fsn))
(cog-execute! pda)
(cog-prt-atomspace)