(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (srfi srfi-1))
(cog-execute!
(LgParseMinimal
(PhraseNode "this is a test.")
(LgDictNode "en")
(NumberNode 1)))
(cog-prt-atomspace)
(define (get-parses SENT)
(cog-execute! (Meet (Present (Parse (Variable "?parse") SENT)))))
(get-parses sent)
(define pars (first (cog-value->list (get-parses sent))))
(define (get-word-instances PARS)
(cog-execute! (Meet (Present (WordInstance (Variable "?wrd") PARS)))))
(get-word-instances pars)
(define (get-words PARS)
(define qry
(Query
(VariableList (Variable "?winst") (Variable "?wrd"))
(Present
(WordInstance (Variable "?winst") PARS)
(Reference (Variable "?winst") (Variable "?wrd")))
(Variable "?wrd")))
(cog-execute! qry))
(get-words pars)
(define (get-word-seq PARS)
(define qry
(Query
(VariableList
(Variable "?winst")
(Variable "?wrd")
(Variable "?wseq"))
(Present
(WordInstance (Variable "?winst") PARS)
(WordSequence (Variable "?winst") (Variable "?wseq"))
(Reference (Variable "?winst") (Variable "?wrd")))
(List (Variable "?wseq") (Variable "?wrd"))))
(define qwords (cog-execute! qry))
(define wds (cog-value->list qwords))
(map
(lambda (APAIR)
(cons
(cog-name (cog-value-ref APAIR 0))
(cog-name (cog-value-ref APAIR 1))))
wds)
)
(get-word-seq pars)
(define (get-links PARS)
(define qry
(Query
(VariableList
(TypedVariable (Variable "?winst") (Type 'WordInstanceNode))
(TypedVariable (Variable "?wi2") (Type 'WordInstanceNode))
(TypedVariable (Variable "?lnk") (Type 'LgLinkNode)))
(Present
(WordInstance (Variable "?winst") PARS)
(Evaluation
(Variable "?lnk")
(List (Variable "?winst") (Variable "?wi2"))))
(Evaluation
(Variable "?lnk")
(List (Variable "?winst") (Variable "?wi2")))))
(cog-execute! qry))
(get-links pars)
(cog-execute!
(LgParse
(PhraseNode "What a deal!")
(LgDictNode "en")
(NumberNode 1)))
(cog-prt-atomspace)