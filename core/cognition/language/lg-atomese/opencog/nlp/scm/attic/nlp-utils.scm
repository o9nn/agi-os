(use-modules (srfi srfi-1))
(use-modules (opencog))
(define-public (document-get-sentences DOCO)
"
document-get-sentences DOCO -- Get sentences in document DOCO
Given a document DOCO, return a list of sentences in that document.
Throws an error if DOCO is not a DocumentNode
"
(if (eq? (cog-type DOCO) 'DocumentNode)
(cog-get-reference DOCO)
(throw 'wrong-atom-type 'document-get-sentences
"Error: expecting DocumentNode:" DOCO)
)
)
(define-public (sentence-get-parses sent-node)
"
sentence-get-parses    Get parses of a sentence.
sentence-get-parses sent-node
Given a sentence, return a list of parses in that sentence
Basically, chase a ParseLink to a ParseNode
Throws an error if sent-node is not a SentenceNode
"
(cog-chase-link-chk 'ParseLink 'ParseNode sent-node 'SentenceNode)
)
(define-public (sent-list-get-parses sent-list)
"
sent-list-get-parses   Get parses of a list of sentences.
Given a list of sentences, return a list of parses of those sentences.
That is, given a List of SentenceNode's, return a list of ParseNode's
associated with those sentences.
"
(concatenate! (map sentence-get-parses sent-list))
)
(define-public (sentence-get-utterance-type sent)
"
sentence-get-utterance-type SENT -- Check the utterance speech act type
Expect SENT to be (SentenceNode \"sentence@45c470a6-29...\")
Will return (DefinedLinguisticConceptNode ACT) where ACT is
one of DeclarativeSpeechAct, InterrogativeSpeechAct,
TruthQuerySpeechAct, etc...
"
(define parse (car (cog-chase-link 'ParseLink 'ParseNode sent)))
(define interp (car
(cog-chase-link 'InterpretationLink 'InterpretationNode parse)))
(define act-type (cog-chase-link
'InheritanceLink 'DefinedLinguisticConceptNode interp))
act-type
)
(define-public (sent-get-words-in-order sent-node)
"
sent-get-words-in-order - Given a sentence, return a list of all
of the word-instances in each parse, in order.
Given a sentence, return all word instances in order
"
(map parse-get-words-in-order (sentence-get-parses sent-node))
)
(define-public (parse-get-links parse-node)
"
parse-get-links    Get all LG links in a parse.
Given a parse, return a list of all LG links in that parse, i.e.
the EvaluationLinks of the form
EvaluationLink
LgLinkNode 'foo'
ListLink
WordInstanceNode 'dog@12345'
WordInstanceNode 'toy@6789'
"
(delete-duplicates!
(concatenate!
(map word-inst-get-links
(parse-get-words parse-node))))
)
(define-public (sent-get-interp sent-node)
"
sent-get-interp - Given a SentenceNode returns a list of InterpretationNodes
XXX fix-me -- might this not be parse-dependent???
"
(parse-get-interp (car (sentence-get-parses sent-node)))
)
(define-public (parse-get-words parse-node)
"
parse-get-words - Return a list of all word-instances in the parse.
Given a parse, return all word instances in arbitrary order
This version is faster than the ordered version.
"
(cog-chase-link 'WordInstanceLink 'WordInstanceNode parse-node)
)
(define-public (parse-get-words-in-order PARSE)
"
parse-get-words-in-order - Given PARSE, return a list of all word
instances in the parse, in sequential order.
"
(define (get-number word-inst)
(cog-number (word-inst-get-number word-inst)))
(define (less-than word-inst-1 word-inst-2)
(< (get-number word-inst-1) (get-number word-inst-2)))
(sort (parse-get-words PARSE) less-than)
)
(define-public (parse-get-interp parse-node)
"
parse-get-interp    Get the interpretations of the parse.
Returns the InterpretationNodes associated with a ParseNode.
"
(cog-chase-link 'InterpretationLink 'InterpretationNode parse-node)
)
(define-public (interp-get-parse interp)
"
interp-get-parse    Get the parse that resulted in the given interpretation.
Returns the ParseNode associated with an InterpretationNode.
"
(car (cog-chase-link 'InterpretationLink 'ParseNode interp))
)
(define-public (word-inst-get-parse word-inst)
"
word-inst-get-parse   Return the ParseNode associated with word-inst
Return the ParseNode associated with 'word-inst'
"
(car (cog-chase-link 'WordInstanceLink 'ParseNode word-inst))
)
(define-public (word-inst-get-number word-inst)
"
word-inst-get-number   Return the NumberNode associated with word-inst
Return the NumberNode associated with 'word-inst'. This number is
the ordinal of the word in the sentence.  There can only ever be one
such ordinal per WordInstance, so this returns a single atom.
"
(car (cog-chase-link 'WordSequenceLink 'NumberNode word-inst))
)
(define-public (word-inst-get-word word-inst)
"
word-inst-get-word  -- Return the WordNode associated with word-inst
Return the WordNode associated with 'word-inst'. For example, given
`(WordInstance 'olah@12345')`, return `(WordNode 'olah')`
There can only ever be one WordNode per WordInstance, so this returns
a single atom.  However ...
However, due to a RelEx bug, this function might throw an exception.
Specifically, if the word in a sentence is a parenthesis, then the
ReferenceLink between the specific paren, and the general paren does
not get created.  Viz, there is no `(ReferenceLink (WordInstanceNode
'(@4bf5e341-c6b') (WordNode '('))`. Some paren-counter somewhere is
broken and gets confused. Beats me where. It should be fixed. In the
meanhile, a 'wrong-type-arg exception is thrown, when the `car` below
dereferences the empty list. Callers of this function may want to
catch this exception.
"
(car (cog-chase-link 'ReferenceLink 'WordNode word-inst))
)
(define-public (word-inst-get-links word-inst)
"
word-inst-get-links       Get LG links involving word-inst
Given a word instance, return a list of all LG links that the
word-instance participates in.  For example, given
`(WordInstanceNode 'dog@12345')`, return all EvaluationLinks
of the form
Evaluati`onLink
LgLinkNode 'foo'
ListLink
WordInstanceNode 'dog@12345'
WordInstanceNode 'toy@6789'
with the WordInstance in either the first or the second place.
"
(cog-get-pred word-inst 'LgLinkNode)
)
(define-public (word-inst-get-senses word-inst)
"
word-inst-get-senses   Get word senses associated with word.
Given a word instance, return a list of the word-senses associated
with the word.
"
(cog-chase-link 'InheritanceLink 'WordSenseNode word-inst)
)
(define-public (word-inst-sense-score word-inst word-sense)
"
word-inst-sense-score  Get ranking score for word-inst & word-sense.
Given a word-instance and a word-sense, return the numerical
word-sense ranking score. The rank is stored as the 'count' part of the
count truth value of the InheritanceLink connecting the two: i.e. in
the structure
InheritanceLink (ctv 1.0 0.0 -0.006025)
WordInstanceNode 'day@171506d8f3'
WordSenseNode 'daylight%1:28:00::'
If there is no such link, return -999 as the score.
"
(let ((slink (cog-link 'InheritanceLink word-inst word-sense)))
(if (nil? slink) -999.0 (cog-count slink)))
)
(define-public (word-inst-get-cset word-inst)
"
word-inst-get-cset -- Get the connector set (LgWordCset) attached
to the word-inst.  For example, this returns
(LgWordCset
(WordInstanceNode 'foobar@62e9c582-1984')
(LgAnd ..))
"
(car (filter!
(lambda (x) (eq? (cog-type x) 'LgWordCset))
(cog-incoming-set word-inst)))
)
(define-public (word-inst-get-disjunct word-inst)
"
word-inst-get-disjunct -- Get the disjunct (LgAnd) used for a word-inst.
"
(car (cog-chase-link 'LgWordCset 'LgAnd word-inst))
)
(define-public (delete-sentence sent)
"
delete-sentence SENT -- delete all atoms associated with a sentence.
Delete the parses and word-instances associated with the sentence,
including LemmaLink's, ReferenceLinks, RelEx relations, and
link-grammar linkages.
Only the atoms in the atomspace are removed
backingstore (database), they are untouched.
Expect SENT to be a SentenceNode
expected to be:
ParseLink
ParseNode
SentenceNode
"
(define (extract-link-instance li)
(if (cog-atom? li) (cog-extract-recursive! li))
)
(define (extract-word-instance wi)
(for-each
(lambda (x)
(cond
((eq? 'ListLink (cog-type x))
(for-each extract-link-instance
(cog-chase-link 'EvaluationLink 'LgLinkInstanceNode x)))
((eq? 'WordSequenceLink (cog-type x))
(let ((oset (cog-outgoing-set x)))
(cog-extract! x)
(cog-extract! (cadr oset))))))
(cog-incoming-set wi))
(cog-extract-recursive! wi)
)
(define (extract-parse parse)
(for-each
(lambda (x)
(if (eq? 'WordInstanceLink (cog-type x))
(extract-word-instance (car (cog-outgoing-set x)))))
(cog-incoming-set parse)
)
(cog-extract-recursive! parse)
)
(for-each
(lambda (x)
(cond
((eq? 'ParseLink (cog-type x))
(extract-parse (car (cog-outgoing-set x))))
((eq? 'SentenceSequenceLink (cog-type x))
(let ((oset (cog-outgoing-set x)))
(cog-extract! x)
(cog-extract! (cadr oset))))
)
)
(cog-incoming-set sent)
)
(cog-extract! sent)
)
(define-public (delete-sentences)
"
delete-sentences       Delete all atoms that occur in sentences
Delete atoms that belong to particular sentence instances
want to clog up the atomspace with old junk we don't want any more.
Only the atoms in the atomspace are removed
backingstore (database), they are untouched.
XXX TODO: In principle, this could be accomplished by lowering the
AttentionValue for the atoms, and letting the ForgettingAgent do its
work. In practice, this is too complicated, for now.
XXX: Some of these nodes & links are needed by the language generation
pipeline for matching to old sentences, so maybe they cannot be
removed.
"
(let ((n 0))
(define (delit atom) (cog-extract-recursive! atom) (set! n (+ n 1)) #f)
(define (delone atom) (extract-hypergraph atom) (set! n (+ n 1)) #f)
(cog-map-type delone 'ParseLink)
(cog-map-type delone 'ReferenceLink)
(cog-map-type delone 'LgLinkInstanceLink)
(cog-map-type delone 'CosenseLink)
(cog-map-type delone 'SimilarityLink)
(cog-map-type delit 'DocumentNode)
(cog-map-type delit 'SentenceNode)
(cog-map-type delit 'ParseNode)
(cog-map-type delit 'WordInstanceNode)
(cog-map-type delit 'LgLinkInstanceNode)
(system (string-join (list "echo deleted: " (number->string n) )))
)
)