(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog matrix))
(use-modules (srfi srfi-1))
(define pair-pipe-parser #f)
(define (get-pair-pipe-parser)
	(if (not pair-pipe-parser)
		(begin
			(set! pair-pipe-parser
				(make-random-pair-parser
					(ValueOf (Anchor "pair pipe") (Predicate "text src"))
					(cog-storage-node)))))
	pair-pipe-parser
)
(define (pair-obs-text TXT-STRING)
	(cog-set-value! (Anchor "pair pipe") (Predicate "text src")
		(StringValue TXT-STRING))
	(cog-execute! (get-pair-pipe-parser))
	(define any-sent (SentenceNode "ANY"))
	(count-one-atom any-sent)
)
#|
(use-modules (opencog learn))
(use-modules (opencog persist))
(use-modules (opencog persist-rocks))
(load "../common.scm")
(load "pipe-count.scm")
(define rsn (RocksStorageNode "rocks:///tmp/foo"))
(cog-open rsn)
(pair-obs-text "this is a test")
(cog-report-counts)
(cog-get-atoms 'AnyNode)
(cog-get-atoms 'WordNode)
(define CNT (PredicateNode "*-TruthValueKey-*"))
(cog-execute! (ValueOf (SentenceNode "ANY") CNT))
(cog-execute! (ValueOf (ParseNode "ANY") CNT))
(cog-execute! (ValueOf (WordNode "is") CNT))
(cog-execute! (ValueOf (Edge (Bond "ANY") (List (Word "is") (Word "a"))) CNT))
(cog-close rsn)
(load-atomspace)
|#
(define-public (make-block-random-pair-observer)
"
   make-block-random-pair-observer -- Make an observer for counting
   random pairs in text blocks. Returns a function of the following
   form:
   func TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for word-pair counting.
   TEXT-BLOCK is a utf8 string of text. A sliding window, of the default
   width of 9 words, is created on that block. Everything within the
   window is sent to the LG 'any' random-planar-tree parser. The word
   pairs in the random tree are then counted. Counts are stored.
"
	(make-observe-block pair-obs-text #:WIN-SIZE 9)
)