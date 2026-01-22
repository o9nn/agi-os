(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog matrix))
(define disjunct-pipe-parser #f)
(define (get-disjunct-pipe-parser)
	(if (not disjunct-pipe-parser)
		(begin
			(set! disjunct-pipe-parser
				(make-disjunct-parser
					(ValueOf (Anchor "disjunct pipe") (Predicate "text src"))
					(cog-storage-node)))))
	disjunct-pipe-parser
)
(define (disjunct-obs-text TXT-STRING)
	(cog-set-value! (Anchor "disjunct pipe") (Predicate "text src")
		(StringValue TXT-STRING))
	(cog-execute! (get-disjunct-pipe-parser))
	(define mst-sent (SentenceNode "MST"))
	(count-one-atom mst-sent)
)
(define-public (make-block-mpg-pipe-observer)
"
   make-block-mpg-pipe-observer -- Make an observer for counting
      MST/MPG disjuncts in text blocks. Uses the Atomese pipeline.
   The returned function has the form:
   func TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for MPG/MST parsing.
   TEXT-BLOCK is a utf8 string of text. A sliding window is created
   on that text block, of default width 12. The words within the
   window are then sent to the LG parser, using the 'dict-pair'
   dictionary.  This dictionary is presumed to hold word-pairs
   with valid word-MI on them, accessible via the `BondNode ANY`
   EvaluationLinks.
   The LG parser creates MST/MPG parses using that dictionary.
   Then the count on each disjunct in the parse is incremented.
   This is a block observer, because, at this point, we do not yet know
   where the sentence boundaries might be, and so a sliding window is
   used to examine everything in the general vicinity.
"
	(make-observe-block disjunct-obs-text #:WIN-SIZE 12)
)