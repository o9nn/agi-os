(use-modules (ice-9 optargs))
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec))
(define*-public (make-disjunct-counter STOBJ BLOBJ DICT
	#:key
		(NUM-LINKAGES 3)
		(ATOMSPACE #t)
		(STORAGE #f)
	)
"
  make-disjunct-counter STOBJ BLOBJ DICT -- Parse text using DICT.
  Return a function that will parse text strings, and update Section
  counts (using STOBJ) and edge counts (using BLOBJ) for the parses.
  The LgDictNode DICT will be used to access the dictionary.
  The STOBJ should be a matrix object that holds Sections. Typically,
  the `make-pseudo-cset-api` object, or something similar, should be
  used.
  The BLOBJ should be a matrix object that holds Edge-Bond links.
  Typically, the `make-bond-link-api` object, or something similar,
  should be used.
  The DICT should be an `LgDictNode` specifying the dictionary.
  Recommend the one in `run-common/dict-combined`.
  This returns a function that takes a single argument, a plain-text
  UTF-8 string holding a single sentence, and sends it to the
  Link Grammar parser for parsing. The resulting parses are converted
  into Sections and given to the STOBJ for counting.
  This takes three optional parameters:
  #:NUM-LINKAGES -- The number of linkages that the LG parser should
  generate. Recall that each linkage is a different parse of the
  sentence
  #:ATOMSPACE -- Use the provided AtomSpace for dictionary contents.
  If not specified, use the current AtomSpace in the current thread.
  Set this to #f to use a local, private AtomSpace. In this case, the
  DICT must specify a StorageNode which can provide the dictionary
  contents.
  #:STORAGE -- If the current AtomSpace is being used, or a custom
  AtomSpace has been specified, then this parameter can be any
  StorageNode (including ProxyNodes) that will be used to access
  dictionary data. This can be used to specify a ProxyNode that
  computes MI or other statistics on-the-fly, for the accessed
  dictionary definitions.
  In addition to counting disjuncts, this will keep a running total
  of the numer of times this was called (aka 'the number of sentences')
  and the number of parses.
  Sentences are counted by updating the count on `(SentenceNode \"MST\")`.
  Parses are counted by updating the count on `(ParseNode \"MST\")`.
  XXX TODO Make above configurable.
"
	(define stol (if STORAGE (list STORAGE) '()))
	(define atml
		(if ATOMSPACE
			(cons
				(if (cog-atom? ATOMSPACE) ATOMSPACE (cog-atomspace))
				stol)
		'()))
	(define args (list DICT (Number NUM-LINKAGES) atml))
	(define mst-sent (SentenceNode "MST"))
	(define mst-parse (ParseNode "MST"))
(define mst-start (AnchorNode "MST Starts"))
(define mst-timeo (AnchorNode "MST Timeouts"))
(define mst-elaps (AnchorNode "MST Elapsed Time Secs"))
	(define (update-parse-counts PARSE)
		(define sect-bond (cog-value->list PARSE))
		(define sects (first sect-bond))
		(define bonds (second sect-bond))
		(count-one-atom mst-parse)
		(for-each
			(lambda (SECT) (STOBJ 'inc-count (cog-new-atom SECT) 1.0))
			(cog-value->list sects))
		(for-each
			(lambda (BOND) (BLOBJ 'inc-count BOND 1.0))
			(cog-value->list bonds)))
	(define (obs-txt PLAIN-TEXT)
(define start (current-time))
(define timeo #f)
		(define (pthunk)
			(define phrali (Phrase PLAIN-TEXT))
			(define parses (cog-value->list
				(cog-execute! (PureExec (LgParseSections phrali args)))))
			(count-one-atom mst-sent)
			(for-each update-parse-counts parses)
			(cog-extract-recursive! phrali)
		)
		(catch #t pthunk (lambda (key . args) (set! timeo #t)))
(count-one-atom mst-start)
(count-inc-atom mst-elaps (- (current-time) start))
(if timeo (count-one-atom mst-timeo))
		(monitor-parse-rate #f)
	)
	obs-txt
)
(define-public observe-mpg
	(make-disjunct-counter
		(add-storage-count (add-count-api (make-pseudo-cset-api)))
		(add-storage-count (add-count-api (make-bond-link-api)))
		(LgDictNode "dict-pair")))
(define-public (make-block-mpg-observer)
"
   make-block-mpg-observer -- Make an observer for counting MST/MPG
      disjuncts in text blocks.
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
	(define pca (make-pseudo-cset-api))
	(define pcc (add-count-api pca))
	(define pcs (add-storage-count pcc))
	(define bla (make-bond-link-api))
	(define blc (add-count-api bla))
	(define bls (add-storage-count blc))
	(define dict (LgDictNode "dict-pair"))
	(define obs-mpg (make-disjunct-counter pcs bls dict #:NUM-LINKAGES 3))
	(make-observe-block obs-mpg #:WIN-SIZE 12)
)