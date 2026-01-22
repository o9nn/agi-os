(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(define-public monitor-parse-rate (make-rate-monitor))
(set-procedure-property! monitor-parse-rate 'documentation
"
   monitor-parse-rate MSG - monitor the parse rate.
   Call this function with a string MSG to print out the current
   parse rate
")
(define*-public (make-pair-counter LLOBJ
	#:key
		(NUM-LINKAGES 24)
		(DICT (LgDict "any"))
	)
"
  make-pair-counter LLOBJ --
     Return a function that will update word-pair counts on LLOBJ.
  DEPRECATED: This works and is the formally correct solution
  the (still experimental) code `make-block-pipe-observer` is 3x faster.
  The LLOBJ should be a matrix object that can hold a pair of words
  on the left and right. The `any-link-api` object will do.
  This returns a function that takes a single argument, a plain-text
  UTF-8 string holding a single sentence, and sends it to the
  Link Grammar parser for parsing. The individual links in the
  resulting parses are sent to the LLOBJ for pair-counting.
  This takes two optional parameters:
  #:NUM-LINKAGES -- the number of linkages that the LG parser should
  generate. Recall that each linkage is a different parse of the
  sentence
  #:DICT -- the `LgDictNode` to use. This is the dictionary to use for
  parsing. By default, this is the `any` dictionary, which creates
  uniformly-distributed random parse trees.
  The parse rate can be monitored by calling, by hand, the guile function
   `(monitor-parse-rate MSG)` for some string MSG.
"
	(define NUML (Number NUM-LINKAGES))
	(define wild-wild (LLOBJ 'wild-wild))
	(define any-sent (SentenceNode "ANY"))
	(define any-parse (ParseNode "ANY"))
	(define (update-word-counts WRD-LIST)
		(for-each (lambda (WRD) (count-one-atom (cog-new-atom WRD)))
			(cog-value->list WRD-LIST)))
	(define is-any-obj (equal? (LLOBJ 'id) "ANY"))
	(define (incr-pair EDGE)
		(define w-left  (gadr EDGE))
		(define w-right (gddr EDGE))
		(LLOBJ 'pair-inc w-left w-right 1.0))
	(define (inc-count EDGE)
		(LLOBJ 'inc-count (cog-new-atom EDGE) 1.0))
	(define (update-pair-counts PAIR-LIST)
		(if is-any-obj
			(for-each inc-count (cog-value->list PAIR-LIST))
			(for-each incr-pair (cog-value->list PAIR-LIST))))
	(define (obs-txt PLAIN-TEXT)
		(define phrali (Phrase PLAIN-TEXT))
		(define parses (cog-execute! (PureExecLink
			(LgParseBonds phrali DICT NUML))))
		(count-one-atom any-sent)
		(for-each
			(lambda (PARSE)
				(count-one-atom any-parse)
				(update-word-counts (cog-value-ref PARSE 0))
				(update-pair-counts (cog-value-ref PARSE 1)))
			(cog-value->list parses))
		(cog-extract-recursive! phrali)
		(monitor-parse-rate #f)
	)
	obs-txt
)
(define-public observe-text
	(make-pair-counter
		(add-storage-count (add-count-api (make-any-link-api)))))
(define-public (make-block-pair-observer)
"
   make-block-pair-observer -- Make an observer for counting pairs in
   text blocks. Returns a function of the following form:
   func TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for word-pair counting.
   TEXT-BLOCK is a utf8 string of text. A sliding window, of the default
   width of 9 words, is created on that block. Everything within the
   window is sent to the LG 'any' random-planar-tree parser. The word
   pairs in the random tree are then counted. Counts are stored.
"
	(define ala (make-any-link-api))
	(define alc (add-count-api ala))
	(define als (add-storage-count alc))
	(define obs-text (make-pair-counter als #:NUM-LINKAGES 6))
	(make-observe-block obs-text #:WIN-SIZE 9)
)