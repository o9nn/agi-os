(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(define-public (add-singleton-classes LLOBJ)
"
  add-singleton-classes LLOBJ -- manage singleton WordClassNodes
  After clustering, there will still be many WordNodes that have not
  been assigned to clusters. This may be the case for three reasons:
   * The Wordnode really is in a class of it's own.
   * The clustering algo has not gotten around to it yet.
   * It is rare, infrequently-observed junk.
  This object provides management callbacks to place some words into
  singleton WordClassNodes, based on their count or relative rank.
  Note that using this object will cause the MI values between
  word-classes and disjuncts to become invalid
  then they will need to be recomputed.
  Provided methods:
     'delete-singles -- Remove all WordClassNodes that have only a
           single member.
     'create-singles LIST -- Create a WordClassNode for each WordNode
           in the LIST. The complete set of Sections will be copied
           from the WordNode to the WordClassNode
           Section will be copied as well, so that the Sections have
           correct counts on them.
     'create-hi-count-singles MIN-CUTOFF -- Create a WordClassNode for
           each WordNode whose observation count is greater than
           MIN-CUTOFF. As above, Sections and values are copied.
     'create-top-rank-singles NUM -- Create a WordClassNode for the
           top-ranked NUM WordNode's having the highest observation
           counts.  As above, Sections and values are copied.
  Known bugs:
  * delete-singles is broken
"
	(if (not (LLOBJ 'provides 'flatten))
		(throw 'missing-method 'add-singleton-classes
			"The 'flatten method is needed to create singletons\nUse `add-covering-sections` to get it."))
	(define (delete-singles)
		(throw 'not-implemented 'add-singleton-classes
			"This method is borken and don't work right!")
		(for-each cog-delete-recursive!
			(filter
				(lambda (WRDCLS)
					(eq? 1 (cog-incoming-size-by-type WRDCLS 'MemberLink)))
				(LLOBJ 'left-basis))))
	(define pss (add-support-api LLOBJ))
	(define (create-singles WORD-LIST)
		(define (copy-values NEW OLD)
			(for-each
				(lambda (KEY)
					(cog-set-value! NEW KEY (cog-value OLD KEY)))
				(cog-keys OLD)))
		(define (flatten WCL PNT)
			(define flat (LLOBJ 'flatten WCL PNT))
			(define cnt (LLOBJ 'get-count PNT))
			(if (eq? 'Section (cog-type PNT))
				(begin
					(copy-values flat PNT)
					(store-atom flat)
					(for-each
						(lambda (XES) (LLOBJ 'set-count XES cnt))
						(LLOBJ 'make-cross-sections flat))
					(for-each cog-delete! (LLOBJ 'get-cross-sections PNT))
					(cog-delete! PNT)
				)
				(let ((SEC (LLOBJ 'make-section flat))
						(OSC (LLOBJ 'get-section PNT)))
					(LLOBJ 'set-count SEC cnt)
					(store-atom SEC)
					(for-each
						(lambda (XES) (LLOBJ 'set-count XES cnt))
						(LLOBJ 'make-cross-sections SEC))
					(if (cog-atom? OSC) (cog-delete! OSC))
				))
		)
		(define start-time (current-time))
		(for-each
			(lambda (WRD)
				(define wcl (WordClass (string-append (cog-name WRD) "#uni")))
				(define memb (MemberLink WRD wcl))
				(cog-inc-count! memb (pss 'right-count WRD))
				(store-atom memb)
				(for-each
					(lambda (PNT) (if (cog-atom? PNT) (flatten wcl PNT)))
					(LLOBJ 'right-stars WRD)))
			WORD-LIST)
		(format #t "Created ~A singleton word classes in ~A secs\n"
			(length WORD-LIST) (- (current-time) start-time))
		(LLOBJ 'clobber)
	)
	(define (trim-low-counts MIN-CNT)
		(define trimmed-words
			(remove (lambda (WRD)
				(or
					(equal? 'WordClassNode (cog-type WRD))
					(< (pss 'right-count WRD) MIN-CNT)))
				(LLOBJ 'left-basis)))
		(format #t "After trimming, ~A words left, out of ~A\n"
			(length trimmed-words) (LLOBJ 'left-basis-size))
		(create-singles trimmed-words)
	)
	(define (top-ranked NUM-TOP)
		(define (nobs WRD) (pss 'right-count WRD))
		(define words-only
			(remove
				(lambda (WRD) (equal? 'WordClassNode (cog-type WRD)))
				(LLOBJ 'left-basis)))
		(define ranked-words
			(sort! words-only
				(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))
		(define short-list (take ranked-words NUM-TOP))
		(format #t "After sorting, kept ~A words out of ~A\n"
			(length short-list) (LLOBJ 'left-basis-size))
		(create-singles short-list)
	)
	(lambda (message . args)
		(case message
			((delete-singles) (delete-singles))
			((create-singles) (apply create-singles args))
			((create-hi-count-singles) (apply trim-low-counts args))
			((create-top-rank-singles) (apply top-ranked args))
			(else             (apply LLOBJ (cons message args)))
		))
)