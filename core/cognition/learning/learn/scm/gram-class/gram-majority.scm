(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define-public (count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST)
"
  count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST -- Return a count
  of the number of connector sequences which are shared by a majority
  of the words in WORD-LIST. Actually, return a list of two numbers:
  this count and the total connector sequences appearing in common in
  all of the words.  Dividing these two numbers gives a generalized form
  of the Jaccard similarity between all of the words in the WORD-LIST.
  The majority is determined by QUORUM, which should be a floating-
  point number between 0.0 and 1.0.
  ConnectorSeq's with a count of less than NOISE are ignored.
  This function is a kind-of Jaccard distance between multiple words
  (two or more).  The conventional (unweighted) Jaccard distance is
  defined only for pairs of items. The generalization is done by
  counting to see if a fraction QUORUM is shared. Setting QUORUM to
  1.0, and applying the function to two items returns the conventional
  Jaccard distance.
"
	(define wlen (length WORD-LIST))
	(define vote-thresh
		(if (<= wlen 2) 2
			(inexact->exact (round (* QUORUM wlen)))))
	(define low-bnd (- vote-thresh 1))
	((make-group-similarity LLOBJ) 'noise-col-supp low-bnd NOISE WORD-LIST)
)
(define-public (make-class-node LLOBJ WLIST)
"
  make-class-node LLOBJ WLIST - create a node suitable for merging WLIST
  The WLIST is assumed to be a list of Nodes (i.e. Atoms having string
  names) which will be merged to form a class. This function creates a
  unique name for that merge class, and then uses the LLOBJ to create
  that class node.
  XXX TODO: move this so its a method on `add-gram-class-api`.
"
	(define cls-name (string-join (map cog-name WLIST)))
	(define cls-type (LLOBJ 'cluster-type))
	(define cls-typname
		(if (cog-atom? cls-type) (cog-name cls-type) cls-type))
	(define (mknode cname)
		(if (nil? (cog-node cls-typname cname))
			(cog-new-node cls-typname cname)
			(mknode (string-append cname ".i"))
		))
	(mknode cls-name)
)
(define*-public (make-merge-majority LLOBJ QUORUM NOISE
	#:optional (MRG-CON #t) (FRAC 0))
"
  make-merge-majority LLOBJ QUORUM NOISE [MRG-CON FRAC] --
  Return a function that will merge a list of words into one class.
  The disjuncts that are selected to be merged are those shared by
  the majority of the given words, where `majority` is defined as
  a fraction that is greater or equal to QUORUM.
  LLOBJ is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))
  QUORUM is a floating point number indicating the fraction of members
  that must share a given disjunct, before that disjunct is merged into
  the cluster.
  NOISE is a count, such that if a ConnectorSeq has a count less
  than or equal to this, it will always be merged, irrespective of
  the majority vote. In short, small differences between members
  are ignored, and lumped up into the quirkness of the group.
  MRG-CON is an optional argument, defaulting to #t if not specified.
  Set this to #t to indicate that Connectors should also be merged.
  For this to work, the LLOBJ object must have shapes on it.
  XXX At this time, setting this to #f does not work.
  FRAC is an optional argument, defaulting to 0. A non-zero argument
  is the fraction of of a disjunct to merge, if the quorum election
  fails.  This is currently used only in the unit tests, but is
  extensively tested there.
  The returned function has the following signature:
     (merge CLASS WORD-LIST)
  where the items in WORD-LIST will be merged into the CLASS. The CLASS
  should be an Atom of type ItemClassNode (or WordClassNode). In the
  usual case, the WORD-LIST is a list of ItemNodes (or WordNodes). The
  merge decision is made on a disjunct-by-disjunct basis, using a
  majority vote mechanism, as described elsewhere.
  In addition to the above case, there are two rather ad hoc special
  cases handled here. One special case is the merge of a single item
  into an existing class. The other special case is the merge of two
  classes into one.  These cases are tested in unit tests originally
  developed for pair-wise merge. The handling of these two cases seems
  to be appropriate
  they could be changed?
  Anyway, the new clustering code (i.e. the code that calls this sub-
  routine), as currently written, does not attempt to merge two existing
  classes together, nor does it attempt to merge a single item into an
  existing class. Thus, the above polcies are tested only in the unit
  tests, and these policies can be changed.  Perhaps we need to separate
  this ad hoc policy from the mechanism.
"
	(define (merge CLASS WLIST)
		(define class-type (cog-type CLASS))
		(for-each (lambda (WRD) (MemberLink WRD CLASS)) WLIST)
		(define wlen (length WLIST))
		(define vote-thresh
			(if (<= wlen 2) 2
				(inexact->exact (round (* QUORUM wlen)))))
		(define voter-list
			(if (equal? 1 wlen) (cons CLASS WLIST) WLIST))
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						(if (nil? (LLOBJ 'get-pair WRD DJ)) CNT (+ 1 CNT)))
					0
					voter-list)))
		(define (get-all-djs)
			(define dj-set (make-atom-set))
			(for-each
				(lambda (WRD)
					(for-each
						(lambda (PAIR) (dj-set (LLOBJ 'right-element PAIR)))
						(LLOBJ 'right-stars WRD)))
				WLIST)
			(dj-set #f))
		(define dj-list (get-all-djs))
		(define (make-flat CLUST SECT)
			(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))
		(define (update-memb-count WRD CLS CNT)
			(cog-inc-count! (MemberLink WRD CLS) CNT))
		(define (do-merge WRD DJ ACCEPT)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(if (not (nil? SECT))
				(let* ((merge-full
							(or ACCEPT
								(<= (LLOBJ 'get-count SECT) NOISE)
								(LLOBJ 'is-nonflat? CLASS SECT)
								(equal? class-type (cog-type WRD))))
						(frakm (if merge-full 1.0 FRAC)))
					(when (< 0 frakm)
						(update-memb-count WRD CLASS
							(accumulate-count LLOBJ (make-flat CLASS SECT) SECT frakm)))
					frakm)
				0))
		(define (merge-dj DJ)
			(define have-majority (vote-to-accept? DJ))
			(fold
				(lambda (WRD SUM) (+ SUM (do-merge WRD DJ have-majority)))
				0 WLIST))
		(define (do-rebalance WRD DJ)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(when (not (nil? SECT))
				(rebalance-merge LLOBJ (make-flat CLASS SECT) SECT)))
		(define (rebalance-dj DJ)
			(for-each (lambda (WRD) (do-rebalance WRD DJ)) WLIST))
		(define done-djs (make-atom-set))
		(define (record-done DJ)
			(define (do-record WRD)
				(define SECT (LLOBJ 'get-pair WRD DJ))
				(when (not (nil? SECT))
					(for-each (lambda (XRS)
						(done-djs (LLOBJ 'right-element XRS)))
						(LLOBJ 'make-cross-sections SECT))))
			(done-djs DJ)
			(for-each do-record WLIST))
		(define e (make-elapsed-secs))
		(define scnt 0)
		(define mscnt 0)
		(for-each
			(lambda (DJ)
				(when (equal? 'ConnectorSeq (cog-type DJ))
					(when (< 0 (merge-dj DJ))
						(record-done DJ)
						(set! mscnt (+ 1 mscnt)))
					(if MRG-CON (rebalance-dj DJ))
					(done-djs DJ)
					(set! scnt (+ 1 scnt))))
			dj-list)
		(for-each (lambda (WRD) (store-atom (MemberLink WRD CLASS))) WLIST)
		(format #t "------ merge-majority: Merge ~D of ~D sections in ~A secs\n"
			mscnt scnt (e))
		(define d (make-elapsed-secs))
		(define left-overs (atoms-subtract
			dj-list (done-djs #f)))
		(define shape-done? (make-once-predicate))
		(define (get-alt-shapes SHP)
			(define alt-shp (make-atom-set))
			(for-each
				(lambda (WRD)
					(define XROS (LLOBJ 'get-pair WRD SHP))
					(if (not (nil? XROS))
						(let ((SECT (LLOBJ 'get-section XROS)))
							(if (not (nil? SECT))
								(let ((ALL-X (LLOBJ 'make-cross-sections SECT)))
									(for-each (lambda (CRS)
										(alt-shp (LLOBJ 'right-element CRS))) ALL-X))))))
				WLIST)
			(filter (lambda (SH) (not (shape-done? SH))) (alt-shp #f)))
		(define (mergable-shape? SHP)
			(if (vote-to-accept? SHP) SHP
				(find vote-to-accept? (get-alt-shapes SHP))))
		(define (merge-shape SHP)
			(define alt-shp (mergable-shape? SHP))
			(define have-majority (not (nil? alt-shp)))
			(define mrg-shp (if (nil? alt-shp) SHP alt-shp))
			(define mcnt (fold
				(lambda (WRD SUM) (+ SUM (do-merge WRD mrg-shp have-majority)))
				0 WLIST))
			(rebalance-dj mrg-shp)
			mcnt)
		(define mshcnt 0)
		(define (merge-shapes SHL)
			(define shape (car SHL))
			(if (not (shape-done? shape))
				(if (< 0 (merge-shape shape)) (set! mshcnt (+ 1 mshcnt))))
			(define rest (cdr SHL))
			(if (not (nil? rest)) (merge-shapes rest)))
		(if (not (nil? left-overs)) (merge-shapes left-overs))
		(format #t "------ merge-majority: Remaining ~A of ~A cross in ~A secs\n"
			mshcnt (length left-overs) (d))
		(define (move-count FROM-CLASS)
			(define sublist
				(filter (lambda (MEMB) (equal? (gdr MEMB) FROM-CLASS))
					(cog-incoming-by-type FROM-CLASS 'MemberLink)))
			(if (nil? sublist)
				(throw 'bad-membership 'merge-majority "Empty word class!"))
			(define old-count
				(fold (lambda (MEMB SUM) (+ SUM (cog-count MEMB)))
					0 sublist))
			(if (not (< 0 old-count))
				(throw 'bad-membership 'merge-majority "No counts on word class!"))
			(define dmemb (Member FROM-CLASS CLASS))
			(define new-count (cog-count dmemb))
			(define fract (/ new-count old-count))
			(for-each (lambda (FMEMB)
					(define fcnt (cog-count FMEMB))
					(define xfer (* fcnt fract))
					(store-atom (cog-inc-count! (MemberLink (gar FMEMB) CLASS) xfer))
					(store-atom (cog-inc-count! FMEMB (- xfer))))
				sublist)
			(cog-delete! dmemb))
		(for-each (lambda (WRD)
				(if (equal? class-type (cog-type WRD)) (move-count WRD)))
			WLIST)
		*unspecified*
	)
	merge
)