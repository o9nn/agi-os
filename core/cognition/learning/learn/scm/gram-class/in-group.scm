(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(define-public (find-in-group SIMFUN WA WB
                              LOWER-BOUND EPSILON TIGHTNESS CANDIDATES)
"
  find-in-group SIMFUN WA WB LOWER-BOUND EPSILON TIGHTNESS CANDIDATES
  Return an in-group of closely related words.
  Given two words WA and WB with a high similarity score, find a clique
  an almost-clique (the in-group), such that all similarity scores in
  that in-group are greater than LOWER-BOUND and are also no less than
  EPSILON below the similarity score of the initial pair.  A clique is
  formed if *all* pair-scores meet this requirement. An in-group is
  formed, if more than a TIGHTNESS fraction of the scores to other
  members in the in-group are above the epsilon threshold. (A TIGHTNESS
  of 0.5 means that a simple majority of the in-group meets the
  requirement
  Arguments:
  WA and WB seed the initial in-group.
  SIMFUN is an function that, given two items, returns a similarity
  score for those items.  Similarities are assumed to be symmetric,
  that is, (SIMFUN a b) == (SIMFUN b a). Usually, the similarity is
  a floating point number, but in fact it can be anything that is
  comparable with greater-than.
  At this time, all experimental results (and thus, recommended
  parameter values) have been done ONLY with ranked-MI. The code
  should still work for other SIMFUN's, but these have not been
  characterized.
  LOWER-BOUND is an absolute lower bound on the in-group similarities.
  All members of the in-group must have similarities that are greater
  than LOWER-BOUND.  Recommended value of 0.0 to 4.0.
  EPSILON is a relative lower bound on the in-group similarities. Most
  members of the in-group must have similarities that are within EPSILON
  of the initial pair.  Pairs that are within EPSILON are termed
  `similar enough`.  Recommended value of 0.5 to 8.0.
  TIGHTNESS is a number between 0 and 1, specifying the fraction of
  the in-group pairs that must be similar enough to one-another. A
  TIGHTNESS of 0.5 means that a majority of the pair-relations must
  be `similar enough`, while a TIGHTNESS of 1.0 means that all of
  them will be. Recommended value of 0.7. Experiments reveal that
  results are relatively insensitive to this value, ranging over 0.3
  to 1.0.
  CANDIDATES is a list of individuals to consider adding to the group.
  Experiments show that the size of the group at first grows slowly as
  a function of increasing EPSILON, followed by a very rapid increase
  after some threshold is passed. Obviously, EPSILON should be set
  below that threshold. Unfortunately, this threshold depends strongly
  on the initial pair, even when working within the same dataset.
"
	(define (accept INGRP CANDIDATE MINSCORE TIGHT)
		(define maxfail (- (length INGRP) TIGHT))
		(define failcnt 0)
		(every
			(lambda (MEMB)
				(define score (SIMFUN CANDIDATE MEMB))
				(if (< score LOWER-BOUND) (set! failcnt (+ failcnt maxfail 999)))
				(if (< score MINSCORE) (set! failcnt (+ failcnt 1)))
				(<= failcnt maxfail)
			)
			INGRP))
	(define benchmark (SIMFUN WA WB))
	(define minscore (- benchmark EPSILON))
	(define (get-tight INGRP)
		(define insz (length INGRP))
		(if (equal? 2 insz) 2
			(inexact->exact (round (* TIGHTNESS insz)))))
	(define clean-cand
		(filter (lambda (cand)
			(not (or (equal? cand WA) (equal? cand WB))))
		CANDIDATES))
	(fold
		(lambda (CAND INGRP)
			(if (accept INGRP CAND minscore (get-tight INGRP))
				(cons CAND INGRP)
				INGRP))
		(list WA WB)
		clean-cand)
)
(define*-public (optimal-in-group SIMFUN WA WB CANDIDATES
	#:key
		(tightness 0.7)
		(epsi-step 0.1)
		(max-epsi 8.5)
		(lower-bound 1.0)
		(max-jump 2.5)
		(win-size 1.0)
		(max-size 12)
	)
"
  optimal-in-group SIMFUN WA WB CANDIDATES
  Return an ingroup of closely related words. The initial members of the
  ingroup are WA and WB. Additional potential members are drawn from
  CANDIDATES if they are similar-enough to the current ingroup, as
  measured by the similarity function SIMFUN.
  This searches for the largest ingroup that is still exclusive. The
  search is performed by admitting individuals from CANDIDATES, one
  at a time, if they are judged similar-enough by SIMFUN. The membership
  requirements are slowly loosened (by dropping the lower bound of what
  is considered 'similar-enough'), until membership explodes. Then
  the lower bound is backed off a bit, just before the explosion.
  Experiments show that as membership requirements are loosened, there
  is a knee in the size of the group: the group size suddenly explodes.
  That is, as the similarity threshold is loosened, the size of the
  group grows slowly at first, and then, at some point, it takes off,
  growing rapidly (growing 'explosively'). This searches for the
  largest group below that inflection point.
  Arguments:
  WA and WB seed the initial in-group.
  SIMFUN is an function that, given two items, returns a similarity
  score for those items.  Similarities are assumed to be symmetric,
  that is, (SIMFUN a b) == (SIMFUN b a). Usually, the similarity is
  a floating point number, but in fact it can be anything that is
  comparable with greater-than. For similarities that are floating point
  numbers, the larger the value, the more similar they are.
  This function has been experimentally tested only for SIMFUN being
  ranked-MI!
  CANDIDATES is a list of individuals to consider adding to the group.
"
	(define epsilon #f)
	(define nsteps (inexact->exact (round (/ max-epsi epsi-step))))
	(define win-slots (inexact->exact (round (/ win-size epsi-step))))
	(define window (make-list win-slots 2))
	(take-while
		(lambda (N)
			(set! epsilon (* N epsi-step))
			(define ing (find-in-group SIMFUN WB WA
				lower-bound epsilon tightness CANDIDATES))
			(define ingsz (length ing))
			(define prevsz (car window))
			(set! window (append (drop window 1) (list ingsz)))
			(define jump (- ingsz prevsz))
			(and (< jump max-jump) (< ingsz max-size))
		)
		(iota nsteps 1))
	(define in-grp
		(find-in-group SIMFUN WA WB
			lower-bound (- epsilon epsi-step) tightness CANDIDATES))
	(reverse in-grp)
)
(define-public (optimal-mi-in-group SIMFUN WA WB CANDIDATES)
"
  optimal-mi-in-group - version of optimal-in-group with parameters
  that work for grammatical-MI similarity. See `optimal-in-group`
  for documentation.
"
	(optimal-in-group SIMFUN WA WB CANDIDATES
		#:tightness 0.7
		#:epsi-step 0.1
		#:max-jump 2.5
		#:win-size 1.0
		#:max-epsi 8.5
		#:lower-bound 1.0
		#:max-size 12
	))
#! ===========
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(define sap (add-similarity-api sha #f "shape-mi"))
(sap 'fetch-pairs)
(define sim (add-pair-stars sap))
(define e (make-elapsed-secs))
(define ranked-words (rank-words pcs))
(e)
(define words-with-sims (take ranked-words 1200))
(define e (make-elapsed-secs))
(define all-sim-pairs (sim 'get-all-elts))
(e)
(length all-sim-pairs)
(define uniq-sims
	(filter (lambda (SIM) (not (equal? (gar SIM) (gdr SIM)))) all-sim-pairs))
(length uniq-sims)
(define (ranked-mi-sim WA WB)
	(define miv (sap 'pair-count WA WB))
	(if miv (cog-value-ref miv 1) -inf.0))
(define e (make-elapsed-secs))
(define hi-comi-sims
	(filter (lambda (SIM) (< 6.0 (ranked-MI (gar SIM) (gdr SIM))))
		 uniq-sims))
(e)
(length hi-comi-sims)
(define (rank-pairs FUN)
	(sort hi-comi-sims
		(lambda (ATOM-A ATOM-B)
			(> (FUN ATOM-A) (FUN ATOM-B))))
)
(define sorted-pairs
	(rank-pairs (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM)))))
(take sorted-pairs 10)
(define (prt-sorted-pairs N)
	(for-each
		(lambda (PR)
			(format #t "common-MI= ~6F ~A <<-->> ~A\n"
				(common-MI (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(drop (take sorted-pairs (+ N 20)) N)))
(prt-sorted-pairs 0)
(define in-group (find-in-group ranked-mi-sim (Word "is") (Word "was")
	0.5  0.7 (take words-with-sims 10)))
(define (rank-of-word WRD)
	(list-index (lambda (RW) (equal? WRD RW)) words-with-sims))
(define (in-group-csv FILENAME WA WB TIGHT)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT O_TRUNC)))
	(format csv "#\n# Initial 2-clique: ~A <<>> ~A\n#\n"
		(cog-name WA) (cog-name WB))
	(format csv "# Tightness = ~6F\n" TIGHT)
	(format csv "# This is using common-MI to determine in-group membership.\n")
	(format csv "#\n# idx\tepsilon\tsize\tmin-index\tmax-index\twords\n")
	(for-each
		(lambda (N)
			(define epsi (+ (* 0.1 N) -2))
			(define in-group (find-in-group common-MI
				WA WB
				epsi TIGHT words-with-sims))
			(define max-idx
				(fold (lambda (W MAXI) (max MAXI (rank-of-word W))) -1000 in-group))
			(define min-idx
				(fold (lambda (W MINI) (min MINI (rank-of-word W))) 1000 in-group))
			(format csv "~D\t~6F\t~D\t~D\t~D\t{ "
				N epsi (length in-group) min-idx max-idx)
			(for-each (lambda (WRD)
				(format csv "~A " (cog-name WRD))) in-group)
			(format csv "}\n")
			(force-output csv))
		(iota 100))
	(close csv))
(in-group-csv "/tmp/grp-is-was.dat" (Word "is") (Word "was") 0.7)
(in-group-csv "/tmp/grp-and-but.dat" (Word "and") (Word "but") 0.7)
(in-group-csv "/tmp/grp-in-of.dat" (Word "in") (Word "of") 0.7)
(in-group-csv "/tmp/grp-she-he.dat" (Word "she") (Word "he") 0.7)
(in-group-csv "/tmp/grp-comma-semi.dat" (Word ",") (Word ";") 0.7)
(in-group-csv "/tmp/grp-period-quest.dat" (Word ".") (Word "?") 0.7)
(in-group-csv "/tmp/grp-plus-minus.dat" (Word "+") (Word "—") 0.7)
(in-group-csv "/tmp/grp-roman-i-ii.dat" (Word "i") (Word "ii") 0.7)
(in-group-csv "/tmp/grp-It-There.dat" (Word "It") (Word "There") 0.7)
(in-group-csv "/tmp/grp-spoke-sat.dat" (Word "spoke") (Word "sat") 0.7)
(in-group-csv "/tmp/grp-look-smile.dat" (Word "look") (Word "smile") 0.7)
(in-group-csv "/tmp/grp-town-earth.dat" (Word "town") (Word "earth") 0.7)
(in-group-csv "/tmp/grp-door-house.dat" (Word "door") (Word "house") 0.7)
========== !#