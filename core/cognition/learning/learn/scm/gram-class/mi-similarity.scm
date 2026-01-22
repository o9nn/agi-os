(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define (add-gram-mi-sim-api LLOBJ)
"
	add-gram-mi-sim-api LLOBJ -- Return the correct similarity API.
"
	(define SIM-ID "shape-mi")
	(add-similarity-api LLOBJ #f SIM-ID))
(define (make-gram-mi-simmer LLOBJ)
"
  make-gram-mi-simmer LLOBJ -- return function that computes and stores
  grammatical-MI's between words.
  This computes and stores both the grammatical-MI and the Ranked-MI scores.
  The computation is performed unconditionally
  even if there is an existing one cached.
"
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define smi (add-symmetric-mi-compute LLOBJ))
	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (if (< 0 x) (* (log x) ol2) -inf.0))
	(define mmt-q (smi 'mmt-q))
	(define (compute-sim WA WB)
		(define fmi (smi 'mmt-fmi WA WB))
		(define mwa (smi 'mmt-marginal WA))
		(define mwb (smi 'mmt-marginal WB))
		(define rmi (+ fmi (* 0.5 (log2 (* mwa mwb))) mmt-q))
		(if (< 6 fmi)
			(format #t "\tMI(`~A`, `~A`) = ~6F  rank-MI = ~6F\n"
				(cog-name WA) (cog-name WB) fmi rmi))
		(store-atom
			(sap 'set-pair-similarity
				(sap 'make-pair WA WB)
				(FloatValue fmi rmi))))
	compute-sim
)
(define (get-mi-ranked-pairs LLOBJ MI-CUTOFF)
"
  get-mi-ranked-pairs LLOBJ MI-CUTOFF - get a ranked list of word pairs
  This returns a list of word-pairs sorted by rank-MI, from greatest
  to least.  All pairs in the list will have an MI of greater than
  MI-CUTOFF.  An MI-CUTOFF of 4 is recommended, maybe down to 2.
  Setting this too low will provide poor merge suggestions, in addition
  to making it take more time (because setting it low will admit more
  pairs, which take more time to sort.)
"
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))
	(define all-sim-pairs (cog-get-atoms 'SimilarityLink))
	(define good-sims
		(filter
			(lambda (SIM)
				(define WA (gar SIM))
				(define WB (gdr SIM))
				(and (< MI-CUTOFF (mi-sim WA WB)) (not (equal? WA WB))))
			all-sim-pairs))
	(define (rank-pairs PRLI FUN)
		(sort PRLI
			(lambda (ATOM-A ATOM-B)
				(> (FUN ATOM-A) (FUN ATOM-B)))))
	(rank-pairs good-sims (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM))))
)
(define (prt-mi-sorted-pairs LLOBJ LST START N)
"
  prt-mi-sorted-pairs LLOBJ PAIR-LST START NUM - print list of word pairs
  and thier grammatical-MI and ranked-MI similarities.
  Handy-dandy debug utility. Starting at START positions deep into the
  PAIR-LST, print the next NUM similarities.
"
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))
	(define len (length LST))
	(define start (min START len))
	(define num (min N (max 0 (- len START))))
	(for-each
		(lambda (PR)
			(format #t "ranked-MI = ~6F MI = ~6F (`~A`, `~A`)\n"
				(ranked-mi-sim (gar PR) (gdr PR))
				(mi-sim (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(take (drop LST start) num))
)