(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define-public (make-jaccard-selector LLOBJ
QUORUM COMMONALITY NOISE)
"
make-jaccard-selector LLOBJ QUORUM COMMONALITY NOISE
Return a function that selects the members of a group, by maximizing
the Jaccard similarity between all members of the group.
Example usage:
(define select-group
(make-jaccard-selector covr-obj 0.7 0.2 3))
(define initial-in-grp
(optimal-mi-in-group SIM-FUN WA WB WLIST))
(select-group initial-in-grp)
An initial group of candidate members, the `in-group`, is constructed
by using MI similarity to each of the two founding members WA, WB.
This in-group is selected by `optimal-in-group`, implemented
elsewhere.
The jaccard-slector begins with this candidate list, and trims it down
so that it meets the jaccard similarity criteria. This works as
follows:
The fraction of disjuncts that all group members have in common
is computed. If that shared fraction is greater than COMMONALITY, then
the selection process is done. Otherwise, a group member is ejected,
and the fraction is recomputed. If it is better, it is accepted
process is repeated until the either the fraction exceeds COMMONALITY
or the highest possible fraction has been found.
There are two ways of ejecting candidates: one is to remove the one
at the tail of the initial list. The other way is to loop over all
of the members, testing the ejection of each in turn. The second
variant is hard-coded. The first variant is stubbed out in the code.
"
(define (trim-tail-rec GRP prev-com prev-grp)
(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
(define comality (/ (car ovlp) (cadr ovlp)))
(format #t "Club size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
(length GRP) (car ovlp) (cadr ovlp) (* comality 100))
(cond
((< COMMONALITY comality) GRP)
((< comality prev-com) prev-grp)
((= (length GRP) 2) GRP)
(else (trim-tail-rec (drop-right GRP 1) comality GRP))))
(define (trim-tail GRP)
(trim-tail-rec GRP -1.0 GRP))
(define (mask LST IDX)
(append (take LST IDX) (drop LST (+ IDX 1))))
(define (trim-greedy-rec cmlty ovlp GRP)
(define glen (length GRP))
(if (or (= 2 glen) (<= COMMONALITY cmlty))
(append ovlp GRP)
(let ((best-cmlty cmlty)
(best-ovlp ovlp)
(best-grp GRP))
(any
(lambda (N)
(define grp (mask GRP N))
(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE grp))
(define cmlty (/ (first ovlp) (second ovlp)))
(when (<= best-cmlty cmlty)
(set! best-cmlty cmlty)
(set! best-ovlp ovlp)
(set! best-grp grp))
(< COMMONALITY cmlty))
(iota glen 0))
(if (not (equal? best-grp GRP))
(begin
(format #t "Better: size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
(length best-grp)
(first best-ovlp) (second best-ovlp)
(* 100 best-cmlty))
(trim-greedy-rec best-cmlty best-ovlp best-grp))
(append ovlp GRP)))))
(define (trim-greedy GRP)
(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
(define cmlty (/ (first ovlp) (second ovlp)))
(format #t "Start:  size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
(length GRP) (first ovlp) (second ovlp) (* 100 cmlty))
(define best (trim-greedy-rec cmlty ovlp GRP))
(define comality (/ (first best) (second best)))
(format #t "Best:   size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
(- (length best) 2) (first best) (second best) (* 100 comality))
(drop best 2))
(define (max-jaccard-grp initial-in-grp)
(trim-greedy initial-in-grp)
)
max-jaccard-grp
)
#! ========
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(define bat (batch-transpose sha))
(bat 'mmt-marginals)
(define sap (add-gram-mi-sim-api sha))
(define asm (add-symmetric-mi-compute sha))
==== !#