(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define*-public (rank-words LLOBJ
#:optional (WRD-LIST (LLOBJ 'left-basis))
)
"
rank-words LLOBJ -- Return a list of all words, ranked by count.
If counts are equal, then rank by support. This may take half-a-
minute to run.  Assumes that supports have been computed and are
available.
Here, a 'word' is any item appearing in the left-basis of LLOBJ.
Thus, it might include word-classes, not just words.
"
(define sup (add-support-api LLOBJ))
(define (nobs WRD) (sup 'right-count WRD))
(define (nsup WRD) (sup 'right-support WRD))
(sort WRD-LIST
(lambda (ATOM-A ATOM-B)
(define na (nobs ATOM-A))
(define nb (nobs ATOM-B))
(if (cog-equal? na nb)
(> (nsup ATOM-A) (nsup ATOM-B))
(> na nb))))
)
(define (recomp-all-sim SIM-API SIMMER WLIST)
"
recomp-all-sim SIM-API SIMMER WLIST - Recompute (all) similarities
for all words in WLIST. The SIMMER function will be called on all
word-pairs provided by the SIM-API.
For each pair of words in WLIST, determine if there is an existing
similarity between them, according to SIM-API. If so, then call the
SIMMER function on that pair. New pairings are not created.
"
(define e (make-elapsed-secs))
(define sms (add-pair-stars SIM-API))
(define (recomp-one WX LIGNORE)
(define todo-list (atoms-subtract (sms 'left-duals WX) LIGNORE))
(SIMMER WX WX)
(for-each (lambda (WRD)
(when (not (nil? (SIM-API 'get-pair WRD WX)))
(SIMMER WRD WX)))
todo-list))
(define (redo-list WX WLI WDONE)
(recomp-one WX (cons WX WDONE))
(when (not (nil? WLI))
(redo-list (car WLI) (cdr WLI) (cons WX WDONE))))
(define all-wrds (sms 'left-basis))
(define unaff (atoms-subtract all-wrds WLIST))
(define aff (atoms-subtract all-wrds unaff))
(format #t "Will recompute sims for ~3D words (~A unaffected) out of ~3D\n"
(length aff) (length unaff) (length all-wrds))
(if (not (nil? aff)) (redo-list (car aff) (cdr aff) '()))
(format #t "Recomputed sims for ~3D words out of ~3D in ~A secs\n"
(length aff) (length all-wrds) (e))
)