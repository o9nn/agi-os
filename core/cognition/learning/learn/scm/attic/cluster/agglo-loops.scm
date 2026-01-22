(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define (assign-word-to-class MERGER WRD CLS-LST)
(define (merge-pred cls) (MERGER 'merge-predicate cls WRD))
(let ((cls (find merge-pred CLS-LST))
)
(if (not cls)
WRD
(MERGER 'merge-function cls WRD)))
)
(define (assign-expand-class MERGER WRD-OR-CLS WRD-LST)
(if (null? WRD-LST) WRD-OR-CLS
(let ((wrd (car WRD-LST))
(rest (cdr WRD-LST)))
(if (MERGER 'merge-predicate WRD-OR-CLS wrd)
(assign-expand-class MERGER
(MERGER 'merge-function WRD-OR-CLS wrd) rest)
(assign-expand-class MERGER WRD-OR-CLS rest))))
)
(define (sort-class-list CLS-LST)
(define (nwords-in-cls CLS)
(fold
(lambda (MEMB sum)
(if (eq? (cog-type (gar MEMB)) 'WordNode) (+ sum 1) sum))
0
(cog-incoming-by-type CLS 'MemberLink)))
(sort! CLS-LST
(lambda (ATOM-A ATOM-B)
(> (nwords-in-cls ATOM-A) (nwords-in-cls ATOM-B))))
)
(define (is-in-cls? WRD CLS)
(not (null? (cog-link 'MemberLink WRD CLS))))
(define (got-done WRDS CLS)
(filter (lambda (w) (is-in-cls? w CLS)) WRDS))
(define (still-to-do WRDS CLS)
(remove (lambda (w) (is-in-cls? w CLS)) WRDS))
(define (block-assign-to-classes MERGER WRD-LST CLS-LST)
(format #t "-------  Words remaining=~A Classes=~A ~A ------\n"
(length WRD-LST) (length CLS-LST)
(strftime "%c" (localtime (current-time))))
(if (null? WRD-LST) CLS-LST
(let* ((wrd (car WRD-LST))
(rest (cdr WRD-LST))
(cls (assign-word-to-class MERGER wrd CLS-LST)))
(if (eq? 'WordClassNode (cog-type cls))
(block-assign-to-classes MERGER rest CLS-LST)
(let* ((new-cls (assign-expand-class MERGER wrd rest))
(new-lst
(if (eq? 'WordClassNode (cog-type new-cls))
(append! CLS-LST (list new-cls))
CLS-LST)))
(block-assign-to-classes MERGER rest new-lst)))))
)
(define (assign-to-classes MERGER TRUE-CLS-LST FAKE-CLS-LST WRD-LST)
(format #t "--- To-do=~A num-classes=~A num-done=~A ~A ---\n"
(length WRD-LST) (length TRUE-CLS-LST) (length FAKE-CLS-LST)
(strftime "%c" (localtime (current-time))))
(if (null? WRD-LST) TRUE-CLS-LST
(let* ((wrd (car WRD-LST))
(rest (cdr WRD-LST))
(cls (assign-word-to-class MERGER wrd TRUE-CLS-LST)))
(if (eq? 'WordClassNode (cog-type cls))
(assign-to-classes MERGER TRUE-CLS-LST FAKE-CLS-LST rest)
(let* ((new-cls (assign-word-to-class MERGER wrd FAKE-CLS-LST))
(is-new-cls (eq? 'WordClassNode (cog-type new-cls)))
(new-true
(if is-new-cls
(sort-class-list (cons new-cls TRUE-CLS-LST))
TRUE-CLS-LST))
(new-fake
(if is-new-cls
(still-to-do FAKE-CLS-LST new-cls)
(append! FAKE-CLS-LST (list new-cls)))))
(assign-to-classes MERGER new-true new-fake rest)))))
)
(define *-greedy-anchor-* (AnchorNode "*-greedy-singleton-words-*"))
(define (greedy-grow MERGER TRUE-CLS-LST FAKE-CLS-LST DONE-LST WRD-LST)
(define min-greedy 200)
(define scan-multiplier 4)
(define supp-obj (add-support-compute MERGER))
(define (num-classified-words)
(define (nmemb CLS) (length (cog-incoming-by-type CLS 'MemberLink)))
(fold (lambda (CLS cnt) (+ cnt (nmemb CLS))) 0 TRUE-CLS-LST))
(define (num-to-scan)
(max min-greedy (* scan-multiplier
(+ (num-classified-words) (length FAKE-CLS-LST)))))
(define (keep WORD OLD-COUNT)
(MERGER 'clobber)
(format #t "---- Remaining count = ~6F of ~6F for \"~A\"\n"
(supp-obj 'right-count WORD) OLD-COUNT (cog-name WORD))
(if (MERGER 'discard? WORD) '() (list WORD)))
(format #t "--- To-do=~A ncls=~A sing=~A nredo=~A ~A -- \"~A\" ---\n"
(length WRD-LST) (length TRUE-CLS-LST) (length FAKE-CLS-LST)
(length DONE-LST)
(strftime "%F %T" (localtime (current-time)))
(if (null? WRD-LST) '() (cog-name (car WRD-LST)))
)
(if (null? WRD-LST) TRUE-CLS-LST
(let* ((wrd (car WRD-LST))
(rest (cdr WRD-LST))
(old-count (supp-obj 'right-count wrd))
(cls (assign-word-to-class MERGER wrd TRUE-CLS-LST)))
(if (eq? 'WordClassNode (cog-type cls))
(greedy-grow MERGER TRUE-CLS-LST FAKE-CLS-LST
(append! DONE-LST (keep wrd old-count)) rest)
(let ((new-cls (assign-word-to-class MERGER wrd FAKE-CLS-LST)))
(if (eq? 'WordNode (cog-type new-cls))
(begin
(store-atom (Member new-cls *-greedy-anchor-*))
(greedy-grow MERGER TRUE-CLS-LST
(append! FAKE-CLS-LST (list new-cls))
DONE-LST rest))
(let* ((rest-len (min (num-to-scan) (length rest)))
(short-list (take rest rest-len)))
(format #t "--- Greedy-checking next ~A items\n"
rest-len)
(assign-expand-class MERGER new-cls short-list)
(format #t "--- Checking the done-list len=~A\n"
(length DONE-LST))
(assign-expand-class MERGER new-cls DONE-LST)
(for-each
(lambda (unfake)
(cog-delete! (Member unfake *-greedy-anchor-*)))
(got-done FAKE-CLS-LST new-cls))
(greedy-grow MERGER
(sort-class-list (cons new-cls TRUE-CLS-LST))
(still-to-do FAKE-CLS-LST new-cls)
(append! DONE-LST
(got-done FAKE-CLS-LST new-cls)
(keep wrd old-count)
(got-done short-list new-cls))
(still-to-do rest new-cls))
))))))
)
(define (classify-pair-wise MERGER WRD-LST GLST)
(define (check-pair WORD-A WORD-B CLS-LST)
(if (MERGER 'merge-predicate WORD-A WORD-B)
(let ((grm-class (MERGER 'merge-function WORD-A WORD-B)))
(assign-expand-class MERGER grm-class WRD-LST)
(cons grm-class CLS-LST))))
(format #t "Start pair-wise classification of ~A words\n"
(length WRD-LST))
(fold-unordered-pairs GLST check-pair WRD-LST)
)
(define (agglo-over-words MERGER WRD-LST CLS-LST)
(format #t "Start agglo classification of ~A words\n"
(length WRD-LST))
(assign-to-classes MERGER CLS-LST '() WRD-LST)
)
(define (diag-over-words MERGER WRD-LST CLS-LST)
(define (diag-blocks wlist size clist)
(if (null? wlist) '()
(let* ((wsz (length wlist))
(minsz (if (< wsz size) wsz size))
(chunk (take wlist minsz))
(rest (drop wlist minsz))
(new-clist (block-assign-to-classes MERGER chunk clist)))
(diag-blocks rest (* 2 size) new-clist)
)
)
)
(define diag-block-size 20)
(define num-to-drop (inexact->exact (round (* 1.6 (length CLS-LST)))))
(define ranked-words (drop WRD-LST num-to-drop))
(format #t "Drop first ~A words from consideration, leaving ~A\n"
num-to-drop (length ranked-words))
(format #t "Start diag-block of ~A words, chunksz=~A\n"
(length ranked-words) diag-block-size)
(diag-blocks ranked-words diag-block-size CLS-LST)
)
(define (greedy-over-words MERGER WRD-LST CLS-LST)
(define mdone-list
(fold (lambda (CLS LST)
(append! LST (map gar (cog-incoming-by-type CLS 'MemberLink))))
'() CLS-LST))
(define done-list
(filter! (lambda (w) (eq? 'WordNode (cog-type w))) mdone-list))
(define (is-done? w)
(find (lambda (x) (equal? x w)) done-list))
(define remain-words (remove! is-done? WRD-LST))
(define junk (fetch-incoming-by-type *-greedy-anchor-* 'MemberLink))
(define singletons (map gar
(cog-incoming-by-type *-greedy-anchor-* 'MemberLink)))
(define (is-single? w)
(find (lambda (x) (equal? x w)) singletons))
(define todo-words (remove! is-single? remain-words))
(define (print-concluding-report)
(define aset (make-atom-set))
(for-each (lambda (wcn)
(for-each (lambda (memb) (aset (gar memb)))
(cog-incoming-by-type wcn 'MemberLink)))
(cog-get-atoms 'WordClassNode))
(format #t
"Finished greedy-agglomeration: ~A words assigned to ~A classes\n"
(length (aset #f)) (length (cog-get-atoms 'WordClassNode)))
)
(format #t "Start greedy-agglomeration of ~A words\n"
(length todo-words))
(format #t "Existing classes=~A singletons=~A done=~A\n"
(length CLS-LST) (length singletons) (length done-list))
(greedy-grow MERGER CLS-LST singletons done-list todo-words)
(print-concluding-report)
)
(define (load-stuff)
(define start-time (get-internal-real-time))
(display "Start loading words and word-classes\n")
(load-atoms-of-type 'WordNode)
(load-atoms-of-type 'WordClassNode)
(for-each
(lambda (cls) (fetch-incoming-by-type cls 'MemberLink))
(cog-get-atoms 'WordClassNode))
(format #t "Finished loading ~A words in ~5f seconds\n"
(length (cog-get-atoms 'WordNode))
(* 1.0e-9 (- (get-internal-real-time) start-time)))
)
(define (trim-and-rank LLOBJ WRD-LST)
(define pss (add-support-api LLOBJ))
(define (nobs WRD) (pss 'right-count WRD))
(define start-time (get-internal-real-time))
(for-each
(lambda (WRD) (fetch-atom (LLOBJ 'right-wildcard WRD)))
WRD-LST)
(format #t "Finished fetching wildcards in ~5F seconds\n"
(* 1.0e-9 (- (get-internal-real-time) start-time)))
(let* ((tr-start (get-internal-real-time))
(trimed-words
(remove (lambda (WRD) (LLOBJ 'discard-margin? WRD)) WRD-LST)))
(format #t "Trimmed in ~5F seconds\n"
(* 1.0e-9 (- (get-internal-real-time) tr-start)))
(format #t "After trimming, ~A words left, out of ~A\n"
(length trimed-words) (length WRD-LST))
(let* ((ra-start (get-internal-real-time))
(ranked-words
(sort! trimed-words
(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B))))))
(format #t "Sorting in ~5F seconds\n"
(* 1.0e-9 (- (get-internal-real-time) ra-start)))
ranked-words))
)
(define (gram-classify ALGO MERGER)
(load-stuff)
(let* ((wrd-lst (cog-get-atoms 'WordNode))
(ranked-words (trim-and-rank MERGER wrd-lst))
(cls-lst (cog-get-atoms 'WordClassNode))
(sorted-cls (sort-class-list cls-lst)))
(ALGO MERGER ranked-words sorted-cls))
)
(define-public (gram-classify-pair-wise STARS COS-CUT FRAC MIN-OBS)
"
gram-classify-pair-wise COS-CUT FRAC MIN-OBS - Merge words into
word-classes.
Very slow, exhaustive O(N^2) algorithm. Suggest using instead
`gram-classify-agglo`, `gram-classify-diag-blocks` or
`gram-classify-greedy` for better performance.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
COS-CUT is the minimum cosine between vectors before a merge is
considered.  Current recomendation is 0.65.
FRAC is the fraction of the non-overlapping disjuncts that are merged
into the class. Current recommendation is 0.3.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify classify-pair-wise (make-fuzz STARS COS-CUT FRAC ZIPF MIN-OBS))
)
(define-public (gram-classify-agglo STARS MIN-OBS)
"
gram-classify-agglo - Merge words into word-classes.
Conservative O(N^2) algorithm.  Faster than `gram-classify-pair-wise`
but still slow-ish.  Suggest using instead `gram-classify-diag-blocks`
or `gram-classify-greedy` for better performance.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify agglo-over-words (make-fuzz STARS 0.65 0.3 ZIPF MIN-OBS))
)
(define-public (gram-classify-diag-blocks STARS MIN-OBS)
"
gram-classify-diag-blocks - Merge words into word-classes.
Uses a diagonal-block merge strategy. Reasonably fast, better than
O(N^2) performance, but may miss optimal clusters. Faster than
`gram-classify-pair-wise` and `gram-classify-agglo`. However, the
`gram-classify-greedy` variant is both faster and more accurate.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify diag-over-words (make-fuzz STARS 0.65 0.3 ZIPF MIN-OBS))
)
(define-public (gram-classify-greedy-fuzz STARS COS-CUT FRAC MIN-OBS)
"
gram-classify-greedy-fuzz - Merge words into word-classes.
Uses several tricks to try to get close to O(N log N) performance,
while retaining high accuracy.  Faster than the exhaustive-search
`gram-classify-pair-wise` and `gram-classify-agglo` variants. Should
be faster and more accurate than `gram-classify-diag-blocks`.
Uses the \"fuzz\" merge algo.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
COS-CUT is the minimum cosine between vectors before a merge is
considered.  Current recomendation is 0.65.
FRAC is the fraction of the non-overlapping disjuncts that are merged
into the class. Current recommendation is 0.3.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify greedy-over-words (make-fuzz STARS COS-CUT FRAC ZIPF MIN-OBS))
)
(define-public (gram-classify-greedy-discrim STARS COSINE MIN-OBS)
"
gram-classify-greedy-discrim - Merge words into word-classes.
Uses several tricks to try to get close to O(N log N) performance,
while retaining high accuracy.  Faster than the exhaustive-search
`gram-classify-pair-wise` and `gram-classify-agglo` variants. Should
be faster and more accurate than `gram-classify-diag-blocks`.
Uses the \"discriminating\" merge algo: cosine, frac=sigmoid
See `make-discrim` for detailed documentation.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
COSINE should be the minimum cosine angle acceptable to perform
a merge on. Currently, 0.5 is recommended.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify greedy-over-words (make-discrim STARS COSINE ZIPF MIN-OBS))
)
(define-public (gram-classify-greedy-disinfo STARS MI MIN-OBS)
"
gram-classify-greedy-disinfo - Merge words into word-classes.
Deprecated. Experimental work indicates that the taper fraction
was badly designed.  This function will be removed in the future.
It should be gone by 2022, if not sooner.
Similar to `gram-classify-greedy-discrim`, but uses MI instead
of cosine to perform merge decisions and determine a merge fraction.
See `make-disinfo` for detailed documentation.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
MI should be the minimum MI acceptable to perform a merge on.
This is dataset dependent
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify greedy-over-words (make-disinfo STARS MI ZIPF MIN-OBS))
)
(define-public (gram-classify-greedy-mifuzz STARS MI FRAC MIN-OBS)
"
gram-classify-greedy-mifuzz - Merge words into word-classes.
Similar to `gram-classify-greedy-fuzz`, but uses MI instead
of cosine to perform merge decisions and determine a merge fraction.
See `make-mifuzz` for detailed documentation.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
MI should be the minimum MI acceptable to perform a merge on.
This is dataset dependent
FRAC is the fraction of the union-merge to be added into the
cluster. Recommended value is zero, else a very small number,
no larger than 1/2^self-MI of the items to be merged.
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify greedy-over-words (make-mifuzz STARS MI FRAC ZIPF MIN-OBS))
)
(define-public (gram-classify-greedy-midisc STARS MI MIN-OBS)
"
gram-classify-greedy-midisc - Merge words into word-classes.
Similar to `gram-classify-greedy-discrim`, but uses MI instead
of cosine to perform merge decisions and determine a merge fraction.
See `make-midisc` for detailed documentation.
STARS is the object holding the disjuncts. For example, it could be
(add-dynamic-stars (make-pseudo-cset-api)) or perhaps a shape vector.
MI should be the minimum MI acceptable to perform a merge on.
This is dataset dependent
MIN-OBS is the smallest number of observations of the word that
is acceptable
"
(define ZIPF 4)
(gram-classify greedy-over-words (make-midisc STARS MI ZIPF MIN-OBS))
)