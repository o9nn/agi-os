(use-modules (opencog) (opencog persist) (opencog persist-sql))
(use-modules (opencog nlp) (opencog learn))
(use-modules (opencog matrix))
(use-modules (srfi srfi-1))
(use-modules (opencog cogserver))
(start-cogserver)
(sql-open "postgres:///en_pairs_ttwo_sim?user=linas")
(load-atoms-of-type 'WordNode)
(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
(psa 'fetch-pairs)
(print-matrix-summary-report psa)
(define fsi (add-subtotal-filter psa 50 30 10))
(define all-cset-words (fsi 'left-basis))
(length all-cset-words)
(define top-cset-words
(filter (lambda (wrd) (< 1500 (cset-vec-word-observations wrd)))
all-cset-words))
(length top-cset-words)
(define ranked-csw (sort top-cset-words
(lambda (a b) (> (cset-vec-word-observations a) (cset-vec-word-observations b)))))
(define poi (add-pair-cosine-compute fsi))
(define (get-cos wa wb)
(define cos-fkey (PredicateNode "*-Cosine 803 Key-*"))
(pair-sym-cache wa wb cos-fkey
(lambda (wx wy) (poi 'right-cosine wx wy))))
(define (make-cos wa wb)
(define cos-fkey (PredicateNode "*-Cosine 803 Key-*"))
(pair-sym-cache wa wb cos-fkey
(lambda (wx wy)
(define wcos (poi 'right-cosine wx wy))
(store-atom (List wx wy))
(store-atom (List wy wx))
wcos)))
(define (make-all-cos wordlist)
(define wlen (length wordlist))
(if (< 0 wlen)
(let ((head (car wordlist))
(rest (cdr wordlist)))
(format #t "Pairs remaining: ~A\n" wlen)
(for-each (lambda (w) (make-cos head w)) wordlist)
(make-all-cos rest))))
(make-all-cos (take ranked-csw 40))
(define (make-them-all nxt)
(if (< nxt (length ranked-csw))
(make-all-cos (take ranked-csw nxt))
(make-all-cos ranked-csw))
(if (< nxt (length ranked-csw))
(make-them-all (inexact->exact (truncate (* 1.5 nxt))))))
(define (make-get-cmi LST)
(define left-marg (make-left-summer LST get-cos))
(define right-marg (make-right-summer LST get-cos))
(define cos-wild-wild
(fold (lambda (it acc) (+ acc (left-marg it))) 0 LST))
(define oln2 (- (/ 1.0 (log 2.0))))
(lambda (word-a word-b)
(* oln2 (log (/
(* (get-cos word-a word-b) cos-wild-wild)
(* (left-marg word-b) (right-marg word-a)))))))
(define get-cmi (make-get-cmi (take ranked-csw 108)))
(define get-cmi (make-get-cmi (take ranked-csw 162)))
(define (pair-sym-cache wa wb KEY FN)
"
Get cached value, or compute it.  Symmetric.
Stores the value in the atomspace, under KEY for (ListLink wa wb)
"
(define (get-val PR)
(cog-value-ref (cog-value PR KEY) 0))
(define (set-val PR VAL)
(cog-set-value! PR KEY (FloatValue VAL)))
(define wpr (ListLink wa wb))
(define got
(catch #t
(lambda () (get-val wpr))
(lambda (k . args) #f)))
(if got got
(let* ((flp (ListLink wb wa))
(gat (catch #t
(lambda () (get-val flp))
(lambda (k . args) #f))))
(if gat
(begin
(set-val wpr gat)
gat)
(let ((val (FN wa wb)))
(set-val wpr val)
(set-val flp val)
val)))))
(define (make-left-summer LST FN)
"
Return a function (func ITEM) that performs wild-card sums
over FN(*, ITEM) for * in LST.
"
(define (summer ITEM)
(fold
(lambda (it acc) (+ acc (FN it ITEM)))
0
LST))
(make-afunc-cache summer))
(define (make-right-summer LST FN)
"
Return a function (func ITEM) that performs wild-card sums
over FN(ITEM, *) for * in LST.
"
(define (summer ITEM)
(fold
(lambda (it acc) (+ acc (FN ITEM it)))
0
LST))
(make-afunc-cache summer))
(define (make-sym-pairs LST FN)
"
Return a list of pair-value pairs of pairs constructed from LST
and values obtained by applying FN.
That is, return a list of ( (x . y) . v) where x and y are from LST
and v is equal to (FN x y)
"
(define (make-all-helper WLST RSLT)
(define wlen (length WLST))
(if (< 0 wlen)
(let* ((head (car WLST))
(rest (cdr WLST))
(prs (map (lambda (w) (cons (cons head w) (FN head w))) WLST))
)
(format #t "Pairs remaining: ~A\n" wlen)
(make-all-helper rest (append prs RSLT)))
RSLT))
(make-all-helper LST '()))