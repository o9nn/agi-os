(use-modules (opencog))
(use-modules (opencog persist))
(define-public (get-total-atom-count atom-list)
(define (hlpr lst cnt)
(if (null? lst) cnt
(hlpr (cdr lst) (+ cnt (get-count (car lst))))))
(hlpr atom-list 0)
)
(define freq-key (PredicateNode "*-FrequencyKey-*"))
(define (set-freq ATOM FREQ)
(define ln2 (* -1.4426950408889634 (log FREQ)))
(cog-set-value! ATOM freq-key (FloatValue FREQ ln2))
)
(define (compute-atom-logli atom total)
(set-freq atom (/ (get-count atom) total))
)
(define (compute-all-logli atom-list)
(let ((total (get-total-atom-count atom-list)))
(map
(lambda (atom) (compute-atom-logli atom total))
atom-list
)
)
)
(define (compute-all-word-freqs)
(begin
(call-only-once fetch-all-words)
(compute-all-logli (get-all-words))
)
)
(define-public (total-word-observations)
"
total-word-observations -- return a total of the number of times
any/all words were observed.  That is, compute and return N(*),
as defined above, and in the diary.  This does NOT work from a
cached value.  Also, this does NOT fetch atoms from the database!
"
(get-total-atom-count (get-all-words))
)
(define-public (get-sentence-count)
"
get-sentence-count -- get the number of sentences observed.
This does fetch the count from the database.
This count is maintained by the link-pipeline code.
"
(get-count (fetch-atom (SentenceNode "ANY")))
)
(define-public (get-parse-count)
"
get-parse-count -- get the number of parses observed.
This does fetch the count from the database.
This count is maintained by the link-pipeline code.
"
(get-count (fetch-atom (ParseNode "ANY")))
)
(define-public (avg-sentence-length)
"
avg-sentence-length -- get expected value for the number of words
in the sentence.
"
(/ (total-word-observations) (get-parse-count))
)