(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(define-public (make-item-pair-api)
"
make-item-pair-api -- Item-pair access methods for generic item pairs.
The counts are obtained from EvaluationLinks of the form
(EvaluationLink
(PredicateNode \"*-Item Pair-*\")
(List left-atom right-atom))
"
(make-evaluation-pair-api
*-item-pair-tag-*
'ItemNode
'ItemNode
(AnyNode "left-item")
(AnyNode "right-item")
"item-pairs"
"Generic ItemNode Pairs")
)
(define-public (make-clique-pair-api)
"
make-clique-pair-api -- Word-pair access methods from clique-counting.
Object for getting word-pair counts, obtained from clique counting.
The counts are stored on EvaluationLinks with the predicate
(PredicateNode \"*-Sentence Word Pair-*\").
"
(make-evaluation-pair-api
*-word-pair-tag-*
'WordNode
'WordNode
(AnyNode "left-word")
(AnyNode "right-word")
"cliq"
"Sentence Clique Word Pairs")
)
(define-public (make-distance-pair-api MAX-DIST)
"
make-distance-pair-api -- Word-pair access methods from
clique-counting, but limited by edge-length.
Object for getting word-pair counts, obtained from clique counting.
The counts are considered only if the distance between the words
was observed to be MAX-DIST or less.
The counts are stored in the form of ExecutionLinks:
ExecutionLink
SchemaNode  *-Pair Distance-*
ListLink
WordNode lefty
WordNode righty
NumberNode 3
"
(let* ((max-dist MAX-DIST)
(dist-name (format #f "*-Pair Max Dist ~A-*" max-dist))
(pair-max (PredicateNode dist-name)))
(define (get-count ATOM) (cog-count ATOM))
(define (set-count ATOM CNT)
(cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))
(define (get-dist ATOM)
(cog-number (cog-outgoing-atom ATOM 2)))
(define any-left (AnyNode "left-word"))
(define any-right (AnyNode "right-word"))
(define (get-left-type) 'WordNode)
(define (get-right-type) 'WordNode)
(define (get-pair-type) 'EvaluationLink)
(define (get-pair L-ATOM R-ATOM)
(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
(if (null? maybe-list) '()
(cog-link 'EvaluationLink pair-max maybe-list)))
(define (make-pair L-ATOM R-ATOM)
(EvaluationLink pair-max (ListLink L-ATOM R-ATOM)))
(define (get-left-element PAIR)
(gadr PAIR))
(define (get-right-element PAIR)
(gddr PAIR))
(define (get-pair-count L-ATOM R-ATOM)
(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
(if (null? maybe-list) 0
(fold
(lambda (pr sum) (+ sum (get-count pr)))
0
(filter!
(lambda (lnk) (<= (get-dist lnk) max-dist))
(cog-incoming-by-type maybe-list 'ExecutionLink)))))
(define (get-left-wildcard WORD)
(make-pair any-left WORD))
(define (get-right-wildcard WORD)
(make-pair WORD any-right))
(define (get-wild-wild)
(make-pair any-left any-right))
(define (fetch-distance-pairs)
(define start-time (current-time))
(fetch-incoming-set *-word-pair-dist-*)
(format #t "Elapsed time to load distance pairs: ~A secs\n"
(- (current-time) start-time)))
(define (provides meth)
(case meth
((pair-count)     get-pair-count)
((get-pair)       get-pair)
((get-count)      get-count)
((set-count)      set-count)
((make-pair)      make-pair)
((left-element)   get-left-element)
((right-element)  get-right-element)
(else             #f)))
(lambda (message . args)
(apply (case message
((name) (lambda () "Sentence Clique Distance-Limited Word Pairs"))
((id)   (lambda () "cldist"))
((left-type) get-left-type)
((right-type) get-right-type)
((pair-type) get-pair-type)
((pair-count) get-pair-count)
((get-pair) get-pair)
((get-count) get-count)
((set-count) set-count)
((make-pair) make-pair)
((left-element) get-left-element)
((right-element) get-right-element)
((left-wildcard) get-left-wildcard)
((right-wildcard) get-right-wildcard)
((wild-wild) get-wild-wild)
((fetch-pairs) fetch-distance-pairs)
((provides) provides)
((filters?) (lambda () #f))
(else (error "Bad method call on clique-pair:" message)))
args))))
(define-public (verify-clique-pair-sums)
"
This checks consistency of the the clique-pair total count, with
the subcounts of each pair, according to the distance between
the words. The sum of the subtotals should equal the total.
It should not throw.
Example usage: (verify-clique-pair-sums)
"
(define cliq (make-clique-pair-api))
(define dist (make-distance-pair-api 10000000))
(define all-pairs (cliq 'get-all-elts))
(define cnt 0)
(for-each
(lambda (PAIR)
(set! cnt (+ cnt 1))
(if (not (eqv? (cliq 'get-count PAIR) (dist 'get-count PAIR)))
(throw 'bad-count 'foobar PAIR)
(format #t "Its OK ~A\n" cnt)
))
all-pairs)
)
(define-public (batch-any-pairs)
(batch-pairs (make-any-link-api))
)
(define-public (batch-clique-pairs)
(batch-pairs (make-clique-pair-api))
)