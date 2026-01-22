(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(define-public (make-score-fn LLOBJ METHOD)
"
make-score-fn LLOBJ METHOD -- Create a function that returns a
score for a pair of atoms, the score being given by invoking
METHOD on LLOBJ.  The LLOBJ must provide the METHOD, of course,
and also the 'get-pair method, so that pairs can be assembled.
If either atom is nil, or if the atom-pair cannot be found, then a
default value of -1e40 is returned.
"
(define bad-mi -1e40)
(lambda (left-atom right-atom distance)
(define wpr
(if (and (not (null? left-atom)) (not (null? right-atom)))
(LLOBJ 'get-pair left-atom right-atom)
'()))
(if (null? wpr) bad-mi (LLOBJ METHOD wpr))
)
)
(define-public (graph-add-mst GRAPH NUMA-LIST SCORE-FN NUM-EDGES)
"
Projective, Undirected Maximum Spanning Tree parser.
Given an existing (possibly empty) GRAPH, extend it by adding up
to NUM-EDGES new edges, adding them one at a time, such that each
added edge having the highest score possible, and does not intersect
any of the existing edges. If NUM-EDGES is set to -1, then as many
edges as possible are added, until a planar spanning tree is created,
or until it is impossible to add a new edge (because the edge-score
is minus-infinity).
The NUMA-LIST should be a scheme-list of ordinally-numbered atoms.
This should be a list of scheme pairs `(Num . Atom)` where `Num` is
is an ordinal number, and `Atom` is some Atom.
The SCORE-FN should be a function that, when give a left-right ordered
pair of atoms, and the distance between them, returns a numeric score
for that pair. This numeric score will be maximized during the parse.
The SCORE-FN should take three arguments: left-atom, right-atom and
the (numeric) distance between them (i.e. when the atoms are ordered
sequentially, this is the difference between the ordinal numbers).
If no such edge exists or is impossible to score, then minus infinity
should be returned
is invoked as `(SCORE-FN Atom Atom Dist)`.
The NUM-EDGES should be an integer, indicating the number of extra
edges to add to the GRAPH. The highest-scoring edges are added
first, until either NUM-EDGES edges have been added, or it is not
possible to add any more edges.  There are two reasons for not being
able to add more edges: (1) the extension would no longer be a tree,
or (2) no such edges are recorded in the AtomSpace (they have a score
of minus-infinity). To add as many edges as possible, pass -1 for
NUM-EDGES.
This returns a new graph, in the form of a wedge-list.
"
(define bad-mi -1e30)
(define min-acceptable-mi -1e15)
(define bad-pair (cons (cons (cons 0 #f) (cons 0 #f)) bad-mi))
(define (pick-best-cost-left-pair left-numa numa-list)
(fold
(lambda (right-numa max-pair)
(define max-mi (cdr max-pair))
(define cur-mi
(SCORE-FN (cdr left-numa) (cdr right-numa)
(- (car right-numa) (car left-numa))))
(if (< max-mi cur-mi)
(cons (cons left-numa right-numa) cur-mi)
max-pair
)
)
bad-pair
numa-list
)
)
(define (pick-best-cost-right-pair right-numa numa-list)
(fold
(lambda (left-numa max-pair)
(define max-mi (cdr max-pair))
(define cur-mi
(SCORE-FN (cdr left-numa) (cdr right-numa)
(- (car right-numa) (car left-numa))))
(if (<= max-mi cur-mi)
(cons (cons left-numa right-numa) cur-mi)
max-pair
)
)
bad-pair
numa-list
)
)
(define (pick-best-cost-pair numa-list)
(define best-left (pick-best-cost-left-pair
(car numa-list) (cdr numa-list)))
(if (eq? 2 (length numa-list))
best-left
(let ((best-rest (pick-best-cost-pair (cdr numa-list))))
(if (< (cdr best-left) (cdr best-rest))
best-rest
best-left
)
)
)
)
(define (starting-edge numa-list)
(define start-pair (pick-best-cost-pair numa-list))
(if (equal? bad-pair start-pair) '() (list start-pair))
)
(define (max-of-pair-list choice-list)
(define (*pick-best choice-list best-so-far)
(define so-far-mi (cdr best-so-far))
(if (null? choice-list)
best-so-far
(let* ((first-choice (car choice-list))
(first-mi (cdr first-choice))
(curr-best
(if (<= so-far-mi first-mi)
first-choice
best-so-far)))
(*pick-best (cdr choice-list) curr-best)))
)
(*pick-best choice-list bad-pair)
)
(define (connect-numa brk-numa numa-list)
(define brk-num (car brk-numa))
(define brk-node (cdr brk-numa))
(filter-map
(lambda (numa)
(define try-num (car numa))
(define try-node (cdr numa))
(if (< try-num brk-num)
(let ((mi (SCORE-FN try-node brk-node (- brk-num try-num))))
(if (< min-acceptable-mi mi)
(cons (cons numa brk-numa) mi) #f))
(let ((mi (SCORE-FN brk-node try-node (- try-num brk-num))))
(if (< min-acceptable-mi mi)
(cons (cons brk-numa numa) mi) #f))
)
)
numa-list
)
)
(define (connect-to-graph bare-numas graph-numas)
(append-map
(lambda (grph-numa) (connect-numa grph-numa bare-numas))
graph-numas
)
)
(define (pick-no-cross-best candidates graph-wedges)
(define best (max-of-pair-list candidates))
(if (not (wedge-cross-any? best graph-wedges))
best
(pick-no-cross-best
(lset-difference equal? candidates (list best)) graph-wedges)
)
)
(define (get-fresh cost-pair numa-list)
(define numa-pair (car cost-pair))
(define left-numa (car numa-pair))
(define right-numa (cdr numa-pair))
(if (any (lambda (numa) (equal? numa left-numa)) numa-list)
left-numa
right-numa
)
)
(define (*pick-em numa-list graph-links nected-numas n-to-do)
(define trial-pairs (connect-to-graph numa-list nected-numas))
(define best (pick-no-cross-best trial-pairs graph-links))
(if (or (= 0 n-to-do) (>= min-acceptable-mi (cdr best)))
graph-links
(let* (
(bigger-graph (append graph-links (list best)))
(fresh-numa (get-fresh best numa-list))
(shorter-list (lset-difference equal? numa-list (list fresh-numa)))
(more-nected (append nected-numas (list fresh-numa)))
)
(if (null? shorter-list)
bigger-graph
(*pick-em shorter-list bigger-graph more-nected (- n-to-do 1))
)
)
)
)
(define starting-graph
(if (null? GRAPH)
(starting-edge NUMA-LIST)
GRAPH))
(define nected-list (numas-in-wedge-list starting-graph))
(define discon-list (lset-difference equal? NUMA-LIST nected-list))
(if (null? starting-graph)
'()
(*pick-em discon-list starting-graph nected-list NUM-EDGES))
)
(define-public (mst-parse-atom-seq ATOM-LIST SCORE-FN)
"
Projective, Undirected Maximum Spanning Tree parser.
Given a sequence of atoms, find an unlabeled, undirected, projective
dependency parse of the sequence, by finding a dependency tree that
maximizes the pair-wise scoring function. This returns a list of
atom-pairs, together with associated score.  The tree is projective,
in that no edges cross.
The ATOM-LIST should be a scheme-list of atoms, all presumably of
a uniform atom type.
The SCORE-FN should be a function that, when give a left-right ordered
pair of atoms, and the distance between them, returns a numeric score
for that pair. This numeric score will be maximized during the parse.
The most basic choice is to use the mutual information between the
pair of atoms.
-----
The M in MST often stands for 'minimum'
score is maximized.
There are many MST algorithms
Prim is very easy
Kruskal is good, but it seems hard to control a no-link-crossing
constraint with it. This implements a variant of Borůvka's algo,
which seems to be robust, and fast enough for the current needs.
It has been benchmarked (using the code in `bench-mst`) to run in
O(N^3) time, for a sequence of length N. From what I can tell, the
state-of-the-art projective algo is Eisner, which runs at O(N^3) time.
The code here is NOT Eisner! but seems to have comparable run-time.
The projective (no-edge-cross) constraint might not be required, see
R. Ferrer-i-Cancho (2006) “Why do syntactic links not cross?”
However, that would require changing the metric from mutual information
to something else, perhaps incorporating the dependency distance
(as defined by Ferrer-i-Cancho), or possibly the 'hubiness', or some
combination.  Since I really, really want to stick to entropy concepts,
the mean-dependency-distance metric needs to be re-phrased as some
sort of graph entropy. Hmmm...
Another idea is to apply the Dick Hudson Word Grammar landmark
transitivity idea, but exactly how this could work for unlabeled
trees has not been explored.
So, for now, a no-links-cross constraint is hand-coded into the algo.
Without it, it seems that the pair-MI scores alone give rather unruly
dependencies (unclear, needs exploration).  So, in the long-run, it
might be better to instead pick something that combines MI scores with
mean-dependency-distance or with hubiness. See, for example:
Haitao Liu (2008) “Dependency distance as a metric of language
comprehension difficulty” Journal of Cognitive Science, 2008 9(2): 159-191.
or also:
Ramon Ferrer-i-Cancho (2013) “Hubiness, length, crossings and their
relationships in dependency trees”, ArXiv 1304.4086
"
(define numa-list (atom-list->numa-list ATOM-LIST))
(graph-add-mst '() numa-list SCORE-FN -1)
)