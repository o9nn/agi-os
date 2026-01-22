(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(define-public (graph-add-mpg GRAPH NUMA-LIST SCORE-FN NUM-EDGES)
"
Projective, Undirected Maximum Planar Graph (MPG) parser.
Given an existing GRAPH, add up to NUM-EDGES additional edges, such
that each added edge has the highest possible score, and no added
edge intersects any existing edge.  The non-intersection constraint
keeps the graph planar or projective. If NUM-EDGES is set to -1,
then as many edges as possible are added, resulting in the maximal
planar graph.
The GRAPH should be an existing (possibly empty) list of 'wedges'
connecting Atom pairs. Each 'wedge' is a weighted pair of numbered
atoms, having the scheme form of `((NL . AL) (NR . AR) . W)` where
AL and AR are the left and right Atoms of the edge
ordinal numbers (integers), such that NL is less than NR, and W is
a floating-point weight. The dot represents a scheme pair, built
with `cons`.
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
able to add more edges: (1) there is no room or (2) no such edges are
recorded in the AtomSpace (they have a score of minus-infinity). To
add as many edges as possible, pass -1 for NUM-EDGES.
This returns a new graph, in the form of a wedge-list.
"
(define min-acceptable-mi -1e15)
(define (inter-links NUMA NALI WELI)
(filter-map
(lambda (r-numa)
(define weight
(SCORE-FN (cdr NUMA) (cdr r-numa)
(- (car r-numa) (car NUMA))))
(define wedge (cons (cons NUMA r-numa) weight))
(and (< min-acceptable-mi weight)
(not (wedge-cross-any? wedge WELI))
wedge))
NALI)
)
(define (non-intersecting-links NALI WELI)
(define (*tail-rec nali rslt)
(define rest (cdr nali))
(if (null? rest) rslt
(*tail-rec rest
(append rslt (inter-links (car nali) rest WELI)))))
(if (null? NALI) '() (*tail-rec NALI '()))
)
(define candidates (non-intersecting-links NUMA-LIST GRAPH))
(define sorted-cands
(sort candidates
(lambda (sa sb)
(< (wedge-get-score sb) (wedge-get-score sa)))))
(define (add-link NED CANDS RSLT)
(if (or (= 0 NED) (null? CANDS)) RSLT
(if (wedge-cross-any? (car CANDS) RSLT)
(add-link NED (cdr CANDS) RSLT)
(add-link (- NED 1) (cdr CANDS) (cons (car CANDS) RSLT)))))
(add-link NUM-EDGES sorted-cands GRAPH)
)
(define-public (mpg-parse-atom-seq ATOM-LIST SCORE-FN)
"
Projective, Undirected Maximum Planar Graph parser.
Given a sequence of atoms, find an unlabeled, undirected, projective
maximum spanning-tree parse. To this parse, add additional edges
until NUM-LOOPS have been created. The resulting graph is planar
(projective) in that no edges cross.
The ATOM-LIST should be a scheme-list of atoms, all presumably of
a uniform atom type.
The SCORE-FN should be a function that, when give a left-right ordered
pair of atoms, and the distance between them, returns a numeric score
for that pair. This numeric score will be maximized during the parse.
See `graph-add-mpg` for additional details.
"
(define numa-list (atom-list->numa-list ATOM-LIST))
(define mst-tree (graph-add-mst '() numa-list SCORE-FN -1))
(graph-add-mpg mst-tree numa-list SCORE-FN -1)
)