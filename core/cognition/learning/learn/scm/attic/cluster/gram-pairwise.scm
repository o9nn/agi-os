(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define-public (accumulate-count LLOBJ ACC DONOR FRAC)
"
accumulate-count LLOBJ ACC DONOR FRAC -- Accumulate a fraction
FRAC of the count from DONOR into ACC.
ACC and DONOR should be two pairs in the matrix LLOBJ.
FRAC should be a numeric fraction, between 0.0 and 1.0.
A fraction FRAC of the count on DONOR will be transferred to ACC.
"
(define (is-zero? cnt) (< cnt 1.0e-10))
(define moved (LLOBJ 'move-count ACC DONOR FRAC))
(when (not (is-zero? moved))
(rebalance-count LLOBJ ACC (get-count ACC))
(rebalance-count LLOBJ DONOR (get-count DONOR))
)
moved
)
(define (assign-to-cluster LLOBJ CLS WA CLIQUE)
"
assign-to-cluster LLOBJ CLS WA CLIQUE --
Loop over the disjuncts on WA, and call CLIQUE on each,
passing CLS and the disjunct to it.
A MemberLink from WA to CLS will be created, holding the
accumulated count returned by CLIQUE.
LLOBJ is used to access pairs.
WA should be of `(LLOBJ 'left-type)`
CLS should be interpretable as a row in LLOBJ.
CLIQUE is a function that returns how much of a given disjunct
is merged.
The merger of row WA into CLS is performed, using the CLIQUE
function to make disjunct-by-disjunct merge decisions.
This assumes that storage is connected
written to storage.
"
(define monitor-count (make-count-monitor))
(define accum-cnt 0)
(for-each
(lambda (PAIR-A)
(define cnt (CLIQUE CLS PAIR-A accumulate-count))
(monitor-count #f)
(when (< 0 cnt)
(monitor-count #t)
(set! accum-cnt (+ accum-cnt cnt))))
(LLOBJ 'right-stars WA))
(define memb-a (MemberLink WA CLS))
(cog-inc-count! memb-a accum-cnt)
(store-atom memb-a)
(monitor-count
(string-append
"------ Assign: Merged ~D of ~D sections on `"
(cog-name WA)
"` in ~6,1F secs\n"))
)
(define (rebalance-shapes LLOBJ CLS WA CLIQUE)
"
rebalance-shapes LLOBJ CLS WA CLIQUE --
Loop over the pairs having WA on the left, and call CLIQUE on each,
passing CLS and the pair to it.
LLOBJ is used to access pairs.  WA and CLS should both be 'left-types
of `LLOBJ`
CLIQUE should be a function that takes the given CLS and donor pair,
and uses those to deduce the merge-into pair. The merge-into pair,
and the donor pair are then handed to `rebalance-merge`, which makes
the counts consistent on both pairs.
This assumes that storage is connected
to storage.
"
(define monitor-rate (make-rate-monitor))
(for-each
(lambda (PAIR-A)
(monitor-rate #f)
(CLIQUE CLS PAIR-A rebalance-merge)
)
(LLOBJ 'right-stars WA))
(monitor-rate
"------ Assign: Revised ~A shapes in ~5F secs; ~6F scts/sec\n")
)
(define-public (start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON)
"
start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON --
Start a new cluster by merging rows WA and WB of LLOBJ into a
combined row CLS.
In the prototypical use case, each row corresponds to a WordNode,
and the result of summing them results in a WordClassNode. Thus,
by convention, it is assumed that the pairs are (word, disjunct)
pairs, and LLOBJ was made by `make-pseudo-cset-api` or by
`add-shape-vec-api`. The code itself is generic, and might work on
other kinds of LLOBJ's too. (It might work, but has not been tested.)
LLOBJ is used to access pairs.
WA and WB should both be of `(LLOBJ 'left-type)`. They should
designate two different rows in LLOBJ that will be merged,
column-by-column.
CLS denotes a new row in LLOBJ, that will contain the merged counts.
MemberLinks will be created from WA and WB to CLS.
FRAC-FN should be a function taking WA and WB as arguments, and
returning a floating point number between zero and one, indicating
the fraction of a non-shared count to be used.
Returning 1.0 gives the sum of the union of supports
Returning 0.0 gives the sum of the intersection of supports.
MRG-CON boolean flag
The merger of rows WA and WB are performed, using the 'projection
merge' strategy described above. To recap, this is done as follows.
If counts on a given column of both WA and WB are non-zero, they are
summed, and the total is placed on the matching column of CLS. The
contributing columns are removed (as their count is now zero).
If one is zero, and the other is not, then only a FRAC of the count
is transferred.
Accumulated row totals are stored in the two MemberLinks that attach
WA and WB to CLS.
This assumes that storage is connected
to storage.
"
(define frac-to-merge (FRAC-FN WA WB))
(define (make-flat CLUST SECT)
(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))
(define (clique CLUST SECT ACC-FUN)
(define WRD (LLOBJ 'left-element SECT))
(define DJ (LLOBJ 'right-element SECT))
(define WOTHER (if (equal? WRD WA) WB WA))
(define OTHSEC (LLOBJ 'get-pair WOTHER DJ))
(if (nil? OTHSEC)
(if (<= (LLOBJ 'get-count SECT) NOISE)
(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
(if (< 0 frac-to-merge)
(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT frac-to-merge)))
(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
)
)
(MemberLink WA CLS)
(MemberLink WB CLS)
(assign-to-cluster LLOBJ CLS WA clique)
(assign-to-cluster LLOBJ CLS WB clique)
(when MRG-CON
(rebalance-shapes LLOBJ CLS WA clique)
(rebalance-shapes LLOBJ CLS WB clique)
)
(define e (make-elapsed-secs))
(LLOBJ 'clobber)
(remove-empty-sections LLOBJ WA MRG-CON)
(remove-empty-sections LLOBJ WB MRG-CON)
(remove-empty-sections LLOBJ CLS MRG-CON)
(LLOBJ 'clobber)
(format #t "------ StartCluster: Cleanup ~A in ~5F secs\n"
(cog-name CLS) (e))
)
(define-public (merge-into-cluster LLOBJ CLS WA FRAC-FN NOISE MRG-CON)
"
merge-into-cluster LLOBJ CLS WA FRAC-FN MRG-CON --
Merge WA into cluster CLS. These are two rows in LLOBJ,
the merge is done column-by-column. A MemberLink from
WA to CLS will be created.
See start-cluster for additional details.
LLOBJ is used to access pairs.
WA should be of `(LLOBJ 'left-type)`
CLS should be interpretable as a row in LLOBJ.
FRAC-FN should be a function taking CLS and WA as arguments, and
returning a floating point number between zero and one, indicating
the fraction of a non-shared count to be used.
Returning 1.0 gives the sum of the union of supports
Returning 0.0 gives the sum of the intersection of supports.
MRG-CON boolean flag
The merger of row WA into CLS is performed, using the 'projection
merge' strategy described above. To recap, this is done as follows.
If counts on a given column of both CLS and WA are non-zero, then
all of the count from WA is transferred to CLS. That column in WA
is removed (as it's count is now zero). If the count on CLS is zero,
then only a FRAC of WA's count is transferred.
Accumulated row totals are stored in the MemberLink that attaches
WA to CLS.
This assumes that storage is connected
to storage.
"
(define frac-to-merge (FRAC-FN CLS WA))
(define (make-flat CLUST SECT)
(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))
(define (clique CLUST SECT ACC-FUN)
(define DJ (LLOBJ 'right-element SECT))
(define CLS-SECT (LLOBJ 'get-pair CLUST DJ))
(if (nil? CLS-SECT)
(if (or
(<= (LLOBJ 'get-count SECT) NOISE)
(LLOBJ 'is-nonflat? CLUST SECT))
(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
(if (< 0 frac-to-merge)
(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT frac-to-merge)))
(ACC-FUN LLOBJ CLS-SECT SECT 1.0))
)
(MemberLink WA CLS)
(assign-to-cluster LLOBJ CLS WA clique)
(when MRG-CON
(rebalance-shapes LLOBJ CLS WA clique)
)
(define e (make-elapsed-secs))
(LLOBJ 'clobber)
(remove-empty-sections LLOBJ WA MRG-CON)
(remove-empty-sections LLOBJ CLS MRG-CON)
(LLOBJ 'clobber)
(format #t "------ Merge-Into-Cluster: Cleanup ~A in ~5F secs\n"
(cog-name CLS) (e))
)
(define-public (merge-clusters LLOBJ CLA CLB MRG-CON)
"
merge-clusters LLOBJ CLA CLB FRAC-FN MRG-CON --
Merge clusters CLA and CLB. These are two rows in LLOBJ,
the merge is done column-by-column.
This will perform a \"union merge\" -- all disjuncts on CLB will
be transferred to CLA, and CLB will be removed.
See start-cluster for additional details.
"
(define (make-flat CLUST SECT)
(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))
(define (clique CLUST SECT ACC-FUN)
(define DJ (LLOBJ 'right-element SECT))
(define MSECT (LLOBJ 'make-pair CLUST DJ))
(define CLS-SECT (make-flat CLUST MSECT))
(ACC-FUN LLOBJ CLS-SECT SECT 1.0)
)
(for-each
(lambda (MEMB-B) (MemberLink (gar MEMB-B) CLA))
(cog-incoming-by-type CLB 'MemberLink))
(assign-to-cluster LLOBJ CLA CLB clique)
(cog-delete! (Member CLB CLA))
(for-each
(lambda (MEMB-B)
(define WRD (gar MEMB-B))
(define CNT-A 0)
(define CNT-B (LLOBJ 'get-count MEMB-B))
(define MEMB-A (cog-link 'MemberLink WRD CLA))
(if (not (nil? MEMB-A))
(set! CNT-A (LLOBJ 'get-count MEMB-A)))
(define MBA (MemberLink WRD CLA))
(set-count MBA (+ CNT-A CNT-B))
(store-atom MBA)
(if (not (cog-delete! MEMB-B))
(set-count MEMB-B 0))
)
(cog-incoming-by-type CLB 'MemberLink))
(when MRG-CON
(rebalance-shapes LLOBJ CLA CLB clique)
)
(define e (make-elapsed-secs))
(LLOBJ 'clobber)
(remove-empty-sections LLOBJ CLA MRG-CON)
(remove-empty-sections LLOBJ CLB MRG-CON)
(LLOBJ 'clobber)
(if (or
(not (equal? 0 (cog-incoming-size-by-type CLB 'Section)))
(not (equal? 0 (cog-incoming-size-by-type CLB 'CrossSection)))
(not (equal? 0 (cog-incoming-size-by-type CLB 'Shape))))
(throw 'non-empy-class 'merge-clusters "we expect it to be empty!"))
(cog-delete! CLB)
(format #t "------ Merge-Clusters: Cleanup ~A in ~5F secs\n"
(cog-name CLA) (e))
)
(define-public (make-merge-pair STARS FRAC-FN NOISE STORE FIN MRG-CON)
"
make-merger-pair STARS FRAC-FN NOISE STORE FIN MRG-CON --
Return object that implements the `merge-project` merge style
(as described at the top of this file).
STARS is the object holding the disjuncts. For example, it could
be (add-dynamic-stars (make-pseudo-cset-api))
FRAC-FUN is a function that takes two rows in STARS and returns a
number between 0.0 and 1.0 indicating what fraction of a row to merge,
when the corresponding matrix element in the other row is null.
NOISE is a floating-point numeric value indicating a count, below
which a merge will always be made. That is, if the count on the
donating section is less than this value, then that section will
be merged in it's entirety (ignoring the value returned by FRAC-FUN.)
STORE is an extra function called, after the merge is to completed,
and may be used to compute and store additional needed data that
the algo here is unaware of. This include computation of supports,
marginal MI and similar. It is called with an argument of the altered
row.
FIN is an extra function called, after the merge is to completed.
It is called without an argument.
MRG-CON is #t if Connectors should also be merged.  This requires
that the STARS object have shapes on it.
"
(define (merge WA WB)
(define wa-is-cls (equal? (STARS 'cluster-type) (Type (cog-type WA))))
(define wb-is-cls (equal? (STARS 'cluster-type) (Type (cog-type WB))))
(define cls (STARS 'make-cluster WA WB))
(STARS 'clobber)
(cond
((and wa-is-cls wb-is-cls)
(merge-clusters STARS WA WB MRG-CON))
((and (not wa-is-cls) (not wb-is-cls))
(begin
(start-cluster STARS cls WA WB FRAC-FN NOISE MRG-CON)
(STORE cls)))
(wa-is-cls
(merge-into-cluster STARS WA WB FRAC-FN NOISE MRG-CON))
(wb-is-cls
(merge-into-cluster STARS WB WA FRAC-FN NOISE MRG-CON))
)
(STORE WA)
(STORE WB)
(FIN)
cls
)
merge
)