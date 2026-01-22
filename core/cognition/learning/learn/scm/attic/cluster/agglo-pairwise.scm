(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define-public (pair-wise-cluster LLOBJ NRANK LOOP-CNT)
"
pair-wise-cluster LLOBJ NRANK LOOP-CNT - perform clustering.
This is the main entry point for clustering similar words. It uses
a relatively simple algorithm for determining when words should be
merged into clusters. As of this writing, it works quite well,
apparently as well as any earlier attempts.
This starts by computing similarities between pairs of words among
the NRANK most frequent words. It is recommended that NRANK be set
to between 100 and 200. This initial calculation takes hours,
depending on the dataset and the CPU.
The similarity that is computed is the symmetric-MI similarity and
the `ranked-MI` similarity. This is computed between word-vectors
that include both Sections and CrossSections. There is no option
to use a different similarity function
improvement, and seveal downsides, so we've hard-coded this.
There is no option to use anything other than the Sections (+)
CrossSections word-vector. This is because the CrossSections are
required to perform a coherent connector merge, and so we use that
throughout. This is all automated, under the covers.
LOOP-CNT is the number of grammatical classes to create. This will
loop through the merge step this many times.
The merge step picks the top-two most similar words (or word and
word-class) and merges them, including merging the connectors.
After the merge is performs, marginals are recomputed, so that they
remain accurate. After each merge, the list of most frequent
words/word-classes is recomputed, and similarities are computed for
the next three words (hard-coded at 3, for now). Thus, by the end,
thwere will be similarities for `NRANK + 3*LOOP-CNT` most frequent
words/word-classes.
This can be contrasted to the clique-merge algorithm, which computes
an in-group of the most similar pairs, and merges them in each merge
step. The clique-merge algo is more complex, because it has to find
this in-group. It is also more powerful, since it generalizes the
overlaps of the word-vectors, which this algo is fundamentally
incapable of doing.
Status: more-or-less finished. Works. Prints a lot of diagnostics.
Easy-to-use, simple, not a lot of user-adjustable parameters.
Recommended usage:
```
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(if (forgot-to-do-the-mmt-marginals-yet?)
((batch-transpose sha) 'mmt-marginals)
)
(pair-wise-cluster sha 200 500)
```
"
(setup-initial-similarities LLOBJ NRANK)
(define (none WA WB) 0.0)
(define (store-mmt WRD) (recompute-mmt LLOBJ (list WRD)))
(define (store-final) (recompute-mmt-final LLOBJ))
(define merge-pair (make-merge-pair LLOBJ
none 0.0 store-mmt store-final #t))
(define (do-merge N WA WB)
(format #t "------ Start merge ~D of `~A` and `~A`\n"
(+ N 1) (cog-name WA) (cog-name WB))
(if (and (equal? (cog-type WA) 'WordClassNode)
(equal? (cog-type WB) 'WordClassNode))
(for-each cog-delete!
(cog-incoming-by-type WB 'SimilarityLink)))
(define e (make-elapsed-secs))
(define wclass (merge-pair WA WB))
(format #t "------ Merged `~A` and `~A` into `~A` in ~A secs\n"
(cog-name WA) (cog-name WB) (cog-name wclass) (e))
(recomp-all-sim LLOBJ WA)
(recomp-all-sim LLOBJ WB)
(format #t "------ Recomputed MI for `~A` and `~A` in ~A secs\n"
(cog-name WA) (cog-name WB) (e))
)
(main-loop LLOBJ do-merge NRANK LOOP-CNT)
)