(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define TRACK-ENTROPY #f)
(define (main-loop LLOBJ SORT-PAIRS MERGE-FUN EXPAND-UNIVERSE
NRANK LOOP-CNT PUSH-FRAMES)
"
Unleash the fury. Inside of a loop, apply the MERGE-FUN to the
top-ranked word-pair, for LOOP-CNT iterations. After each
iteration, the the EXPAND-UNIVERSE function is called. The default
expansion computes the similarities for a few more words, so that,
no matter the LOOP-CNT, there is a suitably deep set of word-pair
similarities to rank and consider.
"
(define base-done-count (get-merge-iteration LLOBJ))
(define log-dataset-stuff (make-merge-logger LLOBJ))
(define (loop-step N)
(define iter-count (+ 1 N base-done-count))
(when PUSH-FRAMES
(let* ((as-name (format #f "MI-merge layer ~D" iter-count))
(as-frame (cog-new-atomspace as-name (cog-atomspace))))
(cog-set-atomspace! as-frame)
(store-frames as-frame)))
(define e (make-elapsed-secs))
(define sorted-pairs (SORT-PAIRS LLOBJ))
(define top-pair (car sorted-pairs))
(log-dataset-stuff top-pair)
(MERGE-FUN iter-count (gar top-pair) (gdr top-pair))
(format #t "------ Completed merge in ~A secs\n" (e))
(EXPAND-UNIVERSE LLOBJ iter-count NRANK)
(update-merge-iteration LLOBJ iter-count)
)
(for-each loop-step (iota LOOP-CNT))
)
(define (get-affected-basis LLOBJ WRD-LIST)
"
get-affected-basis LLOBJ WRD-LIST - Return two lists of basis
elements affected by the merge.
The first list is the left basis, the second list is the right-basis.
"
(define dj-set (make-atom-set))
(define wrd-set (make-atom-set))
(for-each wrd-set WRD-LIST)
(define (pair-margins PAIR)
(wrd-set (LLOBJ 'left-element PAIR))
(dj-set (LLOBJ 'right-element PAIR)))
(define (cross-margins PAIR)
(for-each pair-margins (LLOBJ 'make-cross-sections PAIR)))
(define (expand-margins PAIR)
(dj-set (LLOBJ 'right-element PAIR))
(if (equal? 'Section (cog-type PAIR))
(cross-margins PAIR)
(let ((sect (LLOBJ 'make-section PAIR)))
(pair-margins sect)
(cross-margins sect))))
(for-each
(lambda (WRD) (for-each expand-margins (LLOBJ 'right-stars WRD)))
WRD-LIST)
(define affected-djs (dj-set #f))
(for-each (lambda (DJ) (for-each wrd-set (LLOBJ 'left-duals DJ)))
affected-djs)
(list (wrd-set #f) affected-djs)
)
(define (recompute-entropies LLOBJ wrd-list dj-list)
"
recompute-entropies LLOBJ wrd-list dj-list -- Recompute marginal entropy
The marginal entropies and the marginal MI for MI(w,d) appears to be
interesting. So keep these up to date.
At this time, these are all just 'interesting'
needed for anything, so this computation could be skipped. All that
would happen is that the logging of data would fail.
This does take a significant amount of CPU time!
"
(define freq-obj (make-compute-freq LLOBJ))
(define ent-obj (add-entropy-compute LLOBJ))
(define sup-obj (add-support-api LLOBJ))
(freq-obj 'init-freq)
(define e (make-elapsed-secs))
(for-each
(lambda (WRD)
(for-each (lambda (PR) (freq-obj 'cache-pair-freq PR))
(LLOBJ 'right-stars WRD)))
wrd-list)
(for-each
(lambda (DJ)
(when (< 0 (sup-obj 'left-count DJ))
(freq-obj 'cache-left-freq DJ)
(store-atom (ent-obj 'cache-left-entropy DJ))))
dj-list)
(for-each
(lambda (WRD)
(when (< 0 (sup-obj 'right-count WRD))
(freq-obj 'cache-right-freq WRD)
(store-atom (ent-obj 'cache-right-entropy WRD))))
wrd-list)
(format #t "------ Recomputed entropies in ~A secs\n" (e))
)
(define (recompute-mmt LLOBJ wrd-list dj-list)
"
recompute-mmt LLOBJ wrd-list dj-list -- Recompute MMT for for the
basis elements in wrd-list dj-list.
This recomputes the marginals for support and counts for the words
in the WRD-LIST, and also for the disjuncts attached to those words.
In particular, this recomputes the N(*,d) which is needed by MM^T.
"
(define sup (add-support-api LLOBJ))
(define psu (add-support-compute LLOBJ))
(define atc (add-transpose-compute LLOBJ))
(define dj-orphan (make-atom-set))
(define wrd-orphan (make-atom-set))
(for-each
(lambda (DJ)
(define marg (psu 'set-left-marginals DJ))
(if (< 0 (sup 'left-count DJ))
(store-atom marg) (dj-orphan marg)))
dj-list)
(for-each
(lambda (WRD)
(define marg (psu 'set-right-marginals WRD))
(if (< 0 (sup 'right-count WRD))
(store-atom marg) (wrd-orphan marg)))
wrd-list)
(for-each
(lambda (WRD) (store-atom (atc 'set-mmt-marginals WRD)))
wrd-list)
(if TRACK-ENTROPY
(recompute-entropies LLOBJ wrd-list dj-list))
(list (wrd-orphan #f) (dj-orphan #f))
)
(define (delete-orphans LLOBJ left-marg right-marg)
"
delete-orphans left-marg right-marg -- delete marginals.
These are the marginals associated with words or disjuncts that
have zero count -- i.e. do not appear in the dataset any longer.
"
(define base-obj
(if (LLOBJ 'provides 'cover-base)
(LLOBJ 'cover-base) #f))
(for-each (lambda (WMARG)
(when (cog-atom? WMARG)
(let ((WRD (LLOBJ 'left-element WMARG)))
(cog-delete! WMARG)
(cog-delete-recursive! WRD))))
left-marg)
(for-each (lambda (DJMARG)
(when (cog-atom? DJMARG)
(if (eq? 'ShapeLink (cog-type DJMARG))
(cog-delete! DJMARG)
(let ((DJ (base-obj 'right-element DJMARG)))
(cog-delete! DJMARG)
(cog-delete-recursive! DJ)))))
right-marg)
)
(define (recompute-mmt-final LLOBJ)
"
recompute-mmt-final LLOBJ -- recompute grand totals for the MM^T case
"
(define asc (add-support-compute LLOBJ))
(define atc (add-transpose-compute LLOBJ))
(store-atom (asc 'set-left-totals))
(store-atom (asc 'set-right-totals))
(store-atom (atc 'set-mmt-totals))
(when TRACK-ENTROPY
(let ((ent-obj (add-entropy-compute LLOBJ)))
(store-atom (ent-obj 'cache-entropy))))
)
(define (recompute-marginals LLOBJ WRD-LIST)
"
recompute-marginals LLOBJ WRD-LIST - Recompute marginals after merge.
Recomputes all marginals for all Sections and CrossSections touched
by WRD-LIST. Deletes those which have zero counts left. Also
recomputes the MMT values, needed by the similarity calculations.
"
(define e (make-elapsed-secs))
(LLOBJ 'clobber)
(define affected-basis (get-affected-basis LLOBJ WRD-LIST))
(define wrd-list (first affected-basis))
(define dj-list (second affected-basis))
(format #t "------ Find affected basis of (~A, ~A) in ~A secs\n"
(length wrd-list) (length dj-list) (e))
(define orphans (recompute-mmt LLOBJ wrd-list dj-list))
(remove-all-empty-sections LLOBJ WRD-LIST)
(define left-marg (first orphans))
(define right-marg (second orphans))
(delete-orphans LLOBJ left-marg right-marg)
(LLOBJ 'clobber)
(recompute-mmt-final LLOBJ)
wrd-list
)
(define*-public (in-group-mi-cluster LLOBJ NRANK LOOP-CNT
#:key
(QUORUM 0.7)
(COMMONALITY 0.2)
(NOISE 4)
(PUSH-FRAMES #t)
(PRECISE-SIM #f))
"
in-group-mi-cluster LLOBJ NRANK LOOP-CNT - grammatical-MI clustering.
Loops over a list of the most similar words, and unifies them into a
cluster. Multiple words are selected at the same time to create a
cluster.  The selection of words is done by selecting an 'in-group'
of words that are all similar to one-another. The selection of
ConnectorSeq's to be merged is done by majority voting to determine
those ConnectorSeq's that the majority of the in-group have in common.
The size of the in-group is adjusted to maximize commonality.
Similarity is judged by means of `ranked-MI`, which is the
grammatical-MI similarity of a pair of words, adjusted by the sqrt of
the frequency, so that more frequent words are ranked higher.
There are three important parameters that determine the operation, and
two more that control the overall loop.
The QUORUM parameter is a floating point number, between 0.0 and 1.0
that determines how many of the in-group members must share a
ConnectorSeq for it to be considered to be held 'in common'.  (Think
of a group of individuals having some trait in common.)
Recommended values for QUORUM are in the 0.4 to 0.9 range. At the
moment, 0.7 seems to work best.
The algo begins by selecting the two words that are deemed to be the
most similar to one-another, as reported by the `add-similarity` API.
These two are the initial members of an 'in-group'. Other similar
words are added as members, to create the largest possible in-group
that is still exclusionary. The members of the in-group must have
large pair-wise similarity.  It must also be exclusive, in that if the
similarity threshold was reduced, membership would become explosively
large.  (From experiments, it can be seen that as the similarity
threshold is lowered, the group stays small, growing slowly, or not
at all. Then there is an inflection point, where the group suddenly
grows explosively large, gaining many members despite a small change
in the similarity threshold. The in-group is selected to be the
largest group below that inflection.)
After the formation of the in-group, a poll is taken to see how many
ConnectorSeq's the group has in common (as controlled by QUORUM,
described above.) The 'commonality' is this fraction. If the
commonality is less than the COMMONALITY parameter, then the size of
the in-group is reduced, by removing the least-similar member, and
a poll is taken again. This continues until either the commonality is
greater than the COMMONALITY parameter, or until the commonality
drops, as compared to the previous group. (The commonality can drop,
because in a smaller group, it can be harder to have a quorum.)
Recommended values for COMMONALITY are in the 0.05 to 0.25 range.
At the moment 0.2 seems to work well. In general, the 'commonality'
is usually very low, and so this fraction is almost enever exceeded.
In other words, this parameter has almost no effect on results.
NOISE is a noise-floor threshold. If a given section has a count equal
or less than the NOISE parameter, it gets no vote in determining the
commonality.  (Think of a group of individuals, one of whom has a
minor quirky trait. One does not wish to have that minor trait to
interfer with the group as a whole, thus it is ignored.)
The NOISE parameter also plays a second role (perhaps it should be
split out into a distinct parameter?) All sections with a count equal
or below the noise floor are unconditionally merged into the cluster.
Recommended value for NOISE is 0 to 4.  Note that, due to Zipfian
distributions, almost all sections have extremely low counts. Thus,
the (vast) majority of merged sections will be those below this noise
floor. In other words, results are sharply dependent on this parameter.
All ConnectorSeq's that have been determined to be held in common by
the in-group are then merged into the cluster. Note that the process
of voting has both a narrowing and a broadening effect. Narrowing, in
that once a group of similar words have been selected, not all
ConSeq's are added to the cluster. The goal of this narrowing is to
explcitly factor out distinct word-senses. Thus, a word like 'saw',
which can be both noun and verb, will have it's noun-like ConSeq's
merged with other nouns, while the verb-like ConSeq's are left behind,
to be merged with other verbs.
This algo also has a broadening effect: By majority vote, once a
ConSeq is accepted into the cluster, all of those words will now share
that ConSeq, even if some of them had not previously. The goal of this
broadening is to generalize from particulars to generalities.
There are three control parameters, NRANK, LOOP-COUNT and PRECISE-SIM.
LOOP-COUNT is the number of times to run the loop, performing a
select-and-merge step each time around.
NRANK is the number of words to rank, before similarity computations
are performed. The words are ranked according to the grand-total
observation count on them, most frequent words first. Then the
pair-wise similarities are computed for the top NRANK words (thus,
a total of NRANK * (NRANK - 1) / 2 similarities are computed.) The
goal here is to avoid having to compute simiarities between all words,
which is computationaly infeasible. Experimentally, it is unlikely
that frequent words are similar to infrequent ones, except in
pathological cases. The word-pair with the highest similarity is then
used to seed the in-group at the start of each loop.
As the loop runs, additional similarities are computed each step. The
number of words with similarity scores on them is kept at NRANK plus
twice the number of loops that have been run. This provides for a
deeper buffer, the rarer the words get. That is, there are many
less-common words that are similar to one-another, and these have
widly-varying rank
to capture these.
Recommended value for NRANK is between 100 and 200.
PRECISE-SIM is an optional parameter
then all similarities between all words affected by the merge, even if
they are affected indirectly, are recomputed. If set to #f, then the
only similarities recomputed are those for the words that were merged.
This recomputation can take up most of the CPU time, and so it defaults
to #f.  It is not yet clear how much this affects results. Probably not
much, or not at all.
Status: This code is complete, fully-debugged, stable, well-tested.
"
(define SIM-API (add-gram-mi-sim-api LLOBJ))
(define MAKE-SIMMER make-gram-mi-simmer)
(define (mi-sim WA WB)
(define miv (SIM-API 'pair-count WA WB))
(if miv (cog-value-ref miv 0) -inf.0))
(define e (make-elapsed-secs))
(define ranked-words (rank-words LLOBJ))
(SIM-API 'fetch-pairs)
(format #t "Done fetching pairs in ~A secs\n" (e))
(define simmer (MAKE-SIMMER LLOBJ))
(define (compute-sim WA WB)
(if (not (SIM-API 'pair-count WA WB)) (simmer WA WB)))
(loop-upper-diagonal compute-sim ranked-words 0 NRANK)
(format #t "Done setting up similarity to ~A in ~A secs\n" NRANK (e))
(define *-log-anchor-* (LLOBJ 'wild-wild))
(cog-set-value! *-log-anchor-* (Predicate "quorum-comm-noise")
(FloatValue QUORUM COMMONALITY NOISE NRANK))
(cog-set-value! *-log-anchor-* (Predicate "in-group-sim")
(StringValue "mi-sim"))
(define log-class (make-class-logger LLOBJ))
(define jaccard-select (make-jaccard-selector LLOBJ
QUORUM COMMONALITY NOISE))
(define merge-majority (make-merge-majority LLOBJ QUORUM NOISE #t))
(define (perform-merge N WA WB)
(define e (make-elapsed-secs))
(format #t "------ Start MI-based merge ~D with seed pair `~A` and `~A`\n"
N (cog-name WA) (cog-name WB))
(define ranked-words (rank-words LLOBJ))
(define n-to-take (inexact->exact
(min (length ranked-words) (+ NRANK (* 3 N)))))
(define words-with-sims (take ranked-words n-to-take))
(define initial-in-grp
(optimal-mi-in-group mi-sim WA WB words-with-sims))
(format #t "Initial in-group size=~D:" (length initial-in-grp))
(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
initial-in-grp)
(format #t "\n")
(define in-grp (jaccard-select initial-in-grp))
(format #t "In-group size=~A:" (length in-grp))
(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD))) in-grp)
(format #t "\n")
(define wclass (make-class-node LLOBJ in-grp))
(merge-majority wclass in-grp)
(format #t "------ Merged into `~A` in ~A secs\n"
(cog-name wclass) (e))
(define touched-words (recompute-marginals LLOBJ (cons wclass in-grp)))
(format #t "------ Recomputed MMT marginals in ~A secs\n" (e))
(define simmer (MAKE-SIMMER LLOBJ))
(if PRECISE-SIM
(recomp-all-sim SIM-API simmer touched-words)
(recomp-all-sim SIM-API simmer (filter cog-atom? in-grp)))
(simmer wclass wclass)
(for-each (lambda (WC) (simmer wclass WC))
(LLOBJ 'get-clusters))
(log-class wclass)
(format #t "------ Recomputed similarity in ~A secs\n" (e))
)
(define (top-ranked-mi-pairs LLOBJ)
(define MI-CUTOFF 4.0)
(define sorted-pairs (get-mi-ranked-pairs LLOBJ MI-CUTOFF))
(format #t "------ Round ~A Next in line:\n"
(+ (get-merge-iteration LLOBJ) 1))
(prt-mi-sorted-pairs LLOBJ sorted-pairs 0 12)
sorted-pairs
)
(define (expand-universe LLOBJ N NRANK)
(define GRO-SIZE 2)
(define (diag-end N) (+ NRANK (* GRO-SIZE N)))
(define ranked-words (rank-words LLOBJ))
(define simmer (MAKE-SIMMER LLOBJ))
(define (compute-sim WA WB)
(if (not (SIM-API 'pair-count WA WB)) (simmer WA WB)))
(loop-upper-diagonal compute-sim ranked-words 0 (diag-end N))
(format #t "------ Extended the universe in ~A secs\n" (e))
)
(main-loop LLOBJ
top-ranked-mi-pairs perform-merge expand-universe
NRANK LOOP-CNT PUSH-FRAMES)
)
#! ========
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(fetch-atom (AnchorNode "data logger"))
(define bat (batch-transpose sha))
(bat 'mmt-marginals)
(define smi (add-similarity-api sha #f "shape-mi"))
(define asm (add-symmetric-mi-compute sha))
(smi 'pair-count (Word "she") (Word "he"))
(smi 'get-count (Similarity (Word "she") (Word "he")))
(in-group-mi-cluster sha 200 100
#:QUORUM 0.7
#:COMMONALITY 0.2
#:NOISE 4)
==== !#