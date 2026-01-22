(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))
(define-public (prt-word W)
(define t (cog-type W))
(cond
((equal? t 'WordClassNode) (format #f "{~A}" (cog-name W)))
((equal? t 'WordNode) (cog-name W))
((equal? t 'VariableNode) "$")
)
)
(define (prt-conseq LST)
(string-concatenate
(map (lambda (CON)
(format #f " ~A~A" (prt-word (gar CON)) (cog-name (gdr CON))))
LST))
)
(define (prt-shape SHAPE)
(format #f "<~A, ~A>"
(prt-word (gar SHAPE))
(prt-conseq (cdr (cog-outgoing-set SHAPE))))
)
(define-public (prt-dj DJ)
(if (equal? (cog-type DJ) 'ShapeLink)
(prt-shape DJ)
(prt-conseq (cog-outgoing-set DJ)))
)
(define-public (prt-dj-list LST)
(string-concatenate
(map (lambda (ELT)
(format #f "~A\n" (prt-dj ELT)))
LST))
)
(define (prt-section SECT)
(format #f "~6,3F * (~A, ~A)"
(cog-count SECT)
(prt-word (gar SECT))
(prt-conseq (cog-outgoing-set (gdr SECT))))
)
(define (prt-cross-section XSECT)
(format #f "~6,3F * [~A, ~A]"
(cog-count XSECT)
(prt-word (gar XSECT))
(prt-shape (gdr XSECT)))
)
(define-public (prt-element ELT)
(if (equal? (cog-type ELT) 'Section)
(prt-section ELT)
(prt-cross-section ELT))
)
(define-public (prt-element-list LST)
(string-concatenate
(map (lambda (ELT)
(format #f "~A\n" (prt-element ELT)))
LST))
)
(define (rebalance-count LLOBJ SECTION CNT)
"
rebalance-count LLOBJ SECTION CNT - set count on section and crosses.
The SECTION is presumed to be some section on which the observation
count was adjusted (possibly even set to zero.) This function
enforces 'detailed balance', making sure that the CrossSections
corresponding to SECTION have the same count.
If the count isn't zero, then the SECTION is store to the database.
If it is zero, then it's likely that a later stage will delete it,
so a pointless store is avoided.
"
(define (is-zero? cnt) (< cnt 1.0e-10))
(set-count SECTION CNT)
(if (not (is-zero? CNT)) (store-atom SECTION))
(for-each
(lambda (XST) (set-count XST CNT))
(LLOBJ 'make-cross-sections SECTION))
)
(define (rebalance-merge LLOBJ MRG DONOR)
"
rebalance-merge LLOBJ MRG DONOR - Readjust counts on CrossSections
After a DONOR section has been merged into the MRG section, assorted
CrossSections may be left in inconsistent states. This rebalances all
counts on both Sections and CrossSections.
FRAC is ignored
"
(define is-sect (equal? 'Section (cog-type DONOR)))
(define mrg (if is-sect MRG (LLOBJ 'make-section MRG)))
(define don (if is-sect DONOR (LLOBJ 'make-section DONOR)))
(LLOBJ 'make-cross-sections mrg)
(rebalance-count LLOBJ mrg (LLOBJ 'get-count MRG))
(rebalance-count LLOBJ don (LLOBJ 'get-count DONOR))
)
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
(rebalance-merge LLOBJ ACC DONOR)
)
moved
)
(define (remove-empty-sections LLOBJ ROW RMX)
"
remove-empty-sections LLOBJ ROW RMX -- scan the ROW for Sections and
call cog-delete! on those that have an zero count. If RMX is #t, then
the corresponding CrossSections will also be deleted.
"
(define ns 0)
(define nx 0)
(define (is-zero? cnt) (< cnt 1.0e-10))
(define (del-sect SEC)
(when RMX
(for-each (lambda (xst)
(define shp (LLOBJ 'right-element xst))
(cog-delete! xst)
(cog-delete! shp)
(set! nx (+ 1 nx)))
(LLOBJ 'get-cross-sections SEC)))
(define csq (LLOBJ 'right-element SEC))
(cog-delete! SEC)
(cog-delete! csq)
(set! ns (+ 1 ns)))
(define (del-xes XST)
(define sect (LLOBJ 'get-section XST))
(if (not (nil? sect)) (del-sect sect))
(set! nx (+ 1 nx)))
(for-each
(lambda (ITEM)
(if (and (cog-atom? ITEM) (is-zero? (LLOBJ 'get-count ITEM)))
(cond
((eq? 'Section (cog-type ITEM)) (del-sect ITEM))
((eq? 'CrossSection (cog-type ITEM)) (del-xes ITEM))
(else
(throw 'assert 'remove-empty-sections "Its broken")))
))
(LLOBJ 'right-stars ROW))
(for-each cog-delete! (cog-incoming-by-type ROW 'Connector))
)
(define-public (remove-all-empty-sections LLOBJ WRD-LIST)
"
remove-all-empty-sections LLOBJ WRD-LIST -- Cleanup after merging.
Remove all Sections and CrossSections with a zero count.
"
(define MRG-CON #t)
(for-each
(lambda (WRD) (remove-empty-sections LLOBJ WRD MRG-CON))
WRD-LIST)
(LLOBJ 'clobber)
)