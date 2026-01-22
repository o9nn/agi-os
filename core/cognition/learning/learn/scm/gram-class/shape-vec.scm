(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(define (not-is-member? WRD CLS)
(nil? (cog-link 'MemberLink WRD CLS))
)
(define-public (add-shape-vec-api LLOBJ)
"
add-shape-vec-api -- Provide API for CrossSections (word-shape pairs
that correspond to Sections).  Assumes that LLOBJ provides an API
that gives access to Sections.
A CrossSection has the following form:
(CrossSection
germ  <-- this is a WordNode or a WordClassNode
(Shape
point  <-- this is a WordNode or a WordClassNode
(ConnectorLink ...)
(ConnectorLink ...)
...))
A more detailed description is given in the `shape-vec.scm` file.
In addition to the usual methods, this class also provides a
collection of methods that are used to merge Sections and
CrossSections by replacing specific Words by WordClasses both
in the germ, and in the Connectors.  These are described below.
'make-section CROSS  -- Create and return the section that corresponds
to the CrossSection CROSS.
'get-section CROSS  -- Return the section that corresponds to the
CrossSection CROSS, if it exists.
'get-cross-sections SECT -- Return all of the CrossSections that
cover the Section SECT. This returns only those cross-sections
that are already in the AtomSpace
'explode-sections -- create all possible CrossSections that correspond
to existing Sections (on LLOBJ).  The count on each cross-section
will be set to the count on the section. (This is the correct
way to handle counts, if one wants clustering to commute with
the creation of sections.)
'implode-sections -- remove CrossSections so that they no longer appear
in the basis.
'make-flat CLS PNT -- Rewrite PNT, replacing occurances of any atoms
belonging to CLS by CLS.
'flatten CLS PNT -- Rewrite PNT, replacing occurances of any atoms
belonging to CLS by CLS.  If PNT is already flat, (i.e. if there
is nothing to be rewritten), then return #f. This just calls
one of the two methods below, basecd on the type of PNT.
'flatten-section CLS SECT -- Rewrite SECT, replacing the germ by CLS,
and also any connectors that belong to CLS by the corresponding
connector for CLS. If no connectors belong to CLS, then return #f.
'flatten-cross CLS CROSS -- Rewrite CROSS, replacing the germ by CLS.
If the 'point' belong to CLS, it is replaced by CLS. If any
connectors belong to CLS, they are replaced by the corresponding
connector for CLS. If neither the point, nor the connectors belong
to CLS, then return #f.
"
(let ((l-basis #f)
(r-basis #f)
(l-size 0)
(r-size 0)
)
(define star-wild (Variable "$connector-word"))
(define any-left (AnyNode "cross word"))
(define any-right (AnyNode "cross shape"))
(define (get-right-type)
(TypeChoice (LLOBJ 'right-type) (Type 'ShapeLink)))
(define (get-pair-type)
(TypeChoice (LLOBJ 'pair-type) (Type 'CrossSection)))
(define (get-count SHAPE-PR) (cog-count SHAPE-PR))
(define (set-count SHAPE-PR CNT)
(cog-set-tv! SHAPE-PR (CountTruthValue 1 0 CNT)))
(define (get-pair L-ATOM R-ATOM)
(cog-link 'CrossSection L-ATOM R-ATOM))
(define (make-pair L-ATOM R-ATOM)
(CrossSection L-ATOM R-ATOM))
(define (get-pair-left SHAPE-PR)
(cog-outgoing-atom SHAPE-PR 0))
(define (get-pair-right SHAPE-PR)
(cog-outgoing-atom SHAPE-PR 1))
(define (analyze-xsection XSECT)
(define SHAPE-PR (cog-outgoing-set XSECT))
(define GERM (first SHAPE-PR))
(define SHAPE (second SHAPE-PR))
(define tmpl (cog-outgoing-set SHAPE))
(define point (car tmpl))
(define conseq (cdr tmpl))
(define (not-var? ITEM) (not (cog-equal? (gar ITEM) star-wild)))
(define begn (take-while not-var? conseq))
(define rest (drop-while not-var? conseq))
(define dir (gdr (car rest)))
(define end (cdr rest))
(list GERM dir begn end point))
(define (make-section XSECT)
(define parts (analyze-xsection XSECT))
(define GERM  (list-ref parts 0))
(define dir   (list-ref parts 1))
(define begn  (list-ref parts 2))
(define end   (list-ref parts 3))
(define point (list-ref parts 4))
(define ctcr (Connector GERM dir))
(define cseq (ConnectorSeq begn ctcr end))
(LLOBJ 'make-pair point cseq))
(define (get-section XSECT)
(define parts (analyze-xsection XSECT))
(define GERM  (list-ref parts 0))
(define dir   (list-ref parts 1))
(define begn  (list-ref parts 2))
(define end   (list-ref parts 3))
(define point (list-ref parts 4))
(define ctcr (cog-link 'Connector GERM dir))
(define cseq (if (nil? ctcr) '()
(cog-link 'ConnectorSeq begn ctcr end)))
(if (nil? cseq) '()
(LLOBJ 'get-pair point cseq)))
(define (re-cross GLS XSECT)
(define SHAPE-PR (cog-outgoing-set XSECT))
(define GERM (first SHAPE-PR))
(define SHAPE (second SHAPE-PR))
(define tmpl (cog-outgoing-set SHAPE))
(define conseq (cdr tmpl))
(CrossSection GERM (Shape GLS conseq)))
(define (flatten-section CLS SECT)
(define conseq (cog-outgoing-set (get-pair-right SECT)))
(define non-flat #f)
(define newseq
(map (lambda (con)
(define clist (cog-outgoing-set con))
(if (not-is-member? (car clist) CLS)
con
(begin (set! non-flat #t)
(Connector CLS (cdr clist)))))
conseq))
(define germ (get-pair-left SECT))
(define newgerm
(if (not-is-member? germ CLS) germ
(begin (set! non-flat #t) CLS)))
(if non-flat (LLOBJ 'make-pair newgerm (ConnectorSeq newseq)) #f))
(define (flatten-cross CLS XSECT)
(define SHAPE-PR (cog-outgoing-set XSECT))
(define germ (first SHAPE-PR))
(define SHAPE (second SHAPE-PR))
(define tmpl (cog-outgoing-set SHAPE))
(define point (car tmpl))
(define conseq (cdr tmpl))
(define non-flat #f)
(define newpoint
(if (not-is-member? point CLS) point
(begin (set! non-flat #t) CLS)))
(define newseq
(map (lambda (con)
(define clist (cog-outgoing-set con))
(if (not-is-member? (car clist) CLS)
con
(begin (set! non-flat #t)
(Connector CLS (cdr clist)))))
conseq))
(define newgerm
(if (not-is-member? germ CLS) germ
(begin (set! non-flat #t) CLS)))
(if non-flat (CrossSection newgerm
(Shape newpoint newseq)) #f))
(define (flatten CLS PNT)
(if (equal? 'Section (cog-type PNT))
(flatten-section CLS PNT)
(flatten-cross CLS PNT)))
(define (make-flat CLS PNT)
(define flat (flatten CLS PNT))
(if flat flat
(let* ((germ (get-pair-left PNT))
(newgerm
(if (not-is-member? germ CLS) germ CLS))
(DJ (get-pair-right PNT)))
(if (equal? (cog-type PNT) 'CrossSection)
(CrossSection newgerm DJ)
(LLOBJ 'make-pair newgerm DJ)))))
(define (is-nonflat-section? CLS SECT)
(any
(lambda (con) (cog-equal? (gar con) CLS))
(cog-outgoing-set (get-pair-right SECT))))
(define (get-pair-count L-ATOM R-ATOM)
(define sect (get-pair L-ATOM R-ATOM))
(if (null? sect) 0 (get-count sect)))
(define (get-right-wildcard WORD)
(ListLink WORD any-right))
(define (get-left-wildcard R-ATOM) R-ATOM)
(define (get-wild-wild)
(ListLink any-left any-right))
(define (get-right-basis)
(if (not r-basis) (set! r-basis (cog-get-atoms 'ShapeLink)))
r-basis)
(define (get-right-size)
(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
r-size)
(define (clobber)
(set! r-basis #f)
(set! r-size 0)
(if (LLOBJ 'provides 'clobber) (LLOBJ 'clobber))
)
(define (get-cross-sections SEC)
(define point (gar SEC))
(define cncts (cog-outgoing-set (gdr SEC)))
(define num-cncts (length cncts))
(define (insert-wild N)
(define front (take cncts N))
(define back (drop cncts N))
(define ctr (car back))
(define wrd (gar ctr))
(define dir (gdr ctr))
(define wild (Connector star-wild dir))
(define shape
(cog-link 'Shape point front wild (cdr back)))
(if (nil? shape) #f
(let ((cross (cog-link 'CrossSection wrd shape)))
(if (nil? cross) #f cross))))
(filter-map insert-wild (iota num-cncts))
)
(define (make-cross-sections SEC)
(define point (gar SEC))
(define cncts (cog-outgoing-set (gdr SEC)))
(define num-cncts (length cncts))
(define (insert-wild N)
(define front (take cncts N))
(define back (drop cncts N))
(define ctr (car back))
(define wrd (gar ctr))
(define dir (gdr ctr))
(define wild (Connector star-wild dir))
(define shape (Shape point front wild (cdr back)))
(CrossSection wrd shape))
(map insert-wild (iota num-cncts))
)
(define (explode-sections)
(define (explode-section SEC)
(define weight (get-count SEC))
(define (copy-weight XES) (set-count XES weight))
(for-each copy-weight (make-cross-sections SEC))
)
(define start-time (current-time))
(for-each explode-section (LLOBJ 'get-all-elts))
(clobber)
(format #t "Elapsed time to create shapes: ~A secs\n"
(- (current-time) start-time))
)
(define (implode-sections)
(define (extract-cross PNT)
(when (equal? 'CrossSection (cog-type PNT))
(cog-extract (gdr PNT))
(cog-extract! PNT)))
(for-each cog-extract-recursive! (cog-incoming-set star-wild))
(clobber)
)
(define (fetch-sections)
(define start-time (current-time))
(fetch-incoming-set any-left)
(fetch-incoming-set any-right)
(load-atoms-of-type 'Shape)
(load-atoms-of-type 'CrossSection)
(format #t "Elapsed time to load cross-sections: ~A seconds\n"
(- (current-time) start-time))
)
(define (describe)
(display (procedure-property add-shape-vec-api 'documentation)))
(define (provides meth)
(case meth
((right-basis)      get-right-basis)
((right-basis-size) get-right-size)
((provides)         provides)
((clobber)          clobber)
((flatten)          flatten)
((pair-count)       get-pair-count)
((get-pair)         get-pair)
((get-count)        get-count)
((make-pair)        make-pair)
((left-element)     get-pair-left)
((right-element)    get-pair-right)
(else #f)
))
(lambda (message . args)
(apply (case message
((name)       (lambda () "Cross-section Words"))
((id)         (lambda () "cross-section"))
((right-type)       get-right-type)
((pair-type)        get-pair-type)
((pair-count)       get-pair-count)
((get-pair)         get-pair)
((get-count)        get-count)
((make-pair)        make-pair)
((left-element)     get-pair-left)
((right-element)    get-pair-right)
((left-wildcard)    get-left-wildcard)
((right-wildcard)   get-right-wildcard)
((wild-wild)        get-wild-wild)
((fetch-pairs)      fetch-sections)
((right-basis)      get-right-basis)
((right-basis-size) get-right-size)
((clobber)          clobber)
((explode-sections) explode-sections)
((implode-sections) implode-sections)
((make-section)     make-section)
((get-section)      get-section)
((make-cross-sections) make-cross-sections)
((get-cross-sections)  get-cross-sections)
((re-cross)         re-cross)
((make-flat)        make-flat)
((flatten)          flatten)
((flatten-section)  flatten-section)
((flatten-cross)    flatten-cross)
((is-nonflat?)      is-nonflat-section?)
((provides)         provides)
((filters?)         (lambda () #f))
((describe)         describe)
((help)             describe)
((obj)              (lambda () "add-shape-vec-api"))
((base)             (lambda () LLOBJ))
(else               (lambda ( . rest )
(apply LLOBJ (cons message args)))))
args))
))
(define-public (add-covering-sections LLOBJ)
"
add-covering-sections LLOBJ -- Direct sum of Sections and CrossSections
This object accepts an LLOBJ that exposes vectors of Sections, and
provides an API for vectors of Sections-oplus-CrossSections, i.e. for
the direct sum of these two.  Thus, any given vector will have basis
elements taken from both. The CrossSections are provided by the
`add-shape-vec-api` object.
This is done in order to get a self-consistent view into the
(word,disjunct) pair matrix, when words are being clustered into
clusters. The issue is that words appear not only on the left, but
also within Connectors in the disjunct. During clustering, the
Connectors need to be merged, and the disjuncts updated. The
CrossSections provide an \"almost-linear\" API that helps perform
this task. That is, merging is inherently non-linear, and, in some
cases, non-cummutative. The CrossSections and Shapes help keep things
almost linear as far into the process as possible.
Since the whole point of this object is to support merging, it will
wrap the LLOBJ with the `add-gram-class-api` object.
See docs for `add-shape-vec-api` for more info about CrossSections
and Shapes. See `direct-sum` for more info about the direct sum.
See `add-gram-class-api` for more about grammatical classes.
"
(define gram-obj (add-gram-class-api LLOBJ))
(define stars-obj (add-pair-stars gram-obj))
(define shape-obj (add-shape-vec-api stars-obj))
(define shape-stars (add-pair-stars shape-obj))
(define cover-obj (direct-sum stars-obj shape-stars))
(define cover-stars (add-pair-stars cover-obj))
(define (explode-sections)
(shape-obj 'explode-sections)
(cover-stars 'clobber))
(define (implode-sections)
(shape-obj 'implode-sections)
(cover-stars 'clobber))
(define (describe)
(display (procedure-property add-gram-class-api 'documentation)))
(define (flatten CLS PNT)
(shape-obj 'flatten CLS PNT))
(define (provides meth)
(case meth
((provides)         provides)
((flatten)          flatten)
((cover-base)       (lambda () LLOBJ))
(else               (cover-stars 'provides meth))
))
(lambda (message . args)
(case message
((name)                "Covering Sections for Words")
((id)                  "cover-section")
((fetch-pairs)         (cover-obj 'fetch-pairs))
((explode-sections)    (explode-sections))
((implode-sections)    (implode-sections))
((make-section)        (apply shape-obj (cons message args)))
((get-section)         (apply shape-obj (cons message args)))
((make-cross-sections) (apply shape-obj (cons message args)))
((get-cross-sections)  (apply shape-obj (cons message args)))
((re-cross)            (apply shape-obj (cons message args)))
((make-flat)           (apply shape-obj (cons message args)))
((flatten)             (apply shape-obj (cons message args)))
((is-nonflat?)         (apply shape-obj (cons message args)))
((cluster-type)        (apply gram-obj (cons message args)))
((make-cluster)        (apply gram-obj (cons message args)))
((get-clusters)        (apply gram-obj (cons message args)))
((store-aux)           (apply gram-obj (cons message args)))
((provides)            (apply provides args))
((describe)            describe)
((help)                describe)
((obj)                 "add-covering-sections")
((base)                LLOBJ)
((cover-base)          LLOBJ)
(else             (apply cover-stars (cons message args)))))
)