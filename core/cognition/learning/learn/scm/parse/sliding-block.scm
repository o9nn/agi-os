(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(define*-public (make-observe-block OBSERVE-TEXT
#:key
(WIN-SIZE 8)
(SPLIT-PRED char-set:whitespace)
(STEP 1)
)
"
make-observe-block OBSERVE-TEXT #:WIN-SIZE 8
Return a function that will call OBSERVE-TEXT on a sliding window
of the given width within a larger block of text.
Given a large block of text, this defines a window, of width
#:WIN-SIZE, that starts at the beginning of the block, and then
slides along, with steps of size #:STEP to the next location. At
each location, the OBSERVE-TEXT function will be called on the
window contents.
The window size is measured with respect to 'words' that, by
default, are separated by whitespace.
The optional parameter #:WIN-SIZE specifies the width of the
sliding block, in units of white-space separated words. The
default is 8.
The optional parameter #:SPLIT-PRED specifies a predicate that
defines the white-space along which blocks will be split. The
default is `char-set:whitespace`.
The optional parameter #:STEP specifies how far the window should
slide by each iteration. Defaults to 1. Setting it to a value greater
than 1 will cause the last few words of the block to possibly remain
uncounted.
Note that there are edge-effects: the first window will be a
full-sized window, starting at the beginning of the block
for the last window. This means that the first few words, and the
last few words, will appear in a smaller number of windows, than
those words in the middle of the block.
"
(define (get-deltas STR DLIST MORE)
(define white (string-index STR SPLIT-PRED))
(define nonwhite
(if white (string-skip STR SPLIT-PRED white) #f))
(define end (if nonwhite nonwhite (string-length STR)))
(define next (- end 1))
(if MORE
(get-deltas (substring STR (+ next 1)) (cons next DLIST) nonwhite)
(reverse! DLIST)))
(define (sumy LST)
(fold (lambda (SUM ITM) (+ SUM ITM 1)) 0 (take LST WIN-SIZE)))
(define (make-full-segments CNT DLIST SEGLIST)
(if (<= WIN-SIZE CNT)
(make-full-segments (- CNT 1) (cdr DLIST) (cons (sumy DLIST) SEGLIST))
(reverse! SEGLIST)))
(define (make-segments DLIST)
(define dlen (length DLIST))
(if (<= WIN-SIZE dlen)
(make-full-segments dlen DLIST '())
(list (fold (lambda (SUM ITM) (+ SUM ITM 1)) 0 DLIST))))
(define (make-starts DLIST SUM STARTL)
(if (not (nil? DLIST))
(make-starts (cdr DLIST) (+ 1 SUM (car DLIST)) (cons SUM STARTL))
(reverse! STARTL)))
(define nblocks (Anchor "Num blocks"))
(define slides (Anchor "Slides"))
(define eslides (Anchor "Expected Slides"))
(define (observe-block TEXT-BLOCK)
(define delta-list (get-deltas TEXT-BLOCK '() #t))
(define seg-list (make-segments delta-list))
(define start-list (make-starts delta-list 0 '()))
(count-one-atom nblocks)
(count-inc-atom eslides (length seg-list))
(define cnt 0)
(for-each
(lambda (START LEN)
(define text-seg (substring TEXT-BLOCK START (+ START LEN)))
(when (eq? 0 (modulo cnt STEP))
(OBSERVE-TEXT text-seg)
(count-one-atom slides)
)
(set! cnt (+ cnt 1)))
start-list seg-list))
observe-block
)