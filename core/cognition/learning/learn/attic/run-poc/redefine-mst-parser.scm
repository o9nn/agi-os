(define (make-distance-scorer scorer DIST-MULTS)
	(lambda (LW RW LEN)
		(define multiplier (list-ref DIST-MULTS (- (min LEN (length DIST-MULTS)) 1)))
		(if (< 16 LEN) -2e25 (* multiplier (scorer LW RW LEN)))
	)
)
(define-public (mst-parse-text-file plain-textblock DIST-MULT)
"
	Procedure to MST-parse sentences coming from an instance-pair weight file.
"
	(define split-textblock
		(string-split plain-textblock #\newline)
	)
	(define current-sentence 
		(string-append "###LEFT-WALL### " (car split-textblock))
	)
	(define (word-strs text-line)
		(string-split text-line #\ )
	)
	(define weights-array
		(let* ((array-dim (length (word-strs current-sentence)))
			(tmp-array (make-array -1e40 array-dim array-dim))
			)
			(for-each
				(lambda (weightline)
					(define split-weightline (word-strs weightline))
					(define i1 (string->number (list-ref split-weightline 0)))
					(define i2 (string->number (list-ref split-weightline 2)))
					(define weight (string->number (list-ref split-weightline 4)))
					(array-set! tmp-array weight i1 i2)
				)
				(cdr split-textblock)
			)
			tmp-array
		)
	)
	(define (word-list list-of-words)
		(define cnt -1)
		(map 
			(lambda (str) 
				(set! cnt (+ cnt 1))
				(WordSequenceLink
					(WordNode str)
					(NumberNode cnt)
				)
			)
			list-of-words
		)
	)	
	(define scorer 
		(lambda (left-atom right-atom distance)
			(define left-index (inexact->exact (string->number (cog-name (gdr left-atom)))))
			(define right-index (inexact->exact (string->number (cog-name (gdr right-atom)))))
			(array-ref weights-array left-index right-index)
		)
	)
	(define dist-scorer (make-distance-scorer scorer DIST-MULT))
	(mst-parse-atom-seq (word-list (word-strs current-sentence)) dist-scorer)
)
(define-public (mst-parse-text-mode plain-text cnt-mode DIST-MULT)
	(define word-strs (cons '"###LEFT-WALL###" (string-split plain-text #\ ))
	)
	(define word-list (map (lambda (str) (WordNode str)) word-strs))
	(define pair-obj
		(cond
			((or (equal? cnt-mode "clique")
				 (equal? cnt-mode "clique-dist"))
					(make-clique-pair-api))
			(else (make-any-link-api))))
	(define mi-source (add-pair-freq-api pair-obj))
	(define scorer (make-score-fn mi-source 'pair-fmi))
	(define dist-scorer (make-distance-scorer scorer DIST-MULT))
	(mst-parse-atom-seq word-list dist-scorer)
)
(define-public (mst-parse-text plain-text)
	(mst-parse-text-mode plain-text "any" #f))
(define (export-mst-parse plain-text mstparse filename)
"
  Export an MST-parse to a text file named filename,
  so that parses can be examined.
  The format is:
  [sentence]
  [word1#] [word1] [word2#] [word2]
  [word2#] [word2] [word4#] [word4]
  ...
"
	(define file-port (open-file filename "a"))
	(define (get-mi link) (cdr link))
	(define (get-lindex link)
		(- (car (car (car link))) 1))
	(define (get-rindex link)
		(- (car (cdr (car link))) 1))
	(define (get-lword link)
		(let ((atom (cdr (car (car link)))))
			(if (cog-link? atom)
				(cog-name (gar atom))
				(cog-name atom)
			)
		)
	)
	(define (get-rword link)
		(let ((atom (cdr (cdr (car link)))))
			(if (cog-link? atom)
				(cog-name (gar atom))
				(cog-name atom)
			)
		)
	)
	(define link-comparator
		(lambda (l1 l2)
			(< (get-lindex l1) (get-lindex l2))))
	(if (not (null? plain-text))
		(display
			(format #f "~a\n"
				plain-text)
			file-port))
	(for-each
		(lambda (l)
			(if (> (get-mi l) -1.0e10)
				(display
					(format #f "~a ~a ~a ~a ~a\n"
						(get-lindex l)
						(get-lword l)
						(get-rindex l)
						(get-rword l)
						(get-mi l))
				file-port)))
		(sort mstparse link-comparator)
	)
	(display "\n" file-port)
	(close-port file-port)
)
(define-public (observe-mst-mode plain-text CNT-MODE MST-DIST EXPORT-MST)
"
  observe-mst-mode -- update pseduo-disjunct counts by observing raw text.
  Build mst-parses using MI calculated beforehand.
  Values in MST-DIST adjust word-pair weight values for distance.
  Obtained parses are exported to file if EXPORT-MST is true.
  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up.
"
	(define file-cnt-mode (if (equal? CNT-MODE "file") #t #f))
	(define parse 
		(if file-cnt-mode
			(mst-parse-text-file plain-text MST-DIST)
			(mst-parse-text-mode plain-text CNT-MODE MST-DIST)
		)
	)
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections parse)
	)
	(if (not (equal? EXPORT-MST "NONE"))
		(if file-cnt-mode
			(export-mst-parse (car (string-split plain-text #\newline)) parse EXPORT-MST)
			(export-mst-parse plain-text parse EXPORT-MST)
		)
	)
	parse
)
(define-public (observe-mst plain-text)
	(observe-mst-mode plain-text "any" #f "NONE")
)