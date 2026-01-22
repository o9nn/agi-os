(use-modules (srfi srfi-1))
(use-modules (opencog nlp learn))
(define (export-mst-parses filename)
"
  Export all the MST-parses that can be found in the
  atomspace to a text file named \"filename\", so that
  it would be easier for people to examine the parses.
  The format is:
  [sentence]
  [word1#] [word1] [word2#] [word2]
"
	(define file-port (open-file filename "a"))
	(define (get-index w)
		(inexact->exact (cog-number (word-inst-get-number w))))
	(define (get-word w)
		(cog-name (word-inst-get-word w)))
	(define (export-parse word-insts)
		(if (not (null? word-insts))
			(display
				(format #f "~a\n"
					(string-join (map get-word (cdr word-insts)) " "))
				file-port))
		(for-each
			(lambda (w)
				(for-each
					(lambda (cs)
						(for-each
							(lambda (c)
								(if (equal? (cog-name (gdr c)) "+")
									(display
										(format #f "~a ~a ~a ~a\n"
											(get-index w)
											(get-word w)
											(get-index (gar c))
											(get-word (gar c)))
										file-port)))
							(cog-outgoing-set cs)))
					(cog-chase-link 'Section 'ConnectorSeq w)))
			word-insts
		)
		(display "\n" file-port)
	)
	(for-each
		(lambda (p)
			(export-parse (parse-get-words-in-order p)))
		(cog-get-atoms 'ParseNode))
	(close-port file-port)
)
(define (observe-mst-extra plain-text)
"
  observe-mst-extra -- update pseduo-disjunct counts by observing raw
  text.
  See also 'observe-mst'.
  In addition to what 'observe-mst' does, extra atoms will be generated
  to preserve the MST parse for each of the inputs.
"
	(define word-strs (tokenize-text plain-text))
	(define word-list (map WordNode word-strs))
	(define word-insts (map (lambda (w) (WordInstanceNode
		(random-node-name 'WordInstanceNode 36 (string-append w "@"))))
			word-strs))
	(define parse-node
		(ParseNode (random-node-name 'ParseNode 36 "mst_parse@")))
	(define word-inst-lks
		(map (lambda (wi) (WordInstanceLink wi parse-node)) word-insts))
	(define reference-lks
		(map (lambda (wi w) (ReferenceLink wi w)) word-insts word-list))
	(define num-nodes '())
	(define word-seq-lks (map
		(lambda (wi)
			(define n (NumberNode (length num-nodes)))
			(set! num-nodes (append num-nodes (list n)))
			(WordSequenceLink wi n))
		word-insts))
	(define mstparse (mst-parse-text plain-text))
	(define sections (make-sections mstparse))
	(define mstparse-insts
		(map (lambda (w)
			(define l-idx (overt-get-index (wedge-get-left-overt w)))
			(define r-idx (overt-get-index (wedge-get-right-overt w)))
			(cons
				(cons
					(cons l-idx (list-ref word-insts (- l-idx 1)))
					(cons r-idx (list-ref word-insts (- r-idx 1)))
				)
				(wedge-get-score w)
			))
			mstparse))
	(define section-insts (make-sections mstparse-insts))
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		sections
	)
	(for-each
		(lambda (x)
			(if (equal? (cog-type x) 'Section)
				(if (not (is-oversize? x)) (store-atom x))
				(store-atom x)))
		(append section-insts word-seq-lks word-inst-lks reference-lks)
	)
	(export-mst-parses "mst-parses.txt")
	(for-each cog-extract-recursive word-insts)
	(for-each cog-extract num-nodes)
	(cog-extract parse-node)
)