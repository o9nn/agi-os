(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (opencog matrix))
(use-modules (opencog sheaf))
(define-public (tokenize-text plain-text)
"
  tokenize-text plain-text -- split sentence into words.
  Tokenize the text: take the input sentence (as a UTF-8 encoded string),
  and return a list of the words in the sentence (as a list of strings).
  It is assumed that words are always separated by white-space, so this
  is easy. The tokenizer also makes a vague attempt to also separate
  punctuation, although it is not terribly robust in doing so. It does
  make a limited attempt to split words with certain infixed
  punctuation, such as double-dashes and long dashes.
  This is not terribly rigorous
  selection of oddball unicode punctuation marks as prefixes and
  suffixes. This list is not complete nor well-organized
  it is built up from experience of parsing assorted texts and noting
  the kinds of stuff that actually gets used. Its slanted towards
  European languages, and may be inadequate for other languages.
  I did not want to get too fancy here
  most ordinary text, for now.  A fancier treatment must await
  generalized handling of morphology, at which point, we can treat
  any kind of affixes, and not just punctuation.  So its kind of
  pointless to try to replace the code below by something better,
  unless the better thing is full morphology support.
  See also: `split-text`, which tokenizes only according to whitespace,
     and completely ignores punctuation.
"
	(define prefix "({[<«〈（〔《【［『「``„“‘'''\"…..._-‐‑‒–—―¿¡$£₤€¤₳฿₡₢₠₫৳ƒ₣₲₴₭₺ℳ₥₦₧₱₰₹₨₪﷼₸₮₩¥៛호점†‡§¶©®℗№#")
	(define suffix ")}]>»〉）〕》】］』」’'\"%,.。:
	(define infix "-‐‑‒–—―…()[]{}")
	(define prefix-list (string->list prefix))
	(define suffix-list (string->list suffix))
	(define infix-list (string->list infix))
	(define (strip-prefli word prefli)
		(if (null? prefli)
			(list word)
			(let* ((punct (car prefli))
					(head (string-ref word 0)))
				(if (eq? punct head)
					(append
						(list (string punct))
						(strip-prefix (substring word 1))
					)
					(strip-prefli word (cdr prefli))
				))))
	(define (strip-prefix word)
		(if (< 0 (string-length word))
			(strip-prefli word prefix-list)
			'()))
	(define (strip-sufli word sufli)
		(if (null? sufli)
			(strip-prefix word)
			(let* ((punct (car sufli))
					(len (- (string-length word) 1))
					(tail (string-ref word len)))
				(if (eq? punct tail)
					(append
						(strip-affix (substring word 0 len))
						(list (string punct))
					)
					(strip-sufli word (cdr sufli))
				))))
	(define (strip-affix word)
		(if (< 0 (string-length word))
			(strip-sufli word suffix-list)
			'()))
	(define (pad-a-dash str infx start)
		(define idx (string-index str infx start))
		(if idx
			(let ((idp1 (+ idx 1)))
				(pad-a-dash
					(string-replace str " " idx idx)
					infx
					(+ idx 2)))
			str))
	(define (pad-dash str ifx-list)
		(if (null? ifx-list) str
			(pad-dash (pad-a-dash str (car ifx-list) 0) (cdr ifx-list))))
	(define (remerge tkl buff punct rslt)
		(if (null? tkl)
			(if (< 0 (string-length buff)) (cons buff rslt) rslt)
			(if (string=? (car tkl) punct)
				(remerge (cdr tkl) (string-append buff punct) punct rslt)
				(if (< 0 (string-length buff))
					(remerge tkl "" punct (cons buff rslt))
					(remerge (cdr tkl) "" punct (cons (car tkl) rslt))))))
	(define (remerge-dot-dash tkl)
		(remerge (remerge tkl "" "." '()) "" "-" '()))
	(define left-wall "###LEFT-WALL###")
	(let* ((pad-text (pad-dash plain-text infix-list))
			(word-list (string-split pad-text #\ ))
			(strip-list (map strip-affix word-list))
			(tok-list (concatenate (cons (list left-wall) strip-list)))
			(merge-list (remerge-dot-dash tok-list))
		)
		merge-list
	)
)
(define (parse-setup-tool parser plain-text)
"
  Handy dandy utility to avoid excess cut-n-paste for
  customization.
"
	(define word-strs (tokenize-text plain-text))
	(define word-list (map WordNode word-strs))
	(define pair-obj (make-any-link-api))
	(define mi-source (add-pair-freq-api pair-obj))
	(define scorer (make-score-fn mi-source 'pair-fmi))
	(define (ramp-scorer LW RW LEN)
		(define MAXLEN 16)
		(define RAMPLEN 8)
		(define mplu1 (+ MAXLEN 1))
		(define (ramp len) (/ (- mplu1 len) (- mplu1 RAMPLEN)))
		(if (< MAXLEN LEN) -2e25
			(let ((sco (scorer LW RW LEN)))
				(if (or (< LEN RAMPLEN) (< sco 0))
					sco
					(* sco (ramp LEN))))))
	(parser word-list ramp-scorer)
)
(define-public (mst-parse-text plain-text)
"
  mst-parse-text -- Maximum Spanning Tree parser.
  Given a raw-text sentence, it splits apart the sentence into distinct
  words, and finds an (unlabelled) dependency parse of the sentence, by
  finding a dependency tree that maximizes the mutual information.
  Returns a list of word-pairs, together with the associated mutual
  information.
"
	(parse-setup-tool mst-parse-atom-seq plain-text)
)
(define-public (mpg-parse-text plain-text)
"
  mpg-parse-text -- Maximum Planar Graph parser.
  Given a raw-text sentence, it splits apart the sentence into distinct
  words, and finds an (unlabelled) dependency parse of the sentence, by
  finding a dependency graph that maximizes the mutual information,
  and maximizes the number of edges while keeping the graph planar.
  Returns a list of word-pairs, together with the associated mutual
  information.
"
	(define (mpg-linear ATOM-LIST SCORE-FN)
		(define numa-list (atom-list->numa-list ATOM-LIST))
		(define mst-tree (graph-add-mst '() numa-list SCORE-FN -1))
		(define mpgraph (graph-add-mpg mst-tree numa-list SCORE-FN -1))
		(define disco (graph-add-linear mpgraph numa-list))
		(if (< 30 (length numa-list))
			disco
			(graph-add-bridges disco))
	)
	(parse-setup-tool mpg-linear plain-text)
)
(define (is-oversize? SECTION)
	(< 330 (cog-arity (gdr SECTION)))
)
(define-public (observe-mst plain-text)
"
  observe-mst -- update pseduo-disjunct counts by observing raw text.
  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up. Uses the MST parser to obtain
  a spanning tree parse.
"
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections (mst-parse-text plain-text))
	)
)
(define-public (observe-mpg plain-text)
"
  observe-mpg -- update pseduo-disjunct counts by observing raw text.
  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up. Uses the MPG parser to obtain
  the maximal planar graph.
"
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections (mpg-parse-text plain-text))
	)
)