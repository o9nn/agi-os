(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog exec) (opencog nlp))
(use-modules (opencog nlp lg-dict) (opencog nlp lg-parse))
(define*-public (make-lg-comparator gold-dict test-dict classes #:key
     (INCLUDE-MISSING #f)
     (VERBOSE #f)
)
"
  make-lg-comparator GOLD-DICT OTHER-DICT CLASSES - Return a sentence
  comparison function.
  GOLD-DICT and OTHER-DICT should be the two LgDictNodes to compare.
  The code assumes that GOLD-DICT is the reference or \"golden\" lexis
  to compare to, so that any differences found in OTHER-DICT are blamed
  on OTHER-DICT.
  CLASSES is a list of lists of link types in the GOLD-DICT for which
  statistics should be kept. The idea is that some link types in the
  golden dict are more important to get right than others, and so stats
  should be kept just for those.  CLASSES may be an empty list, in which
  case only overall stats are kept.
  This returns a comparison function.  To use it, pass one or more
  sentence strings to it
  When finished, pass it `#f`, and it will print a summary report.
  Example usage:
     (define compare (make-lg-comparator
        (LgDictNode \"en\") (LgDictNode \"micro-fuzz\")
        (list (list \"S\" \"O\" \"MV\") (list \"X\")) ))
     (compare \"I saw her face\")
     (compare \"I swooned to the floor\")
     (compare #f)
  By default, this does not test sentences that have words that are
  not in the test dictionary.  Over-ride this by supplying the optional
  argument INCLUDE-MISSING.
"
	(let* ((verbose VERBOSE)
			(total-sentences 0)
			(total-compares 0)
			(incomplete-dict 0)
			(temp-cnt 0)
			(bad-sentences 0)
			(total-words 0)
			(total-links 0)
			(length-miscompares 0)
			(word-miscompares 0)
			(link-count-miscompares 0)
			(link-correct 0)
			(link-excess 0)
			(link-deficit 0)
			(nclasses (length classes))
			(present (make-vector (+ 1 nclasses) 0))
			(missing (make-vector (+ 1 nclasses) 0))
			(missing-link-types '())
			(missing-words (make-atom-set))
			(vocab-words (make-atom-set))
		)
		(define (get-word-of-winst WRD)
			(gdr (car (cog-incoming-by-type WRD 'ReferenceLink))))
		(define (get-index-of-winst WRD)
			(gdr (car (cog-incoming-by-type WRD 'WordSequenceLink))))
		(define (sort-word-inst-list LST)
			(sort LST
				(lambda (wa wb)
					(define (get-num wi)
						(string->number (cog-name (get-index-of-winst wi))))
					(< (get-num wa) (get-num wb)))))
		(define (get-linked-winst WIN)
			(sort-word-inst-list
				(map gdr
					(filter
						(lambda (lili)
							(and
								(equal? (gar lili) WIN)
								(not (equal? (cog-name
									(get-word-of-winst (gdr lili))) "###RIGHT-WALL###"))
								(any
									(lambda (evlnk)
										(equal? 'LgLinkNode
											(cog-type (gar evlnk))))
									(cog-incoming-by-type lili 'EvaluationLink))
							))
						(cog-incoming-by-type WIN 'ListLink)))))
		(define (get-link-name lwin rwin)
			(gar (car (filter
				(lambda (evl)
					(equal? (cog-type (gar evl)) 'LgLinkNode))
				(cog-incoming-by-type (ListLink lwin rwin)
					'EvaluationLink)))))
		(define (get-link-str-name lwin rwin)
			(string-trim-right
				(cog-name (get-link-name lwin rwin))
				(char-set-adjoin char-set:lower-case #\*)))
		(define (incr-class-counts COUNT-VEC LINK-NAME)
			(define touched #f)
			(for-each
				(lambda (k)
					(if (any (lambda (ltyp) (equal? ltyp LINK-NAME)) (list-ref classes k))
						(begin
							(vector-set! COUNT-VEC k (+ 1 (vector-ref COUNT-VEC k)))
							(set! touched #t))))
				(iota nclasses))
			(if (not touched)
				(vector-set! COUNT-VEC nclasses (+ 1 (vector-ref COUNT-VEC nclasses))))
		)
		(define (incr-missing-link-type-count link-name)
			(define cnt (assoc-ref missing-link-types link-name))
			(if (not cnt) (set! cnt 0))
			(set! missing-link-types
				(assoc-set! missing-link-types link-name (+ 1 cnt))))
		(define (incr-missing-link-count lwin rwin)
			(define link-name (get-link-str-name lwin rwin))
			(if VERBOSE
				(format #t "Missing link: ~A <-- ~A --> ~A\n"
					(cog-name (get-word-of-winst lwin))
					link-name
					(cog-name (get-word-of-winst rwin))))
			(incr-class-counts missing link-name)
			(incr-missing-link-type-count link-name)
		)
		(define (incr-present-link-count lwin rwin)
			(define link-name (get-link-str-name lwin rwin))
			(if VERBOSE
				(format #t "Have link: ~A <-- ~A --> ~A\n"
					(cog-name (get-word-of-winst lwin))
					link-name
					(cog-name (get-word-of-winst rwin))))
			(incr-class-counts missing link-name)
		)
		(define (num-missing-words winli dict)
			(fold
				(lambda (win cnt)
					(define wrd (get-word-of-winst win))
					(if (< 0.5 (cog-tv-mean
							(cog-evaluate! (LgHaveDictEntry wrd dict))))
						cnt
						(begin
							(missing-words wrd)
							(+ cnt 1))))
				0 winli))
		(define (has-missing-words winli dict)
			(if (< 0 (num-missing-words winli dict))
				(begin
					(set! incomplete-dict (+ 1 incomplete-dict))
					#t)
				#f))
		(define (compare-lengths gold-sorted test-sorted)
			(define ewlilen (length gold-sorted))
			(define owlilen (length test-sorted))
			(if (equal? ewlilen owlilen)
				(set! total-words (+ total-words ewlilen))
				(begin
					(format #t "Length miscompare: ~A vs ~A\n" ewlilen owlilen)
					(set! length-miscompares (+ 1 length-miscompares))))
			(equal? ewlilen owlilen))
		(define (compare-words ewinst owinst)
			(define ewrd (get-word-of-winst ewinst))
			(define owrd (get-word-of-winst owinst))
			(vocab-words ewrd)
			(if (not (equal? ewrd owrd))
				(begin
					(if verbose
						(format #t "Word miscompare at ~A: ~A vs ~A\n"
							(get-index-of-winst ewinst) ewrd owrd))
					(set! word-miscompares (+ 1 word-miscompares)))))
		(define (compare-links ewin owin)
			(define ewrd (get-word-of-winst ewin))
			(define elinked (get-linked-winst ewin))
			(define olinked (get-linked-winst owin))
			(define ewords (map get-word-of-winst elinked))
			(define owords (map get-word-of-winst olinked))
			(define miss-w (lset-difference equal? ewords owords))
			(define have-w (lset-intersection equal? ewords owords))
			(define extra-w (lset-difference equal? owords ewords))
			(define (trim-wili wili wrd-set)
				(filter
					(lambda (wi)
						(any
							(lambda (wrd) (equal? (get-word-of-winst wi) wrd))
							wrd-set))
					wili))
			(define missing-wi (trim-wili elinked miss-w))
			(define present-wi (trim-wili elinked have-w))
			(define extra-wi   (trim-wili olinked extra-w))
			(define n-missing (length missing-wi))
			(define n-present (length present-wi))
			(define n-extra   (length extra-wi))
			(set! link-deficit (+ link-deficit n-missing))
			(set! link-correct (+ link-correct n-present))
			(set! link-excess  (+ link-excess  n-extra))
			(set! total-links (+ total-links (length elinked)))
			(if (or (< 0 n-missing) (< 0 n-extra))
				(begin
					(if verbose
						(format #t "Miscompare right-links: ~A missing, ~A extra for ~A"
							n-missing n-extra ewrd))
					(set! link-count-miscompares (+ 1 link-count-miscompares))))
			(for-each
				(lambda (misw) (incr-missing-link-count ewin misw))
				missing-wi)
			(for-each
				(lambda (havw) (incr-present-link-count ewin havw))
				present-wi)
		)
		(define (do-compare SENT)
			(define gold-sent (cog-execute!
				(LgParseMinimal (PhraseNode SENT) gold-dict (NumberNode 1))))
			(define test-sent (cog-execute!
				(LgParseMinimal (PhraseNode SENT) test-dict (NumberNode 1))))
			(define gold-parse (gar (car
				(cog-incoming-by-type gold-sent 'ParseLink))))
			(define test-parse (gar (car
				(cog-incoming-by-type test-sent 'ParseLink))))
			(define test-word-inst-list
				(map gar (cog-incoming-by-type test-parse 'WordInstanceLink)))
			(define left-wall (WordNode "###LEFT-WALL###"))
			(define right-wall (WordNode "###RIGHT-WALL###"))
			(define gold-word-inst-list
				(filter
					(lambda (winst)
						(define wrd (get-word-of-winst winst))
						(and
							(not (equal? wrd left-wall))
							(not (equal? wrd right-wall))))
					(map gar (cog-incoming-by-type gold-parse 'WordInstanceLink))))
			(define gold-sorted (sort-word-inst-list gold-word-inst-list))
			(define test-sorted (sort-word-inst-list test-word-inst-list))
			(define gold-has-missing-words (has-missing-words gold-sorted gold-dict))
			(define test-has-missing-words (has-missing-words test-sorted test-dict))
			(set! total-sentences (+ total-sentences 1))
			(if gold-has-missing-words
				(format #t "Gold dictionary is missing words in: \"~A\"\n" SENT))
			(if test-has-missing-words
				(format #t "Test dictionary is missing words in: \"~A\"\n" SENT))
			(if (and
					(or (not test-has-missing-words) INCLUDE-MISSING
						(not gold-has-missing-words))
					(compare-lengths gold-sorted test-sorted))
				(begin
					(set! total-compares (+ total-compares 1))
					(set! temp-cnt link-count-miscompares)
					(for-each
						(lambda (ewrd owrd)
							(compare-words ewrd owrd)
							(compare-links ewrd owrd)
						)
						gold-sorted test-sorted)
					(if (not (equal? temp-cnt link-count-miscompares))
						(set! bad-sentences (+ 1 bad-sentences)))
					(format #t "Finish compare of sentence ~A/~A: \"~A\"\n"
						total-compares total-sentences SENT)
				))
		)
		(define (do-compare-gc SENT)
			(define (kill typ)
				(for-each cog-extract-recursive (cog-get-atoms typ)))
			(catch 'C++-EXCEPTION
				(lambda () (do-compare SENT))
				(lambda (key . args) #f))
			(kill 'NumberNode)
			(kill 'WordInstanceNode)
			(kill 'SentenceNode)
			(kill 'PhraseNode)
			(kill 'ParseNode)
			(kill 'LgLinkNode)
			(kill 'LgHaveDictEntry)
		)
		(define (report-stats)
			(define link-expected-positives (exact->inexact total-links))
			(define link-true-positives link-correct)
			(define link-false-positives link-excess)
			(define link-false-negatives link-deficit)
			(define link-recall (/ link-true-positives link-expected-positives))
			(define link-precision (/ link-true-positives
				(+ link-true-positives link-false-positives)))
			(define link-f1 (/ (* 2.0 link-recall link-precision)
				(+ link-recall link-precision)))
			(define sorted-missing-links
				(sort missing-link-types
					(lambda (ia ib) (> (cdr ia) (cdr ib)))))
			(define class-recall (make-vector (+ 1 nclasses) 0))
			(define class-total (make-vector (+ 1 nclasses) 0))
			(for-each
				(lambda (k)
					(define tot (+ (vector-ref missing k) (vector-ref present k)))
					(define rcl
						(if (equal? 0 tot) (inf)
							(/ (vector-ref present k) tot)))
					(vector-set! class-total k tot)
					(vector-set! class-recall k rcl))
				(iota (+ 1 nclasses)))
			(newline)
			(newline)
			(format #t
				"Examined ~A sentences; ~A had words not in dictionary (~6F %).\n"
				total-sentences incomplete-dict
				(/ (* 100.0 incomplete-dict) total-sentences))
			(format #t
				"Finished comparing ~A parses; ~A parsed differently (~6F %).\n"
				total-compares bad-sentences
				(/ (* 100.0 bad-sentences) total-compares))
			(format #t
				"Found ~A word instances, vocab= ~A words; expect to find ~A links\n"
				total-words (length (vocab-words #f)) total-links)
			(format #t "Dictionary was missing ~A words\n"
				(length (missing-words #f)))
			(format #t "Found ~A length-miscompares\n" length-miscompares)
			(format #t "Found ~A word-miscompares\n" word-miscompares)
			(format #t
				"Found ~A words w/diffs in #links: ~A fewer and ~A extra links\n"
				link-count-miscompares
				link-deficit link-excess)
			(format #t "Link precision=~6F recall=~6F F1=~6F\n"
				link-precision link-recall link-f1)
			(newline)
			(for-each
				(lambda (k)
					(format #t "Link-class recall=~6F tot=~A ~A\n"
						(vector-ref class-recall k)
						(vector-ref class-total k)
						(list-ref classes k)))
				(iota nclasses))
			(format #t "Other link-classes recall=~6F tot=~A\n"
				(vector-ref class-recall nclasses)
				(vector-ref class-total nclasses))
			(newline)
			(format #t "Counts of missing link-types: ~A\n\n"
				sorted-missing-links)
			(format #t "Missing words: ~A\n\n"
				(map cog-name (missing-words #f)))
		)
		(lambda (SENT)
			(if (not SENT)
				(report-stats)
				(do-compare-gc SENT)))
	)
)
(define*-public (make-lg-en-comparator DICT #:key
     (INCLUDE-MISSING #f)
     (VERBOSE #f)
)
"
  make-lg-en-comparator DICT - Return an English sentence comparison function.
  DICT should be an LgDictNode containing a dictionary that will be
  compared to the Link Grammar English dictionary.
  This returns a comparison function.  To use it, pass one or more
  sentence strings to it
  When finished, pass it `#f`, and it will print a summary report.
  Example usage:
     (define compare (make-lg-comparator (LgDictNode \"micro-fuzz\")))
     (compare \"I saw her face\")
     (compare \"I swooned to the floor\")
     (compare #f)
  By default, this does not test sentences that have words that are
  not in the test dictionary.  Over-ride this by supplying the optional
  argument INCLUDE-MISSING.
"
	(define primary-links (list "S" "O" "MV" "SI" "CV"))
	(define secondary-links (list "A" "AN" "B" "C" "D" "E" "EA" "G" "J" "M" "MX" "R"))
	(define punct-links (list "X"))
	(define classes (list primary-links secondary-links punct-links))
	(make-lg-comparator (LgDictNode "en") DICT classes
     #:INCLUDE-MISSING INCLUDE-MISSING
     #:VERBOSE VERBOSE)
)