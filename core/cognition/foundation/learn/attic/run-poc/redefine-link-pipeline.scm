(define (update-pair-counts-once PARSE DIST-MODE MAX-LEN RECORD-LEN)
	(define calc-times
		(if DIST-MODE
			(lambda (d) (quotient MAX-LEN d))
			(lambda (d) 1)))
	(define (get-no seq-lnk)
		(string->number (cog-name (gdr seq-lnk))))
	(define (count-one-pair left-seq right-seq)
		(define dist (- (get-no right-seq) (get-no left-seq)))
		(if (<= dist MAX-LEN)
			(let ((pare (ListLink (gar left-seq) (gar right-seq)))
				(counts (calc-times dist)))
				(count-one-atom-times (EvaluationLink pair-pred pare) counts)
				(if RECORD-LEN
					(count-one-atom
						(ExecutionLink pair-dist pare (NumberNode dist)))))))
	(define (count-pairs first rest)
		(if (not (null? rest))
			(begin
				(count-one-pair first (car rest))
				(count-pairs first (cdr rest)))))
	(define (make-pairs word-list)
		(if (not (null? word-list))
			(begin
				(count-pairs (car word-list) (cdr word-list))
				(make-pairs (cdr word-list)))))
	(define word-seq (make-word-sequence PARSE))
	(for-each count-one-atom word-seq)
	(make-pairs word-seq)
)
(define (update-clique-pair-counts SENT MAX-LEN RECORD-LEN)
	(update-clique-pair-counts-mode SENT #f MAX-LEN RECORD-LEN))
(define (update-clique-pair-counts-mode SENT DIST-MODE MAX-LEN RECORD-LEN)
	(update-pair-counts-once
		(car (sentence-get-parses SENT))
		DIST-MODE MAX-LEN RECORD-LEN)
)
(define-public (observe-text-mode plain-text observe-mode count-reach)
"
 observe-text-mode -- update word and word-pair counts by observing raw text.
 There are currently three observing modes, set by observe-mode, all taking
 an integer parameter:
 - any: counts pairs of words linked by the LG parser in 'any' language.
        In this case, 'count-reach' specifies how many linkages from LG-parser
        to use.
 - clique: itearates over each word in the sentence and pairs it with
           every word located within distance 'count-reach' to its right.
           Distance is defined as the difference between words positions
           in the sentence, so neighboring words have distance of 1.
 - clique-dist: same word-pairs as 'clique', but each word-pair is counted
                a number of times determined by the distance between words
                in the pair as:
                (quotient count-reach distance)
 This is the first part of the learning algo: simply count the words
 and word-pairs observed in incoming text. This takes in raw text, gets
 it parsed, and then updates the counts for the observed words and word
 pairs.
"
	(define (update-counts sent)
		(catch 'wrong-type-arg
			(lambda () (begin
				(update-word-counts sent)
				(update-lg-link-counts sent)
			))
			(lambda (key . args) #f)))
	(define (process-sent SENT cnt-mode win-size)
		(update-word-counts SENT)
		(cond
		 	((equal? cnt-mode "any") (update-lg-link-counts SENT))
			((equal? cnt-mode "clique") (update-clique-pair-counts-mode SENT #f win-size #f))
			((equal? cnt-mode "clique-dist") (update-clique-pair-counts-mode SENT #t win-size #f)))
		(delete-sentence SENT)
		(monitor-parse-rate '()))
	(define sometimes-gc
		(let ((cnt 0)
				(how-often 10))
			(lambda ()
				(set! cnt (+ cnt 1))
				(if (eqv? 0 (modulo cnt how-often)) (gc)))))
	(define maybe-gc
		(let ((cnt 0)
				(max-size (* 2750 1000 1000)))
			(lambda ()
				(if (< max-size (- (assoc-ref (gc-stats) 'heap-size)
							(assoc-ref (gc-stats) 'heap-free-size)))
					(begin
						(gc)
						(set! cnt (+ cnt 1))
					)))))
	(define (relex-process TXT)
		(define (do-all-sents)
			(let ((sent (get-one-new-sentence)))
				(if (not (null? sent))
					(begin (process-sent sent) (do-all-sents)))))
		(relex-parse TXT)
		(do-all-sents)
		(maybe-gc)
	)
	(define (local-process TXT obs-mode cnt-reach)
		(catch #t
			(lambda ()
				(let* ((phr (Phrase TXT))
						(num-parses (if (equal? obs-mode "any") cnt-reach 1))
						(lgn (LgParseMinimal phr (LgDict "any") (Number num-parses)))
						(sent (cog-execute! lgn))
					)
					(process-sent sent obs-mode cnt-reach)
					(cog-extract lgn)
					(cog-extract phr)
				))
			(lambda (key . args) #f))
	)
	(local-process plain-text observe-mode count-reach)
)