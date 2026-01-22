(use-modules (opencog) (opencog nlp) (opencog persist))
(use-modules (opencog exec))
(use-modules (srfi srfi-1))
(define *-word-pair-dist-* (SchemaNode "*-Pair Distance-*"))
(define *-word-pair-tag-* (PredicateNode "*-Sentence Word Pair-*"))
(define (make-word-sequence PARSE)
(define (get-number word-inst)
(cog-number (word-inst-get-number word-inst)))
(define (least word-inst lim)
(define no (get-number word-inst))
(if (< no lim) no lim))
(define wall-no (fold least 9e99 (parse-get-words PARSE)))
(define (make-ordered-word word-inst)
(WordSequenceLink
(word-inst-get-word word-inst)
(NumberNode (- (get-number word-inst) wall-no))))
(define (get-no seq-lnk)
(cog-number (gdr seq-lnk)))
(sort (map make-ordered-word (parse-get-words PARSE))
(lambda (wa wb)
(< (get-no wa) (get-no wb))))
)
(define (update-pair-counts-once PARSE MAX-LEN RECORD-LEN)
(define (get-no seq-lnk)
(cog-number (gdr seq-lnk)))
(define (count-one-pair left-seq right-seq)
(define dist (- (get-no right-seq) (get-no left-seq)))
(if (<= dist MAX-LEN)
(let ((pare (ListLink (gar left-seq) (gar right-seq))))
(count-one-atom (EvaluationLink *-word-pair-tag-* pare))
(if RECORD-LEN
(count-one-atom
(ExecutionLink *-word-pair-dist-* pare (NumberNode dist)))))))
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
(update-pair-counts-once
(car (sentence-get-parses SENT))
MAX-LEN RECORD-LEN)
)
(define-public (observe-clique count-reach plain-text)
"
observe-clique COUNT-REACH PLAIN-TEXT --
update word and word-pair counts by observing raw text.
Uses the window counting technique, to examine all possible pairs.
COUNT-REACH is the window size.
PLAIN-TEXT is a utf8 string of text.
Tokenizes the sentence string into words, according to white-space.
It then forms all word-pairs within a sliding window of width
COUNT-REACH, and updates counts on those pairs. Thus, each word will
participate in exactly COUNT-REACH-1 word pairs.
Distance is defined as the difference between word positions in the
sentence, so neighboring words have distance of 1.
The parse rate can be monitored by calling, by hand, the guile function
`(monitor-parse-rate MSG)` for some string MSG.
"
(define (process-sent SENT win-size)
(update-word-counts SENT)
(update-clique-pair-counts SENT win-size #f)
(delete-sentence SENT)
(monitor-parse-rate #f))
(local-process plain-text count-reach)
)