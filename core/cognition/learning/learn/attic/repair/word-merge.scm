#! /usr/bin/env guile
!#
(load "common.scm")
(define (find-pairs wuid)
"
find-pairs -- given a uuid of single word, find all word-pairs
which contain the word.  The wuid must be the uuid of a WordNode.
Returns a list of ListLink pairs that contain the WordNode.
"
(define row #f)
(define pair-list (list))
(define qry "SELECT * FROM atoms WHERE type=8")
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(let ((outset (cdr (assoc "outgoing" row))))
(if (any (lambda (x) (eq? x wuid)) outset)
(begin
(set! pair-list (cons outset pair-list))
)
)
(set! row (dbi-get_row conxion))
)
)
pair-list
)
(define (find-list-link pair)
"
Given a pair of UUID's, get the uuid of the ListLink that holds it.
Return that UUID, else return zero
"
(define luid 0)
(define row #f)
(define qry (string-append
"SELECT * FROM atoms WHERE type=8 AND outgoing="
(make-outgoing-str pair)))
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! luid (cdr (assoc "uuid" row)))
(set! row (dbi-get_row conxion))
)
luid
)
(define (sum-up-eval-counts wuid auid pair-list)
"
sum-up-eval-counts -- look for identical word-pairs
wuid -- uuid of a word node
auid -- uuid of another word node (holding same word)
pair-list -- list of uuid-pairs, one of which is wuid.
Given a list of pairs containing wuid, find the ListLinks
correspodning tio them.  Also: find the ListLinks that hold
the corresponding auid.  Then find the EvaluationLinks that
correspond to these, sum up the count TV's on them and then
delete the ListLink and EvalLink that correspond to auid.
"
(define (replace wuid auid pair)
(if (eq? wuid (car pair))
(list auid (cadr pair))
(list (car pair) auid)
)
)
(define alt-list (map (lambda (x) (replace wuid auid x)) pair-list))
(define luid-list (map find-list-link pair-list))
(define laid-list (map find-list-link alt-list))
(define (get-col colm uuid)
(define row #f)
(define val 0)
(define qry (string-append
"SELECT * FROM atoms WHERE type="
EvalLinkType
" AND outgoing="
(make-outgoing-str (list uuid-of-any uuid))))
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! val (cdr (assoc colm row)))
(set! row (dbi-get_row conxion))
)
val
)
(define (get-count uuid) (get-col "stv_count" uuid))
(define (get-eval-uuid uuid) (get-col "uuid" uuid))
(define (sum-counts luid laid)
(if (and (< 0 luid) (< 0 laid))
(let* ((eud (get-eval-uuid luid))
(aud (get-eval-uuid laid))
)
(if (and (< 0 eud) (< 0 aud))
(let* ((lcnt (get-count luid))
(acnt (get-count laid))
(scnt (+ lcnt acnt))
(upd (string-append
"UPDATE atoms SET stv_count="
(number->string scnt)
" WHERE uuid="
(number->string eud)
";"))
(apd (string-append
"DELETE FROM atoms WHERE uuid="
(number->string aud)
";"))
(alt (string-append
"DELETE FROM atoms WHERE uuid="
(number->string laid)
";"))
)
(dbi-query conxion upd)
(flush-query)
(dbi-query conxion apd)
(flush-query)
(dbi-query conxion alt)
(flush-query)
scnt
)
#f
)
)
(begin
#f
)
)
)
(define (bug-cleanup laid)
(if (< 0 laid)
(let ((alt (string-append
"DELETE FROM atoms WHERE uuid="
(number->string laid)
";")))
(display alt) (newline)
(dbi-query conxion alt)
(display (dbi-get_status conxion)) (newline)
(flush-query)
#t
)
#f
)
)
(define cnt-list (filter-map sum-counts luid-list laid-list))
(display "num of word-pairs: ") (display (length pair-list))(newline)
(display "num of alt-pairs: ") (display (length alt-list))(newline)
(display "num luids (ListLinks): ") (display (length luid-list))(newline)
(display "num of laids (listLinks holding alts): ") (display (length
(filter (lambda (x) (< 0 x)) laid-list)))(newline)
(display "num actually fixed up: ") (display (length cnt-list))(newline)
(newline)
)
(define (swap-alts altid wantid pair-list)
"
swap-alts -- fix up the uuids -- replace altid by wantid in
the ListLink that contains the altid.  This modifies the
ListLink
long as the pair-list was previously de-duped. However, for
multiple lists, this may be violated :-(
altid is the uuid of the unwanted WordNode
wantid is the uuid of the correct WordNode
pair-list is the list of uuid-pairs, one of which is altid.
"
(define (replace wuid auid pair)
(if (eq? wuid (car pair))
(list auid (cadr pair))
(list (car pair) auid)
)
)
(define laid-list (map find-list-link pair-list))
(define fix-list (map (lambda (x) (replace altid wantid x)) pair-list))
(define (fixup laid fix)
(define upd (string-append
"UPDATE atoms SET outgoing="
(make-outgoing-str fix)
" WHERE uuid="
(number->string laid)
";"))
(dbi-query conxion upd)
(flush-query)
)
(for-each fixup laid-list fix-list)
(display "Num of relabeled ListLinks: ")
(display (length laid-list)) (newline)
(newline)
(flush-output-port (current-output-port))
)
(define (escape-quote word)
"
If word has a single-quote in it, then escape it!
"
(define qk (string-index word #\'))
(if qk
(string-replace word "''" qk (+ qk 1))
word
)
)
(define (get-word-uuids word)
"
Given a word (string), get the UUID's of all WordNodes holding
that word. Return these as a list.
"
(define row #f)
(define wuid-list (list))
(define qry (string-append
"SELECT uuid FROM atoms WHERE type=" WordNodeType
" AND name='" (escape-quote word) "';"))
(display qry)(newline)
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! wuid-list (cons (cdr (assoc "uuid" row)) wuid-list))
(set! row (dbi-get_row conxion))
)
(display "Word ")(display word)
(display " has uuids ")(display wuid-list) (newline)
wuid-list
)
(define (sum-word-counts word)
"
Given a word (string), sum up the stv-count of all WordNodes holding
that word. Return the sum
"
(define row #f)
(define sum 0)
(define qry (string-append
"SELECT uuid,stv_count FROM atoms WHERE type=" WordNodeType
" AND name='" (escape-quote word) "';"))
(display qry)(newline)
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! sum (+ (cdr (assoc "stv_count" row)) sum))
(set! row (dbi-get_row conxion))
)
(display "Word ")(display word)
(display " has sum ")(display sum) (newline)
sum
)
(define duplicate-word-list
(look-for-dupes (string-append
"SELECT uuid, name FROM atoms WHERE type="
WordNodeType) "name"))
(display "The duplicate word list is: ")
(display duplicate-word-list) (newline)
(flush-output-port (current-output-port))
(define (dedupe-word word)
"
Deduplicate the word. This sums up counts on the EvaluationLinks
that contain the duplicted words. It also relabels the ListLinks
that use the wrong word uid.  Basically it does everything except
to sum up the WordNodes themselves.
"
(define wuid-list (get-word-uuids word))
(define (get-smallest-uuid uuid-list)
(define sma 6123456123123)
(for-each (lambda (x) (if (< x sma) (set! sma x))) uuid-list)
(display "smallest wuid is ")(display sma)(newline)
sma
)
(define smallest-wuid (get-smallest-uuid wuid-list))
(define bad-wuid-list
(remove (lambda (x) (eq? x smallest-wuid)) wuid-list))
(define wuid-word-pairs (find-pairs smallest-wuid))
(define (fixup-bad bad)
(define alt-list (list))
(sum-up-eval-counts smallest-wuid bad wuid-word-pairs)
(set! alt-list (find-pairs bad))
(display "Need to relabel bad pairs: ")
(display (length alt-list))(newline)
(swap-alts bad smallest-wuid alt-list)
(flush-output-port (current-output-port))
)
(display "Number of word-pairs: ")
(display (length wuid-word-pairs))(newline) (newline)
(flush-output-port (current-output-port))
(display "The bad wuids: ")(display bad-wuid-list)(newline)
(for-each fixup-bad bad-wuid-list)
)
(define (dedupe-sum-word-counts word)
"
sum-word-counts -- Given the (string) word, find all WordNodes that
hold this word, sum up the counts on all of them, assign the count to
the one with the smallest uuid, and delete the rest.
"
(define wuid-list (get-word-uuids word))
(define (get-smallest-uuid uuid-list)
(define sma 6123456123123)
(for-each (lambda (x) (if (< x sma) (set! sma x))) uuid-list)
(display "smallest wuid is ")(display sma)(newline)
sma
)
(define smallest-wuid (get-smallest-uuid wuid-list))
(define bad-wuid-list
(remove (lambda (x) (eq? x smallest-wuid)) wuid-list))
(define sum-of-counts (sum-word-counts word))
(define upd (string-append
"UPDATE atoms SET stv_count="
(number->string sum-of-counts)
" WHERE uuid="
(number->string smallest-wuid)
";"))
(display upd)(newline)
(dbi-query conxion upd)
(display (dbi-get_status conxion)) (newline)
(flush-query)
(delete-atoms bad-wuid-list 0 #t)
)
(for-each dedupe-sum-word-counts duplicate-word-list)