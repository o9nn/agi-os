#! /usr/bin/env guile
!#
(load "common.scm")
(define duplicate-pair-list
(look-for-dupes
"SELECT uuid,outgoing FROM atoms WHERE type=8;" "outgoing"))
(display "the duplicate pair list is: ")
(display duplicate-pair-list) (newline)
(display "Number of dupes: ")
(display (length duplicate-pair-list))(newline)
(flush-output-port (current-output-port))
(define (undup-eval luid-list)
"
undup-eval -- consolidate duplicate EvaluationLinks
The luid-list should be a list of integer uuids for the ListLinks
that are the duplicates. The proceedure here is fully automatic
the total count is computed, the count on the EvalLink with the
smallest uuid is updted, the other EvalLinks and the other ListLinks
are automatically deleted.
"
(define smallest-evid 2012123123)
(define smallest-luid 2012123123)
(define eval-list (list))
(define count_tot 0)
(define qry "")
(define (sum-count uuid)
(define row #f)
(define qry "")
(set! qry (string-append
"SELECT * FROM atoms WHERE type="
EvalLinkType
" AND outgoing="
(make-outgoing-str (list uuid-of-any uuid)))
)
(display "Eval qry is ")(display qry) (newline)
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(let (
(cnt (cdr (assoc "stv_count" row)))
(eid (cdr (assoc "uuid" row))))
(display "EvalLink uuid= ") (display eid)
(display " cnt= ") (display cnt) (newline)
(set! eval-list (cons eid eval-list))
(if (< uuid smallest-luid)
(begin
(set! smallest-luid uuid)
(set! smallest-evid eid)))
(set! count_tot (+ count_tot cnt))
(set! row (dbi-get_row conxion))
)
)
)
(for-each sum-count luid-list)
(display "total stv= ") (display count_tot) (newline)
(display "uuid= ") (display smallest-evid) (newline)
(set! qry (string-concatenate (list
"UPDATE atoms SET stv_count="
(number->string count_tot)
" WHERE uuid="
(number->string smallest-evid))))
(display qry) (newline)
(if do-update (begin
(dbi-query conxion qry)
(display (dbi-get_status conxion)) (newline)
(flush-query)))
(delete-atoms eval-list smallest-evid)
(delete-atoms luid-list smallest-luid)
)
(define (undup-pair pair)
"
undup-pair Given a pair of UUID's that define a ListLink (well, more
than one -- several duplicates), consolidate all of the duplicates.
This works by obtaining a list of all of the duplicate UUID's, and
then calling 'undup-eval' to consolidate them. The undup-eval
sums up the counts, and deletes the duplicates.
"
(define row #f)
(define uuid-list (list))
(define qry (string-append
"SELECT * FROM atoms WHERE type=8 and outgoing="
(make-outgoing-str pair)))
(dbi-query conxion qry)
(display "Duplicate list search status: ")
(display (dbi-get_status conxion)) (newline)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(let ((uuid (cdr (assoc "uuid" row))))
(display "ListLink uuid= ") (display uuid) (newline)
(set! uuid-list (cons uuid uuid-list))
(set! row (dbi-get_row conxion))
)
)
(undup-eval uuid-list)
(flush-output-port (current-output-port))
)
(display "number of dupes: ")
(display (length duplicate-pair-list))(newline)