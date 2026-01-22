#! /usr/bin/env guile
!#
(load "common.scm")
(define duplicate-eval-list
	(look-for-dupes
		(string-append
			"SELECT uuid,outgoing FROM atoms WHERE type= " EvalLinkType ";")
		"outgoing"))
(display "The duplicate eval list size: ")
(display (length duplicate-eval-list)) (newline)
(flush-output-port (current-output-port))
(define (eliminate-eval-dupes oset-list)
"
  eliminate-eval-dupes -- look for identical EvaluationLinks
  Given a list of outgoing-sets, sum the count stv, and update
  the count on one of the dupes, and delete the other dupe.
"
	(define num-done 0)
	(define (sum-counts oset)
		(define row #f)
		(define sum 0)
		(define uuid 0)
		(define smallest-uuid 2012123123)
		(define dup-list (list))
		(define qry (string-append
			"SELECT * FROM atoms WHERE type="
			EvalLinkType
			" AND outgoing="
			(make-outgoing-str oset)))
		(dbi-query conxion qry)
		(set! row (dbi-get_row conxion))
		(while (not (equal? row #f))
			(set! sum (+ sum (cdr (assoc "stv_count" row))))
			(set! uuid (cdr (assoc "uuid" row)))
			(if (< uuid smallest-uuid)
				(set! smallest-uuid uuid))
			(set! dup-list (cons uuid dup-list))
			(set! row (dbi-get_row conxion))
		)
		(let ((upd (string-append
				"UPDATE atoms SET stv_count="
				(number->string sum)
				" WHERE uuid="
				(number->string smallest-uuid)
				";")))
			(if do-update (begin
				(dbi-query conxion upd)
				(flush-query)))
			(delete-atoms dup-list  smallest-uuid)
		)
		(set! num-done (+ num-done 1))
		(if (eq? 0 (modulo num-done 1000)) (begin
			(display "Processed ")(display num-done)
			(display " eval-dedupes")(newline))
			(flush-output-port (current-output-port))
		)
		smallest-uuid
	)
	(define cnt-list (map sum-counts oset-list))
	(display "oset: ") (display (length oset-list))(newline)
	(display "cnts: ") (display (length cnt-list))(newline)
)
(eliminate-eval-dupes duplicate-eval-list)