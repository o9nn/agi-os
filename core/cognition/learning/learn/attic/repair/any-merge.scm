#! /usr/bin/env guile
!#
(load "common.scm")
(define (get-all-atoms query colm)
"
get-all-atoms -- Execute the query, return all colm values.
colm should be the string column name
Returns a list of the 'colm' entries
"
(define alist (list))
(define word-count 0)
(define row #f)
(dbi-query conxion query)
(display "Atom search connection status: ")
(display (dbi-get_status conxion)) (newline)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(let* ((valu (cdr (assoc colm row))))
(set! alist (cons valu alist))
(set! word-count (+ word-count 1))
(set! row (dbi-get_row conxion))
)
)
(display "For the query: ")(display query)(newline)
(display "The num rows was: ") (display word-count) (newline)
alist
)
(define (get-all-evals alist anyid)
"
get-all-evals -- Get all of the EvaluationLinks that contain
a ListLink and the anyid uuid.
Returns a list of the EvaluationLink entries.
"
(define word-count 0)
(define elist (list))
(define (get-eval uuid)
(define euid 0)
(define row #f)
(define qry (string-concatenate (list
"SELECT uuid FROM atoms WHERE type="
EvalLinkType
" AND outgoing="
(make-outgoing-str (list anyid uuid)))))
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! euid (cdr (assoc "uuid" row)))
(set! word-count (+ word-count 1))
(set! row (dbi-get_row conxion))
)
euid
)
(set! elist (map get-eval alist))
(display "Number of EvaluationLinks: ") (display word-count) (newline)
elist
)
(define all-list-links (get-all-atoms
"SELECT uuid FROM atoms WHERE type=8" "uuid"))
(define (count-evlinks any-uuid)
(display "Numb of ") (display any-uuid) (display " evals: ")
(display (length (get-all-evals all-list-links any-uuid)))(newline))
(define (relabel-evals alist bad-id good-id)
"
relabel-evals -- Change the oset of all of the EvaluationLinks
that use the bad ANY uuid
Returns a list of the changed EvaluationLink entries.
"
(define word-count 0)
(define elist (list))
(define (set-eval euid luid any-id)
(define row #f)
(define qry (string-concatenate (list
"UPDATE atoms SET outgoing="
(make-outgoing-str (list any-id luid))
" WHERE uuid="
(number->string euid))))
(dbi-query conxion qry)
(flush-query)
)
(define (change-eval uuid)
(define euid 0)
(define row #f)
(define qry (string-concatenate (list
"SELECT uuid FROM atoms WHERE type="
EvalLinkType
" AND outgoing="
(make-outgoing-str (list bad-id uuid)))))
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! euid (cdr (assoc "uuid" row)))
(set! word-count (+ word-count 1))
(if (eq? 0 (modulo word-count 1000)) (begin
(display "Processed ")(display word-count)
(display " id-relabels")(newline))
(flush-output-port (current-output-port))
)
(set! row (dbi-get_row conxion))
)
(if (< 0 euid)
(set-eval euid uuid good-id))
)
(set! elist (map change-eval alist))
(display "Relabel ANY uuid ") (display bad-id)
(display " to ") (display good-id)(newline)
(display "Relabeled uuid count was ") (display word-count) (newline)
(flush-output-port (current-output-port))
elist
)
(define (get-all-non-any-evals any-id)
"
get-all-non-any-evals -- look for all EvalLinks that do NOT
hold the desired ANY node.  At this point in the game, there
should not be any of these. But there are. WTF. Oh, it was
a bad conversion of int8 to long long in guile-dbi.
"
(define bad-list (list))
(define euid 0)
(define luid 0)
(define oset (list))
(define row #f)
(define qry (string-concatenate (list
"SELECT uuid,outgoing FROM atoms WHERE type="
EvalLinkType)))
(display qry)(newline)
(dbi-query conxion qry)
(set! row (dbi-get_row conxion))
(while (not (equal? row #f))
(set! euid (cdr (assoc "uuid" row)))
(set! oset (cdr (assoc "outgoing" row)))
(set! luid (cadr oset))
(if (not (eq? (car oset) any-id)) (begin
(set! bad-list (cons luid bad-list))
))
(set! row (dbi-get_row conxion))
)
bad-list
)
(define bad-list (get-all-non-any-evals 57))
(display "Number of bad evals: ") (display (length bad-list))(newline)