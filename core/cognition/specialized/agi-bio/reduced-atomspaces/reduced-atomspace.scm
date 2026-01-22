(define (cog-get-categories A)
"
Return the atoms that A is a member of through a MemberLink relationship.
"
(let ((result
(cog-outgoing-set
(cog-bind
(BindLink
(VariableList
(VariableNode "$B")
)
(MemberLink
A
(VariableNode "$B")
)
(VariableNode "$B")
)
)
)
)) result )
)
(define (cog-remove-type atom-type atoms)
(filter (lambda (atom) (not (eq? atom-type (cog-type atom)))) atoms))
(define genes (list "TBC1D4" "ADCY9"))
(define gene-nodes (map GeneNode genes))
(define member-cats (fold union '() (map cog-incoming-set gene-nodes)))
(define cats (map cog-get-categories gene-nodes))
(use-modules (ice-9 common-list))
(set! cats (fold union '() cats))
(define cat-nodes (fold union '() (map cog-get-root cats)))
(define cat-nodes (cog-remove-type 'MemberLink cat-nodes))
(define cat-nodes (cog-remove-type 'SetLink cat-nodes))