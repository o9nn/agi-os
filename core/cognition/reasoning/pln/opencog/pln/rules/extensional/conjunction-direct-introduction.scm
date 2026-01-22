(load "extensional-utils.scm")
(define conjunction-direct-introduction-rule
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(CptT (Type 'Concept)))
(Bind
(VariableSet
(TypedVariable A CptT)
(TypedVariable B CptT))
(Present
A
B)
(ExecutionOutput
(GroundedSchema "scm: conjunction-direct-introduction")
(List
(And A B)
(Set
A
B))))))
(define (conjunction-direct-introduction conclusion . premises)
(if (= (length premises) 1)
(let* ((operands (car premises))
(A (gar operands))
(B (gdr operands))
(A-mbrs (get-members-of A))
(B-mbrs (get-members-of B))
(AB-mbrs (lset-intersection equal? A-mbrs B-mbrs))
(all-nodes (cog-get-atoms 'Node #f))
(usize (length all-nodes))
(tv-s (/ (length AB-mbrs) usize))
(tv-c (count->confidence usize)))
(if (and (< 0 tv-s) (< 0 tv-c))
(let* ((tv (stv tv-s tv-c)))
(map (lambda (x) (Member (stv 1 1) x conclusion)) AB-mbrs)
(cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c)))))))
(define conjunction-direct-introduction-rule-name
(DefinedSchemaNode "conjunction-direct-introduction-rule"))
(DefineLink conjunction-direct-introduction-rule-name
conjunction-direct-introduction-rule)