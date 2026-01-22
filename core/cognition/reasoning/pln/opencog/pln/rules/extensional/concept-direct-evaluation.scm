(define concept-direct-evaluation-rule
(let* ((X (Variable "$X"))
(Y (Variable "$Y"))
(A (Variable "$A"))
(CptT (Type 'Concept)))
(Bind
(VariableSet
X
Y
(TypedVariable A CptT))
(And
(Present
(Member X A)
(Member Y A)
A)
(Evaluation
(GroundedPredicate "scm: absolutely-true")
(Member X A))
(Evaluation
(GroundedPredicate "scm: absolutely-true")
(Member Y A))
(Not (Identical X Y)))
(ExecutionOutput
(GroundedSchema "scm: concept-direct-evaluation")
(List
A
(Set
(Member X A)
(Member Y A)))))))
(define (concept-direct-evaluation conclusion . premises)
(if (= (length premises) 1)
(let* ((mbr-lnks (cog-outgoing-set (car premises)))
(mbr-lnk-A (car mbr-lnks))
(mbr-lnk-B (cadr mbr-lnks))
(all-nodes (cog-get-atoms 'Node #f))
(usize (length all-nodes))
(vsize (if (not (equal? mbr-lnk-A mbr-lnk-B)) 2 1))
(tv-s (/ vsize usize))
(tv-c (count->confidence usize)))
(if (< 0 tv-c)
(cog-merge-hi-conf-tv! conclusion (stv tv-s tv-c))))))
(define concept-direct-evaluation-rule-name
(DefinedSchemaNode "concept-direct-evaluation-rule"))
(DefineLink concept-direct-evaluation-rule-name
concept-direct-evaluation-rule)