(load "extensional-utils.scm")
(define subset-direct-introduction-rule
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(CptT (TypeInh 'Concept))
(AndT (Type 'And)))
(Bind
(VariableSet
(TypedVariable A (TypeChoice CptT AndT))
(TypedVariable B (TypeChoice CptT AndT)))
(Present
A
B)
(ExecutionOutput
(GroundedSchema "scm: subset-direct-introduction")
(List
(Subset A B)
A
B)))))
(define (subset-evidence->tv A-mbrs B-mbrs)
(let*
((A-size (length A-mbrs))
(AB-mbrs (lset-intersection equal? A-mbrs B-mbrs))
(AB-size (length AB-mbrs))
(strength (if (< 0 A-size)
(exact->inexact (/ AB-size A-size))
1))
(confidence (if (< 0 A-size)
(count->confidence A-size)
0)))
(stv strength confidence)))
(define (subset-direct-introduction conclusion . premises)
(if (= (length premises) 2)
(let* ((Ss conclusion)
(A (car premises))
(B (cadr premises))
(A-mbrs (get-members-of A))
(B-mbrs (get-members-of B))
(tv (subset-evidence->tv A-mbrs B-mbrs)))
(if (and (< 0 (cog-tv-mean tv)) (< 0 (cog-tv-confidence tv)))
(cog-merge-hi-conf-tv! Ss tv)))))
(define subset-direct-introduction-rule-name
(DefinedSchemaNode "subset-direct-introduction-rule"))
(DefineLink subset-direct-introduction-rule-name
subset-direct-introduction-rule)