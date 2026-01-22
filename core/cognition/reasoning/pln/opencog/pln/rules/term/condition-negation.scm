(define subset-condition-negation-rule
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(CT (Type "ConceptNode")))
(Bind
(VariableSet
(TypedVariable A CT)
(TypedVariable B CT))
(Present
(Subset A B))
(ExecutionOutput
(GroundedSchema "scm: subset-condition-negation")
(List
(Subset (Not A) B)
(Subset A B)
A
B)))))
(define (subset-condition-negation conclusion . premises)
(if (= (length premises) 3)
(let* ((NS conclusion)
(S (car premises))
(A (cadr premises))
(B (caddr premises))
(Ss (cog-mean S))
(Sc (cog-confidence S))
(As (cog-mean A))
(Ac (cog-confidence A))
(Bs (cog-mean B))
(NAs (- 1 As))
(NSs (if (< As 1)
(/ (- Bs (* Ss As)) NAs)
1))
(NSc (if (< As 1)
(min (count->confidence (* (confidence->count Ac) NAs)) Sc)
0))
(NStv (stv NSs NSc)))
(cog-merge-hi-conf-tv! NS NStv))))
(define subset-condition-negation-rule-name
(DefinedSchemaNode "subset-condition-negation-rule"))
(DefineLink subset-condition-negation-rule-name subset-condition-negation-rule)