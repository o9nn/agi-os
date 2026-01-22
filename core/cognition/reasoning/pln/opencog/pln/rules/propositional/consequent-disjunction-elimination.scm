(define (gen-consequent-disjunction-elimination-rule impl-type var-type)
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(C (Variable "$C")))
(Bind
(VariableList
(TypedVariable A var-type)
(TypedVariable B var-type)
(TypedVariable C var-type))
(Present
(impl-type
A
(Or
B
C)))
(ExecutionOutput
(GroundedSchema "scm: consequent-disjunction-elimination-formula")
(List
(impl-type
A
B)
(impl-type
A
(Or
B
C))
(impl-type
A
C))))))
(define consequent-disjunction-elimination-inheritance-rule
(let ((var-type (TypeChoice
(TypeNode "ConceptNode")
(TypeNode "AndLink")
(TypeNode "OrLink")
(TypeNode "NotLink"))))
(gen-consequent-disjunction-elimination-rule InheritanceLink var-type)))
(define consequent-disjunction-elimination-implication-rule
(let ((var-type (TypeChoice
(TypeNode "PredicateNode")
(TypeNode "LambdaLink")
(TypeNode "AndLink")
(TypeNode "OrLink")
(TypeNode "NotLink"))))
(gen-consequent-disjunction-elimination-rule ImplicationLink var-type)))
(define (consequent-disjunction-elimination-formula conclusion . premises)
(if (= (length premises) 2)
(let* ((ABC (list-ref premises 0))
(AC (list-ref premises 1))
(sABC (cog-mean ABC))
(cABC (cog-confidence ABC))
(sAC (cog-mean AC))
(cAC (cog-confidence AC))
(alpha 0.9)
(AB conclusion)
(precondition (and (<= sAC sABC) (< sAC 1)))
(sAB (if precondition
(/ (- sABC sAC) (- 1 sAC))
1))
(cAB (if precondition
(* alpha (min cABC cAC))
0)))
(if (< 0 cAB)
(cog-merge-hi-conf-tv! AB (stv sAB cAB))))))
(define consequent-disjunction-elimination-inheritance-rule-name
(DefinedSchemaNode "consequent-disjunction-elimination-inheritance-rule"))
(DefineLink consequent-disjunction-elimination-inheritance-rule-name
consequent-disjunction-elimination-inheritance-rule)
(define consequent-disjunction-elimination-implication-rule-name
(DefinedSchemaNode "consequent-disjunction-elimination-implication-rule"))
(DefineLink consequent-disjunction-elimination-implication-rule-name
consequent-disjunction-elimination-implication-rule)