(use-modules (opencog logger))
(use-modules (opencog ure))
(define (gen-full-deduction-rule link-type var-type)
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(C (Variable "$C"))
(AB (link-type A B))
(ABC (link-type (And A B) C))
(ANBC (link-type (And A (Not B)) C))
(AC (link-type A C)))
(Bind
(VariableList
(TypedVariable A var-type)
(TypedVariable B var-type)
(TypedVariable C var-type))
(And
(Present
AB
ABC
ANBC)
(Not (Identical A C)))
(ExecutionOutput
(GroundedSchema "scm: full-deduction")
(List
AC
AB
ABC
ANBC)))))
(define (full-deduction conclusion . premises)
(if (= (length premises) 3)
(let*
((AC conclusion)
(AB (list-ref premises 0))
(ABC (list-ref premises 1))
(ANBC (list-ref premises 2))
(ABs (cog-mean AB))
(ABc (cog-confidence AB))
(ABCs (cog-mean ABC))
(ABCc (cog-confidence ABC))
(ANBCs (cog-mean ANBC))
(ANBCc (cog-confidence ANBC))
(alpha 0.9)
(ACs (+ (* ABs ABCs) (* (- 1 ABs) ANBCs)))
(ACc (* alpha (min ABc ABCc ANBCc))))
(cog-merge-hi-conf-tv! AC (stv ACs ACc)))))
(define full-deduction-inheritance-rule
(let ((var-type (TypeChoice
(TypeNode "ConceptNode")
(TypeNode "AndLink")
(TypeNode "OrLink")
(TypeNode "NotLink"))))
(gen-full-deduction-rule InheritanceLink var-type)))
(define full-deduction-implication-rule
(let ((var-type (TypeChoice
(TypeNode "PredicateNode")
(TypeNode "LambdaLink")
(TypeNode "AndLink")
(TypeNode "OrLink")
(TypeNode "NotLink"))))
(gen-full-deduction-rule ImplicationLink var-type)))
(define full-deduction-subset-rule
(let ((var-type (TypeChoice
(TypeNode "ConceptNode")
(TypeNode "AndLink")
(TypeNode "OrLink")
(TypeNode "NotLink"))))
(gen-full-deduction-rule SubsetLink var-type)))
(define full-deduction-inheritance-rule-name
(DefinedSchemaNode "full-deduction-inheritance-rule"))
(DefineLink full-deduction-inheritance-rule-name
full-deduction-inheritance-rule)
(define full-deduction-implication-rule-name
(DefinedSchemaNode "full-deduction-implication-rule"))
(DefineLink full-deduction-implication-rule-name
full-deduction-implication-rule)
(define full-deduction-subset-rule-name
(DefinedSchemaNode "full-deduction-subset-rule"))
(DefineLink full-deduction-subset-rule-name
full-deduction-subset-rule)