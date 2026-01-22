(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "rule-test")
(test-begin tname)
(define intro
(Rule
(VariableList
(TypedVariable (Variable "$A") (Type 'ConceptNode))
(TypedVariable (Variable "$B") (Type 'ConceptNode)))
(Variable "$B")
(Implication (Variable "$A") (Variable "$B"))))
(define elim
(Rule
(VariableList
(TypedVariable (Variable "$A") (Type 'ConceptNode))
(TypedVariable (Variable "$B") (Type 'ConceptNode)))
(SequentialAnd
(Implication (Variable "$A") (Variable "$B"))
(Variable "$A"))
(Variable "$B")))
(define var-ab
(VariableList
(TypedVariable (Variable "$A") (Type 'ConceptNode))
(TypedVariable (Variable "$B") (Type 'ConceptNode))))
(define lamb-b
(Lambda
(TypedVariable (Variable "$B") (Type 'ConceptNode))
(Variable "$B")))
(define lamb-impl
(Lambda
(VariableList
(TypedVariable (Variable "$A") (Type 'ConceptNode))
(TypedVariable (Variable "$B") (Type 'ConceptNode)))
(Implication
(Variable "$A")
(Variable "$B"))))
(test-assert "vardecl intro" (equal? (cog-execute! (VardeclOf intro)) var-ab))
(test-assert "premise intro" (equal? (cog-execute! (PremiseOf intro)) lamb-b))
(test-assert "conclud intro" (equal? (cog-execute! (ConclusionOf intro)) lamb-impl))
(test-assert "vardecl elim" (equal? (cog-execute! (VardeclOf elim)) var-ab))
(test-assert "premi-1 elim" (equal? (cog-execute! (PremiseOf elim (Number 0))) lamb-impl))
(test-assert "premi-2 elim" (equal? (cog-execute! (PremiseOf elim (Number 1))) lamb-b))
(test-assert "conclud elim" (equal? (cog-execute! (ConclusionOf elim)) lamb-b))
(test-end tname)
(opencog-test-end)