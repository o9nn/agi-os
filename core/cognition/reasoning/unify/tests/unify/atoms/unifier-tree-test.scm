(use-modules (opencog) (opencog exec))
(use-modules (opencog unify))
(use-modules (opencog test-runner))
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
(TypedVariable (Variable "$P") (Type 'ConceptNode))
(TypedVariable (Variable "$Q") (Type 'ConceptNode)))
(SequentialAnd
(Implication (Variable "$P") (Variable "$Q"))
(Variable "$P"))
(Variable "$Q")))
(define rule-reduce (UnifyReduce
(ConclusionOf intro)
(PremiseOf elim (Number 0))
(Rule
(SequentialAnd
(PremiseOf intro)
(PremiseOf elim (Number 1)))
(ConclusionOf elim))))
(opencog-test-runner)
(test-begin "rule-reduce")
(define expected
(LinkValue
(Rule
(TypedVariable (Variable "$B") (Type 'ConceptNode))
(SequentialAnd
(Variable "$B")
(Variable "$B"))
(Variable "$B"))))
(test-assert "minimal-implication"
(equal? expected (cog-execute! rule-reduce)))
(test-end "rule-reduce")
(opencog-test-end)