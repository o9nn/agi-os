(use-modules (opencog) (opencog exec))
(use-modules (opencog unify))
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
(cog-execute! (VardeclOf intro))
(cog-execute! (PremiseOf intro))
(cog-execute! (ConclusionOf intro))
(cog-execute! (VardeclOf elim))
(cog-execute! (PremiseOf elim (Number 0)))
(cog-execute! (PremiseOf elim (Number 1)))
(cog-execute! (ConclusionOf elim))
(cog-execute! (Unifier
(ConclusionOf intro)
(PremiseOf elim (Number 0))
(List (Variable "$P") (Variable "$Q"))))
(define rule-union (Unifier
(ConclusionOf intro)
(PremiseOf elim (Number 0))
(Rule
(SequentialAnd
(PremiseOf intro)
(PremiseOf elim (Number 1)))
(ConclusionOf elim))))
(define proof-tree-set (cog-execute! rule-union))
(define proof-tree (cog-outgoing-atom proof-tree-set 0))
(cog-execute! proof-tree)
(define rule-reduce (UnifyReduce
(ConclusionOf intro)
(PremiseOf elim (Number 0))
(Rule
(SequentialAnd
(PremiseOf intro)
(PremiseOf elim (Number 1)))
(ConclusionOf elim))))
(cog-execute! rule-reduce)