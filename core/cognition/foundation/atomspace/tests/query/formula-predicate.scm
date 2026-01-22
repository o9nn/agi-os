(use-modules (opencog) (opencog exec))
(Member
(Evaluation
(Predicate "has_name")
(List (Concept "node1") (Concept "name1")))
(Concept "node2"))
(define ans (List (Concept "node1") (Concept "name1")))
(define q-basic (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Equal (Variable "Y")
(List (Variable "N") (Concept "name1"))))
(Variable "Y")))
(define qi-basic (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Identical (Variable "Y")
(List (Variable "N") (Concept "name1"))))
(Variable "Y")))
(define qe1 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(TypedVariable (Variable "Y")
(Signature (List (Type 'Concept) (Concept "name1"))))
(Evaluation
(FormulaPredicate
(Minus (Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y"))))
(Variable "Y")))
(Variable "Y")))
(define qe2 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Equal (Variable "Y")
(List (Variable "N") (Concept "name1")))
(Evaluation
(FormulaPredicate
(Minus (Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y"))))
(Variable "Y")))
(Variable "Y")))
(define qe2i (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Identical (Variable "Y")
(List (Variable "N") (Concept "name1")))
(Evaluation
(FormulaPredicate
(Minus (Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y"))))
(Variable "Y")))
(Variable "Y")))
(define qe3 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Equal (Variable "Y")
(List (Variable "N") (Concept "name1")))
(Evaluation
(DefinedPredicate "pred1")
(Variable "Y")))
(Variable "Y")))
(define qe3i (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(Identical (Variable "Y")
(List (Variable "N") (Concept "name1")))
(Evaluation
(DefinedPredicate "pred1")
(Variable "Y")))
(Variable "Y")))
(DefineLink
(DefinedPredicate "pred1")
(FormulaPredicate
(Minus (Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y")))))
(define qe4 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(TypedVariable (Variable "Y")
(Signature (List (Type 'Concept) (Concept "name1"))))
(Evaluation
(DefinedPredicate "pred1")
(Variable "Y")))
(Variable "Y")))
(define qe5 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(TypedVariable (Variable "Y")
(Signature (List (Type 'Concept) (Concept "name1"))))
(GreaterThan
(StrengthOf (Variable "Y")) (Number 0.5)))
(Variable "Y")))
(define qe6 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(TypedVariable (Variable "Y")
(Signature (List (Type 'Concept) (Concept "name1"))))
(GreaterThan
(StrengthOf
(Evaluation
(DefinedPredicate "pred1")
(Variable "Y")))
(Number 0.5)))
(Variable "Y")))
(define qe7 (Query
(And
(Member
(Evaluation (Predicate "has_name") (Variable "Y"))
(Concept "node2"))
(TypedVariable (Variable "Y")
(Signature (List (Type 'Concept) (Concept "name1"))))
(GreaterThan
(StrengthOf
(Evaluation
(FormulaPredicate
(Minus (Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y"))))
(Variable "Y")))
(Number 0.5)))
(Variable "Y")))