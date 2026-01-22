(use-modules (opencog) (opencog exec))
(Define (DefinedSchema "rand-transpose")
(RandomChoice
(List (Number 0.7) (Number 0.3))
(List (Concept "A") (Concept "B"))))
(Define (DefinedSchema "rand-set-choice")
(RandomChoice
(SetLink
(List (Number 0.7) (Concept "A"))
(List (Number 0.3) (Concept "B")))))
(Define (DefinedSchema "randy")
(RandomChoice
(GetLink
(VariableList (VariableNode "$prob") (VariableNode "$expr"))
(AndLink
(EvaluationLink
(PredicateNode "Emotion-expression")
(ListLink (ConceptNode "wake-up") (VariableNode "$expr")))
(StateLink
(ListLink
(ConceptNode "wake-up")
(VariableNode "$expr"))
(VariableNode "$prob"))))
))
(Evaluation (Predicate "Emotion-expression")
(ListLink (Concept "wake-up") (Concept "A")))
(State (ListLink (ConceptNode "wake-up") (Concept "A")) (Number 0.7))
(Evaluation (Predicate "Emotion-expression")
(ListLink (Concept "wake-up") (Concept "B")))
(State (ListLink (ConceptNode "wake-up") (Concept "B")) (Number 0.3))
(State (Anchor "sum-A") (Number 0))
(State (Anchor "sum-B") (Number 0))
(Define (DefinedPredicate "counter")
(SequentialOr
(SequentialAnd
(Equal (DefinedSchema "randy") (Concept "A"))
(True (Put
(State (Anchor "sum-A") (Variable "$x"))
(Plus (Number 1)
(Get (State (Anchor "sum-A") (Variable "$y")))))))
(True (Put
(State (Anchor "sum-B") (Variable "$x"))
(Plus (Number 1)
(Get (State (Anchor "sum-B") (Variable "$y"))))))))
(State (Anchor "loop-count") (Number 0))
(Define (DefinedPredicate "loop a lot of times")
(SequentialAnd
(DefinedPredicate "counter")
(TrueLink (PutLink
(State (Anchor "loop-count") (Variable "$x"))
(Plus (Number 1) (Get (State (Anchor "loop-count") (Variable "$x"))))))
(GreaterThan
(Number 3000)
(Get (State (Anchor "loop-count") (Variable "$x"))))
(DefinedPredicate "loop a lot of times")))
(Define (DefinedSchema "ratio")
(Divide
(Get (State (Anchor "sum-A") (Variable "$x")))
(Get (State (Anchor "sum-B") (Variable "$x")))))
(Define (DefinedPredicate "test")
(SequentialAnd
(GreaterThan (Number 2.5) (DefinedSchema "ratio"))
(GreaterThan (DefinedSchema "ratio") (Number 2.2))))
*unspecified*