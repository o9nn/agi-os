(use-modules (opencog) (opencog exec))
(EvaluationLink
   (PredicateNode "OpenPsi:Decrease")
   (ListLink
      (Node "OpenPsi: Energy-rule-xej90")
      (ConceptNode "OpenPsi:Energy")))
(EvaluationLink
   (PredicateNode "OpenPsi:Increase")
   (ListLink
      (Node "OpenPsi: Energy-rule-pG1ez")
      (ConceptNode "OpenPsi:Energy")))
(define get-nodes1
    (GetLink
       (TypedVariableLink
          (VariableNode "x")
          (TypeNode "Node"))
       (ChoiceLink
          (EvaluationLink
             (PredicateNode "OpenPsi:Increase")
             (ListLink
                (VariableNode "x")
                (ConceptNode "OpenPsi:Energy")))
          (EvaluationLink
             (PredicateNode "OpenPsi:Decrease")
             (ListLink
                (VariableNode "x")
                (ConceptNode "OpenPsi:Energy"))))))