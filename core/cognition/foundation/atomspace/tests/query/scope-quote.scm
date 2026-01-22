(use-modules (opencog))
(use-modules (opencog exec))
(define p-lamb
  (LambdaLink
    (TypedVariableLink
      (VariableNode "$Xaaaa")
      (TypeNode "ConceptNode"))
    (PredicateNode "P")))
(define q-lamb
  (LambdaLink
    (TypedVariableLink
      (VariableNode "$Xbee")
      (TypeNode "ConceptNode"))
    (EvaluationLink
      (PredicateNode "Q")
      (VariableNode "$Xbee"))))
(ListLink p-lamb q-lamb)
(AndLink p-lamb q-lamb)
(define A1-lamb
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableNode "$TyVs-one"))
      (UnquoteLink
        (VariableNode "$A1")))))
(define A2-lamb
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableNode "$TyVs-two"))
      (UnquoteLink
        (VariableNode "$A2")))))
(define blist
  (BindLink
    (VariableList
      (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (VariableNode "$A1")
      (VariableNode "$A2")
    )
    (ListLink
      A1-lamb
      A2-lamb)
    (OrderedLink
      A1-lamb
      A2-lamb))
)
(define bland
  (BindLink
    (VariableList
      (TypedVariableLink
        (VariableNode "$TyVs-one")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (TypedVariableLink
        (VariableNode "$TyVs-two")
        (TypeChoice
          (TypeNode "TypedVariableLink")
          (TypeNode "VariableNode")
          (TypeNode "VariableList")))
      (VariableNode "$A1")
      (VariableNode "$A2")
    )
    (QuoteLink
      (AndLink
        (LambdaLink
          (UnquoteLink
            (VariableNode "$TyVs-one"))
          (UnquoteLink
            (VariableNode "$A1")))
        (LambdaLink
          (UnquoteLink
            (VariableNode "$TyVs-two"))
          (UnquoteLink
            (VariableNode "$A2")))))
    (UnorderedLink
      A1-lamb
      A2-lamb))
)