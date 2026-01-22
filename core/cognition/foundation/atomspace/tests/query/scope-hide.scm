(use-modules (opencog))
(use-modules (opencog exec))
(define forall
	(ForAllLink
		(VariableNode "$X")
		(EvaluationLink (Predicate "P") (VariableNode "$X"))))
(define getx (Get (LocalQuote (ForAllLink (Variable "$X") (Variable "$B")))))
(define getv (Get (LocalQuote (ForAllLink (Variable "$V") (Variable "$B")))))
(MemberLink
  (ConceptNode "ChurchOfEngland")
  (ConceptNode "AnglicanChurch")
)
(define rule
   (BindLink
      (TypedVariableLink
         (VariableNode "?C")
         (TypeChoice
            (TypeNode "ConceptNode")
            (TypeNode "SchemaNode")
            (TypeNode "PredicateNode")
         )
      )
      (MemberLink
         (VariableNode "?C")
         (ConceptNode "AnglicanChurch")
      )
      (RuleLink
         (TypedVariableLink
            (VariableNode "?C")
            (TypeChoice
               (TypeNode "ConceptNode")
               (TypeNode "SchemaNode")
               (TypeNode "PredicateNode")
            )
         )
         (MemberLink
            (VariableNode "?C")
            (ConceptNode "AnglicanChurch")
         )
         (EvaluationLink
            (PredicateNode "subOrganization")
            (ListLink
               (VariableNode "?C")
               (ConceptNode "ChurchOfEngland")
            )
         )
      )
   )
)