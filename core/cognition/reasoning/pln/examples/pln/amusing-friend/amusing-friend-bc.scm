(ure-logger-set-level! "debug")
(load "kb.scm")
(load "pln-bc-config.scm")
(ImplicationLink (stv 1 1)
   (LambdaLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
         )
         (TypedVariableLink
            (VariableNode "$Y")
            (TypeNode "ConceptNode")
         )
      )
      (AndLink
         (EvaluationLink
            (PredicateNode "is-honest")
            (VariableNode "$X")
         )
         (EvaluationLink
            (PredicateNode "is-honest")
            (VariableNode "$Y")
         )
         (InheritanceLink
            (VariableNode "$X")
            (ConceptNode "human")
         )
         (InheritanceLink
            (VariableNode "$Y")
            (ConceptNode "human")
         )
         (EvaluationLink
            (PredicateNode "acquainted")
            (ListLink
               (VariableNode "$X")
               (VariableNode "$Y")
            )
         )
      )
   )
   (AndLink
      (LambdaLink
         (VariableList
            (TypedVariableLink
               (VariableNode "$X")
               (TypeNode "ConceptNode")
            )
            (TypedVariableLink
               (VariableNode "$Y")
               (TypeNode "ConceptNode")
            )
         )
         (AndLink
            (EvaluationLink
               (PredicateNode "is-honest")
               (VariableNode "$X")
            )
            (EvaluationLink
               (PredicateNode "is-honest")
               (VariableNode "$Y")
            )
         )
      )
      (LambdaLink
         (VariableList
            (TypedVariableLink
               (VariableNode "$X")
               (TypeNode "ConceptNode")
            )
            (TypedVariableLink
               (VariableNode "$Y")
               (TypeNode "ConceptNode")
            )
         )
         (AndLink
            (InheritanceLink
               (VariableNode "$X")
               (ConceptNode "human")
            )
            (InheritanceLink
               (VariableNode "$Y")
               (ConceptNode "human")
            )
            (EvaluationLink
               (PredicateNode "acquainted")
               (ListLink
                  (VariableNode "$X")
                  (VariableNode "$Y")
               )
            )
         )
      )
   )
)
(define step-9
(ImplicationLink
   (LambdaLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
         )
         (TypedVariableLink
            (VariableNode "$Y")
            (TypeNode "ConceptNode")
         )
      )
      (AndLink
         (EvaluationLink
            (PredicateNode "is-honest")
            (VariableNode "$X")
         )
         (EvaluationLink
            (PredicateNode "is-honest")
            (VariableNode "$Y")
         )
         (InheritanceLink
            (VariableNode "$X")
            (ConceptNode "human")
         )
         (InheritanceLink
            (VariableNode "$Y")
            (ConceptNode "human")
         )
         (EvaluationLink
            (PredicateNode "acquainted")
            (ListLink
               (VariableNode "$X")
               (VariableNode "$Y")
            )
         )
      )
   )
   (LambdaLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
         )
         (TypedVariableLink
            (VariableNode "$Y")
            (TypeNode "ConceptNode")
         )
      )
      (EvaluationLink
         (PredicateNode "will-be-friends")
         (ListLink
            (VariableNode "$X")
            (VariableNode "$Y")
         )
      )
   )
)
)
(pln-bc step-9)