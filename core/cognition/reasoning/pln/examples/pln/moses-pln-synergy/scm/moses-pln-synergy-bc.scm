(use-modules (opencog ure))
(ure-logger-set-timestamp! #f)
(ure-logger-set-level! "debug")
(use-modules (opencog randgen))
(cog-randgen-set-seed! 100)
(load "moses-model.scm")
(load "background-knowledge.scm")
(load "pln-bc-config.scm")
(define target-12
  (ImplicationLink
    (PredicateNode "take-treatment-1")
    (LambdaLink
      (TypedVariableLink
        (VariableNode "$X")
        (TypeNode "ConceptNode")
      )
      (EvaluationLink
        (PredicateNode "take")
        (ListLink
          (VariableNode "$X")
          (ConceptNode "compound-A")
        )
      )
    )
  )
)
(pln-bc target-12)