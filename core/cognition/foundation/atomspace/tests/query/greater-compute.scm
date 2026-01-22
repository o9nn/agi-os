(use-modules (opencog))
(use-modules (opencog exec))
(EvaluationLink
(PredicateNode "ergs")
(ListLink
(ConceptNode "Ken")
(NumberNode  10)
)
)
(EvaluationLink
(PredicateNode "ergs")
(ListLink
(ConceptNode "Peter")
(NumberNode  8)
)
)
(EvaluationLink
(PredicateNode "ergs")
(ListLink
(ConceptNode "Linas")
(NumberNode  4)
)
)
(EvaluationLink
(PredicateNode "ergs")
(ListLink
(ConceptNode "Joe Novice")
(NumberNode  1)
)
)
(define (eff x)
(NumberNode (sqrt (cog-number x)))
)
(define (crash-b who quant)
(EvaluationLink
(PredicateNode "power")
(ListLink
who
(cog-execute! (TimesLink quant quant quant))
)
)
)
(define (threshold)
(BindLink
(VariableList
(VariableNode "$who")
(VariableNode "$how_much")
)
(AndLink
(EvaluationLink
(PredicateNode "ergs")
(ListLink
(VariableNode "$who")
(VariableNode "$how_much")
)
)
(GreaterThanLink
(PlusLink
(TimesLink
(NumberNode 1.4)
(VariableNode "$how_much")
(VariableNode "$how_much")
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: eff")
(ListLink
(VariableNode "$how_much")
)
)
)
(NumberNode 142)
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: crash-b")
(ListLink
(VariableNode "$who")
(VariableNode "$how_much")
)
)
)
)