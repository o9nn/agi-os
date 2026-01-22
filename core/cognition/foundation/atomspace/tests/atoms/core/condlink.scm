(use-modules (opencog) (opencog exec))
(define single
(CondLink
(GreaterThanLink
(PlusLink
(NumberNode 2)
(NumberNode 2))
(TimesLink
(NumberNode 3)
(NumberNode 2)))
(PlusLink (Number 3)(Number 2))
(FalseLink )
(PlusLink (Number 1)(Number 2))
(PlusLink (Number 1)(Number -2)))
)
(define nondefault
(CondLink
(GreaterThanLink
(PlusLink
(NumberNode 2)
(NumberNode 2))
(TimesLink
(NumberNode 1)
(NumberNode 2)))
(PlusLink (Number 3)(Number 2))
(FalseLink )
(PlusLink (Number 1)(Number 2)))
)
(define listwrapped
(CondLink
(ListLink
(GreaterThanLink
(NumberNode 3)
(NumberNode 4))
(NumberNode 0))
(ListLink
(GreaterThanLink
(NumberNode 4)
(NumberNode 3))
(NumberNode 1))
(ListLink
(TrueLink )
(NumberNode 2)))
)
(define (grounded_cond1 ) (cog-new-stv 0 0))
(define (grounded_cond2 ) (cog-new-stv 1 1))
(define grounded-cond
(CondLink
(Evaluation
(GroundedPredicate "scm:grounded_cond1")
(List ))
(NumberNode 1)
(Evaluation
(GroundedPredicate "scm:grounded_cond2")
(List ))
(NumberNode 2)))
(define sub-expr
(AndLink
(PredicateNode "$a")
(CondLink
(EqualLink (Variable "$knob-2") (NumberNode 1))
(PredicateNode "$b")
(NotLink (PredicateNode "$b")))))
(DefineLink
(DefinedSchema "rep")
(LambdaLink
(VariableList
(Variable "$knob-1")
(Variable "$knob-2"))
(CondLink
(EqualLink (Variable "$knob-1") (NumberNode 1))
sub-expr
(NotLink sub-expr))))
(define put-00 (Put (DefinedSchema "rep") (List (Number 0) (Number 0))))
(define put-01 (Put (DefinedSchema "rep") (List (Number 0) (Number 1))))
(define put-10 (Put (DefinedSchema "rep") (List (Number 1) (Number 0))))
(define put-11 (Put (DefinedSchema "rep") (List (Number 1) (Number 1))))
*unspecified*