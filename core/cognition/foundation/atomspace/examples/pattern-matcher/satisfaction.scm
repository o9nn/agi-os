(use-modules (opencog) (opencog exec))
(define satlink
(Satisfaction
(Evaluation
(Predicate "foobar")
(List
(Concept "funny")
(Variable "$x")))))
(Evaluation
(Predicate "foobar")
(List
(Concept "funny")
(Concept "thing")))
(cog-execute! satlink)
(define gnd-sat
(Satisfaction
(Anchor "please put groundings here")
(Evaluation
(Predicate "foobar")
(List
(Concept "funny")
(Variable "$x")))))
(cog-execute! gnd-sat)
(define anchr (Anchor "please put groundings here"))
(cog-incoming-by-type anchr 'Member)
(define gnd-decl-sat
(Satisfaction
(VariableList
(Variable "$x")
(Anchor "please put groundings here"))
(Evaluation
(Predicate "foobar")
(List
(Concept "funny")
(Variable "$x")))))
(cog-execute! gnd-decl-sat)
(cog-incoming-by-type anchr 'Member)
(define gnd2-sat
(Satisfaction
(VariableList
(Variable "$p")
(Variable "$x")
(Anchor "please put groundings here"))
(Evaluation
(Variable "$p")
(List
(Concept "funny")
(Variable "$x")))))
(cog-execute! gnd2-sat)
(cog-incoming-by-type anchr 'Member)
(define gnd2-get
(Get
(VariableList
(Variable "$p")
(Variable "$x"))
(Evaluation
(Variable "$p")
(List
(Concept "funny")
(Variable "$x")))))
(cog-execute! gnd2-get)