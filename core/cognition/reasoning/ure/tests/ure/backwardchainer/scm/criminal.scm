(ImplicationScopeLink (stv .99 .99)
(VariableList
(TypedVariable
(Variable "$x")
(Type "ConceptNode"))
(TypedVariable
(Variable "$y")
(Type "ConceptNode"))
(TypedVariable
(Variable "$z")
(Type "ConceptNode")))
(AndLink
(InheritanceLink
(VariableNode "$x")
(ConceptNode "American"))
(InheritanceLink
(VariableNode "$y")
(ConceptNode "weapon"))
(EvaluationLink
(PredicateNode "sell")
(ListLink
(VariableNode "$x")
(VariableNode "$y")
(VariableNode "$z")))
(InheritanceLink
(VariableNode "$z")
(ConceptNode "hostile")))
(InheritanceLink
(VariableNode "$x")
(ConceptNode "criminal")))
(AndLink (stv .99 .99)
(InheritanceLink (stv .99 .99)
(ConceptNode "missile@123")
(ConceptNode "missile"))
(EvaluationLink (stv .99 .99)
(PredicateNode "own")
(ListLink
(ConceptNode "Nono")
(ConceptNode "missile@123"))))
(ImplicationScopeLink (stv .99 .99)
(TypedVariable
(Variable "$a")
(Type "ConceptNode"))
(AndLink
(InheritanceLink
(VariableNode "$a")
(ConceptNode "missile"))
(EvaluationLink
(PredicateNode "own")
(ListLink
(ConceptNode "Nono")
(VariableNode "$a"))))
(EvaluationLink
(PredicateNode "sell")
(ListLink
(ConceptNode "West")
(VariableNode "$a")
(ConceptNode "Nono"))))
(InheritanceLink (stv .99 .99)
(ConceptNode "missile")
(ConceptNode "weapon"))
(ImplicationScopeLink (stv .99 .99)
(TypedVariable
(Variable "$b")
(Type "ConceptNode"))
(EvaluationLink
(PredicateNode "enemy_of")
(ListLink
(VariableNode "$b")
(ConceptNode "America")))
(InheritanceLink
(VariableNode "$b")
(ConceptNode "hostile")))
(InheritanceLink (stv .99 .99)
(ConceptNode "West")
(ConceptNode "American"))
(EvaluationLink (stv .99 .99)
(PredicateNode "enemy_of")
(ListLink
(ConceptNode "Nono")
(ConceptNode "America")))