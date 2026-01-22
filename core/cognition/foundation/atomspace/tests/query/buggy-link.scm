(EvaluationLink
(PredicateNode "eat")
(ListLink
(ConceptNode "Bob")
(ConceptNode "Doughnut")
)
)
(EvaluationLink
(PredicateNode "eat")
(ListLink
(ConceptNode "Lily")
(ConceptNode "Cabbage")
)
)
(EvaluationLink
(PredicateNode "eat")
(ListLink
(VariableNode "$var_1")
(ConceptNode "Cabbage")
)
)
(EvaluationLink
(PredicateNode "like")
(ListLink
(ConceptNode "Lily")
(ConceptNode "apple")
)
)
(define bindy
(BindLink
(VariableList
(VariableNode "$var_1")
(VariableNode "$var_2")
)
(AndLink
(EvaluationLink
(VariableNode "$var_2")
(ListLink
(ConceptNode "Bob")
(ConceptNode "Doughnut")
)
)
(EvaluationLink
(VariableNode "$var_2")
(ListLink
(VariableNode "$var_1")
(ConceptNode "Cabbage")
)
)
(EvaluationLink
(PredicateNode "like")
(ListLink
(VariableNode "$var_1")
(ConceptNode "apple")
)
)
)
(ListLink
(VariableNode "$var_1")
(VariableNode "$var_2")
)
)
)