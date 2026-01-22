(define (print-ownership)
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$person")
(TypeNode "FeatureNode")
)
(VariableNode "$nationality")
(VariableNode "$house")
(VariableNode "$pet")
)
(AndLink
(EvaluationLink
(PredicateNode "Nationality")
(ListLink
(VariableNode "$person")
(VariableNode "$nationality")
)
)
(EvaluationLink
(PredicateNode "LivesIn")
(ListLink
(VariableNode "$person")
(VariableNode "$house")
)
)
(EvaluationLink
(PredicateNode "KeepsPet")
(ListLink
(VariableNode "$person")
(VariableNode "$pet")
)
)
)
(OrderedLink
(VariableNode "$person")
(VariableNode "$nationality")
(VariableNode "$house")
(VariableNode "$pet")
)
)
)
(define (print-results)
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$person")
(TypeNode "FeatureNode")
)
(VariableNode "$pred")
(TypedVariableLink
(VariableNode "$attr")
(TypeNode "ConceptNode")
)
)
(AndLink
(EvaluationLink
(VariableNode "$pred")
(ListLink
(VariableNode "$person")
(VariableNode "$attr")
)
)
)
(OrderedLink
(VariableNode "$person")
(VariableNode "$pred")
(VariableNode "$attr")
)
)
)