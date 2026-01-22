(use-modules (opencog))
(use-modules (opencog exec))
(InheritanceLink
(ConceptNode "battery")
(ConceptNode "electrical device"))
(InheritanceLink
(ConceptNode "transistor")
(ConceptNode "electrical device"))
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(ConceptNode "battery")
(VariableNode "car")))
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(ConceptNode "transistor")
(VariableNode "phone")))
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(ConceptNode "windsheild")
(VariableNode "car")))
(DefineLink
(DefinedPredicateNode "Electrical Part Of")
(PresentLink
(InheritanceLink
(VariableNode "$x")
(ConceptNode "electrical device"))
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(VariableNode "$x")
(VariableNode "$y")))))
(DefineLink
(DefinedPredicateNode "Elect-Part bound")
(LambdaLink
(VariableList (VariableNode "$x") (VariableNode "$y"))
(PresentLink
(InheritanceLink
(VariableNode "$x")
(ConceptNode "electrical device"))
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(VariableNode "$x")
(VariableNode "$y"))))))
(DefineLink
(DefinedPredicateNode "Electrical Thing")
(InheritanceLink
(VariableNode "$x")
(ConceptNode "electrical device")))
(DefineLink
(DefinedPredicateNode "Part-whole Relation")
(EvaluationLink
(PredicateNode "PartOf")
(ListLink
(VariableNode "$x")
(VariableNode "$y"))))
(define get-elect
(GetLink (DefinedPredicateNode "Electrical Part Of")))
(define get-elect-bound
(GetLink (DefinedPredicateNode "Elect-Part bound")))
(define get-parts
(GetLink
(AndLink
(DefinedPredicateNode "Electrical Thing")
(DefinedPredicateNode "Part-whole Relation"))))