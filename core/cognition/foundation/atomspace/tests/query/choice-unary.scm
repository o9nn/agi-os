(use-modules (opencog) (opencog exec))
(EvaluationLink (PredicateNode "door A") (ConceptNode "locked"))
(EvaluationLink (PredicateNode "door A") (ConceptNode "closed"))
(EvaluationLink (PredicateNode "door B") (ConceptNode "unlocked"))
(EvaluationLink (PredicateNode "door B") (ConceptNode "closed"))
(EvaluationLink (PredicateNode "door C") (ConceptNode "unlocked"))
(EvaluationLink (PredicateNode "door C") (ConceptNode "open"))
(define (get-a) (GetLink
(AndLink
(ChoiceLink
(EvaluationLink (VariableNode "$door") (ConceptNode "locked")))
(ChoiceLink
(EvaluationLink (VariableNode "$door") (ConceptNode "closed")))
)))
(define (get-bc) (GetLink
(AndLink
(ChoiceLink
(EvaluationLink (VariableNode "$door") (ConceptNode "unlocked")))
(ChoiceLink
(EvaluationLink (VariableNode "$door") (VariableNode "$ajar")))
)))