(use-modules (opencog) (opencog exec))
(Member
	(Evaluation
		(Predicate "has_name")
		(List (Concept "node1") (Concept "name1")))
	(Concept "node2"))
(define qunify
	(Query
		(And
			(Member
				(Evaluation (Predicate "has_name") (Variable "Y"))
				(Concept "node2"))
			(Equal
				(Variable "Y")
				(List (Variable "N") (Concept "name1"))))
		(Variable "Y")))
(define expected
	(List (Concept "node1") (Concept "name1")))