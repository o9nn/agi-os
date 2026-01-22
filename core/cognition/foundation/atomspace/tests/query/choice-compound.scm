(use-modules (opencog) (opencog exec))
(State (List (Concept "Andrew") (Predicate "called robot")) (Concept "true"))
(State (List (Concept "Betty") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Betty") (Predicate "has crate")) (Concept "false"))
(State (List (Concept "Cathy") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Cathy") (Predicate "has crate")) (Concept "true"))
(State (List (Concept "Cathy") (Predicate "crate is full")) (Concept "true"))
(State (List (Concept "Deborah") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Edward") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Edward") (Predicate "has crate")) (Concept "true"))
(State (List (Concept "Frank") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Frank") (Predicate "has crate")) (Concept "true"))
(State (List (Concept "Frank") (Predicate "crate is full")) (Concept "false"))
(State (List (Concept "George") (Predicate "called robot")) (Concept "true"))
(State (List (Concept "George") (Predicate "movement")) (Concept "approaching"))
(define answer
	(Set (ConceptNode "Andrew") (ConceptNode "Betty") (ConceptNode "Cathy")))
(define who-needs-help?
	(Get
		(TypedVariable (Variable "picker") (Type "Concept"))
		(And
			(Absent
				(State
					(List (Variable "picker") (Predicate "movement"))
					(Concept "approaching")))
			(Choice
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "true")))
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "false"))
					(State
						(List (Variable "picker") (Predicate "has crate"))
						(Concept "false")))
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "false"))
					(State
						(List (Variable "picker") (Predicate "has crate"))
						(Concept "true"))
					(State
						(List (Variable "picker") (Predicate "crate is full"))
						(Concept "true")))
			))))