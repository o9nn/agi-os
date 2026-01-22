(use-modules (opencog))
(use-modules (opencog exec))
(Member (Concept "Tom") (Concept "ways and means"))
(Member (Concept "Joe") (Concept "ways and means"))
(Member (Concept "Hank") (Concept "ways and means"))
(Member (Concept "Dick") (Concept "agriculture"))
(List (Member (Concept "Tom") (Concept "Senator")))
(List (Member (Concept "Dick") (Concept "Senator")))
(List (Member (Concept "Joe") (Concept "Representative")))
(List (Member (Concept "Hank") (Concept "CEO")))
(define double
	(Bind
		(And
			(Choice
				(Member (Variable "$x") (Concept "ways and means"))
				(Member (Variable "$x") (Concept "agriculture")))
			(List
				(Choice
					(Member (Variable "$x") (Concept "Senator"))
					(Member (Variable "$x") (Concept "Representative")))))
		(Variable "$x")))