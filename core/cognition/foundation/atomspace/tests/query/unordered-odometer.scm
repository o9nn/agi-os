(use-modules (opencog) (opencog exec))
(List (Concept "A")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R")))
(define odo-dim-one
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "B")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U")))
(define odo-dim-two
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "C")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define odo-dim-three
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "D")
	(Set (Predicate "L") (Predicate "M") (Predicate "N"))
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define odo-dim-four
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$D") (Variable "$E") (Variable "$F"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))