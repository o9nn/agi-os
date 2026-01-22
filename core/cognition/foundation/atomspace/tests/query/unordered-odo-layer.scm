(use-modules (opencog) (opencog exec))
(Set (Concept "A")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R")))
(define layer-dim-one
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "B")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U")))
(define layer-dim-two
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "C")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define layer-dim-three
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "D")
	(Set (Predicate "L") (Predicate "M") (Predicate "N"))
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define layer-dim-four
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$D") (Variable "$E") (Variable "$F"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))