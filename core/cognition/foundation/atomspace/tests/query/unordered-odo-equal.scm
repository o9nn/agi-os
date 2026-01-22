(use-modules (opencog) (opencog exec))
(List (Concept "B")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T")))
(define equ-dim-two
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "C")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T"))
	(Set (Concept "T") (Concept "U") (Concept "V")))
(define equ-dim-three
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$C") (Variable "$U"))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "D")
   (Set (Predicate "P") (Predicate "Q") (Predicate "R"))
   (Set (Predicate "R") (Predicate "S") (Predicate "T"))
   (Set (Predicate "T") (Predicate "U") (Predicate "V"))
   (Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define equ-dim-four
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$D") (Variable "$E") (Variable "$F"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$C") (Variable "$D"))
			(Equal (Variable "$F") (Variable "$U"))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))