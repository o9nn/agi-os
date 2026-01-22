(use-modules (opencog) (opencog exec))
(Set (Concept "A")
(Set (Concept "A") (Predicate "P") (Predicate "Q") (Predicate "R")))
(define indistinct-dim-one
(Bind
(Present (Set (Variable "$CPT")
(Set (Concept "A") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "B")
(Set (Concept "B") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "B") (Predicate "S") (Predicate "T") (Predicate "U")))
(define indistinct-dim-two
(Bind
(Present (Set (Variable "$CPT")
(Set (Concept "B") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "B") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "C")
(Set (Concept "C") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "C") (Predicate "S") (Predicate "T") (Predicate "U"))
(Set (Concept "C") (Predicate "V") (Predicate "W") (Predicate "X")))
(define indistinct-dim-three
(Bind
(Present (Set (Variable "$CPT")
(Set (Concept "C") (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Concept "C") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "C") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Set (Concept "D")
(Set (Concept "D") (Predicate "L") (Predicate "M") (Predicate "N"))
(Set (Concept "D") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "D") (Predicate "S") (Predicate "T") (Predicate "U"))
(Set (Concept "D") (Predicate "V") (Predicate "W") (Predicate "X")))
(define indistinct-dim-four
(Bind
(Present (Set (Variable "$CPT")
(Set (Concept "D") (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Concept "D") (Variable "$D") (Variable "$E") (Variable "$F"))
(Set (Concept "D") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "D") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$D") (Variable "$E") (Variable "$F")
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))