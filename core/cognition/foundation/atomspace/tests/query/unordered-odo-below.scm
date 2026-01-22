(use-modules (opencog) (opencog exec))
(List (Concept "A")
(Set (Concept "A") (Predicate "P") (Predicate "Q") (Predicate "R")))
(define below-dim-one
(Bind
(Present (List (Variable "$CPT")
(Set (Concept "A") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "B")
(Set (Concept "B") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "B") (Predicate "S") (Predicate "T") (Predicate "U")))
(define below-dim-two
(Bind
(Present (List (Variable "$CPT")
(Set (Concept "B") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "B") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "C")
(Set (Concept "C") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "C") (Predicate "S") (Predicate "T") (Predicate "U"))
(Set (Concept "C") (Predicate "V") (Predicate "W") (Predicate "X")))
(define below-dim-three
(Bind
(Present (List (Variable "$CPT")
(Set (Concept "C") (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Concept "C") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "C") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))
(List (Concept "D")
(Set (Concept "D") (Predicate "L") (Predicate "M") (Predicate "N"))
(Set (Concept "D") (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Concept "D") (Predicate "S") (Predicate "T") (Predicate "U"))
(Set (Concept "D") (Predicate "V") (Predicate "W") (Predicate "X")))
(define below-dim-four
(Bind
(Present (List (Variable "$CPT")
(Set (Concept "D") (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Concept "D") (Variable "$D") (Variable "$E") (Variable "$F"))
(Set (Concept "D") (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Concept "D") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$D") (Variable "$E") (Variable "$F")
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y") (Variable "$Z"))))