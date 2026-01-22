(use-modules (opencog) (opencog exec))
(Set (Concept "B")
(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Predicate "R") (Predicate "S") (Predicate "T")))
(define play-dim-two
(Bind
(Present (Set (Variable "$CPT")
(Set (Variable "$U") (Variable "$V") (Variable "$W"))
(Set (Variable "$W") (Variable "$X") (Variable "$Y"))))
(Associative
(Variable "$U") (Variable "$V") (Variable "$W")
(Variable "$X") (Variable "$Y"))))
(Set (Concept "C")
(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Predicate "R") (Predicate "S") (Predicate "T"))
(Set (Predicate "T") (Predicate "U") (Predicate "V")))
(define play-dim-three
(Bind
(Present (Set (Variable "$CPT")
(Set (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Variable "$C") (Variable "$D") (Variable "$E"))
(Set (Variable "$E") (Variable "$F") (Variable "$G"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$D") (Variable "$E") (Variable "$F")
(Variable "$G"))))
(Set (Concept "D")
(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
(Set (Predicate "R") (Predicate "S") (Predicate "T"))
(Set (Predicate "T") (Predicate "U") (Predicate "V"))
(Set (Predicate "V") (Predicate "W") (Predicate "X")))
(define play-dim-four
(Bind
(Present (Set (Variable "$CPT")
(Set (Variable "$A") (Variable "$B") (Variable "$C"))
(Set (Variable "$C") (Variable "$D") (Variable "$E"))
(Set (Variable "$E") (Variable "$F") (Variable "$G"))
(Set (Variable "$G") (Variable "$H") (Variable "$J"))))
(Associative
(Variable "$A") (Variable "$B") (Variable "$C")
(Variable "$D") (Variable "$E") (Variable "$F")
(Variable "$G") (Variable "$H") (Variable "$J"))))