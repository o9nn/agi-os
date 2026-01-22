(use-modules (opencog) (opencog exec))
(Inheritance
(Concept "battery")
(Concept "electrical device"))
(Inheritance
(Concept "transistor")
(Concept "electrical device"))
(Evaluation
(Predicate "PartOf")
(List
(Concept "battery")
(Variable "car")))
(Evaluation
(Predicate "PartOf")
(List
(Concept "transistor")
(Variable "phone")))
(Evaluation
(Predicate "PartOf")
(List
(Concept "windshield")
(Variable "car")))
(DefineLink
(DefinedPredicate "Electrical Part Of")
(Present
(Inheritance
(Variable "$x")
(Concept "electrical device"))
(Evaluation
(Predicate "PartOf")
(List
(Variable "$x")
(Variable "$y")))))
(define get-elect
(Get (DefinedPredicate "Electrical Part Of")))
(cog-execute! get-elect)
(DefineLink
(DefinedPredicate "Electrical Thing")
(Inheritance
(Variable "$x")
(Concept "electrical device")))
(DefineLink
(DefinedPredicate "Part-whole Relation")
(Evaluation
(Predicate "PartOf")
(List
(Variable "$x")
(Variable "$y"))))
(define cnt 0)
(define (do-stuff atom)
(set! cnt (+ cnt 1))
(format #t "At count ~a found this part: ~a \n" cnt atom)
(stv 1 1))
(DefineLink
(DefinedPredicate "Counter Printer")
(Evaluation (GroundedPredicate "scm: do-stuff")
(List (Variable "$x"))))
(define get-electrical-parts
(Get
(And
(DefinedPredicate "Electrical Thing")
(DefinedPredicate "Part-whole Relation")
(DefinedPredicate "Counter Printer")
)))
(cog-execute! get-electrical-parts)