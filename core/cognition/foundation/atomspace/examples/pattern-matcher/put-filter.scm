(use-modules (opencog) (opencog exec))
(define filter-it
	(Put
		(TypedVariable (Variable "%x") (Type "ConceptNode"))
		(Variable "%x")
		(Set
			(Number     "42")
			(Concept    "foo")
			(Predicate  "biffle")
			(Evaluation (Predicate "foo") (Concept "thingy"))
			(Schema     "finagle")
			(Concept    "bar"))))
(cog-execute! filter-it)
(Set
	(Concept "foo")
	(Concept "bar"))
(define filter-links
	(Put
		(TypedVariable (Variable "%x") (Type "EvaluationLink"))
		(Variable "%x")
		(Set
			(Number     "42")
			(Concept    "foo")
			(Predicate  "biffle")
			(Evaluation (Predicate "foo") (Concept "thingy"))
			(Schema     "finagle")
			(Concept    "bar"))))
(cog-execute! filter-links)