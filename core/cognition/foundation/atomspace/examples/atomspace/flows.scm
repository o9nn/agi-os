(use-modules (opencog) (opencog exec))
(Concept "foo" (stv 0.3 0.7))
(cog-evaluate! (TruthValueOf (Concept "foo")))
(cog-execute!  (TruthValueOf (Concept "foo")))
(cog-execute!
(SetTV
(Concept "bar")
(TruthValueOf (Concept "foo"))))
(cog-tv (Concept "bar"))
(cog-execute! (SetTV (Concept "bar")
(FormulaPredicate (Number 0.2718) (Number 0.314))))
(cog-tv (Concept "bar"))
(cog-execute!
(SetTV
(Concept "bar")
(Times
(TruthValueOf (Concept "foo"))
(TruthValueOf (Concept "foo")))))
(DefineLink
(DefinedPredicate "has a reddish color")
(FormulaPredicate
(Minus
(Number 1)
(Times
(StrengthOf (Variable "$X"))
(StrengthOf (Variable "$Y"))))
(Times
(ConfidenceOf (Variable "$X"))
(ConfidenceOf (Variable "$Y")))))
(Concept "A" (stv 0.9 0.98))
(Concept "B" (stv 0.9 0.98))
(cog-execute!
(SetTV
(Concept "bar")
(Evaluation
(DefinedPredicate "has a reddish color")
(List (Concept "A") (Concept "B")))))
(Concept "A" (stv 0.8 0.9))
(cog-execute!
(SetTV
(Concept "bar")
(Evaluation
(DefinedPredicate "has a reddish color")
(List (Concept "A") (Concept "B")))))
(cog-execute!
(SetTV
(Concept "bar")
(DefinedPredicate "has a reddish color")
(List (Concept "A") (Concept "B"))))
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))
(cog-set-value! foo key (FloatValue 1 2 3 4 5))
(cog-execute! (ValueOf foo key))
(cog-execute! (SetValue bar kee (ValueOf foo key)))
(cog-execute! (ValueOf bar kee))
(cog-execute! (SetValue bar kee
(Times (FloatValueOf foo key) (FloatValueOf foo key))))
(cog-execute! (ValueOf bar kee))
(DefineLink
(DefinedSchema "triangle numbers")
(Lambda
(Variable "$X")
(Divide
(Times (Variable "$X") (Plus (Variable "$X") (Number 1)))
(Number 2))))
(cog-execute!
(SetValue bar kee
(DefinedSchema "triangle numbers")
(FloatValueOf foo key)))