(use-modules (opencog) (opencog exec))
(define A (Concept "A"))
(define B (Concept "B"))
(define fututv
	(FormulaTruthValue
		(FormulaPredicate
			(Minus
				(Number 1)
				(Times (StrengthOf A) (StrengthOf B)))
			(Times (ConfidenceOf A) (ConfidenceOf B)))))
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
(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))
(define tv-stream (FormulaTruthValue evlnk))
(define a-implies-b (Implication (Concept "A") (Concept "B")))
(cog-execute!
	(SetTV
		(Implication (Concept "A") (Concept "B"))
		(PromisePredicate
			(FormulaPredicate
				(Minus
					(Number 1)
					(Times
						(StrengthOf (Concept "A"))
						(StrengthOf (Concept "B"))))
				(Times
					(ConfidenceOf (Concept "A"))
					(ConfidenceOf (Concept "B")))))))
(DefineLink
   (DefinedPredicate "dynamic example")
   (FormulaPredicate
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))
(define p-implies-q (Implication (Concept "P") (Concept "Q")))
(cog-execute!
	(SetTV
		(Implication (Concept "P") (Concept "Q"))
		(DefinedPredicate "dynamic example")
		(Concept "A") (Concept "B")))
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define akey (Predicate "some key"))
(define bkey (Predicate "other key"))
(cog-set-value! foo akey (FloatValue 1 2 3 4 5))
(define fstream (FormulaStream (Plus (Number 10) (FloatValueOf foo akey))))
(cog-set-value! bar bkey fstream)