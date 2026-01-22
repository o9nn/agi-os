(use-modules (opencog) (opencog exec))
(Concept "foo" (stv 0.3 0.7))
(define copy-tv
	(SetTV
		(Concept "bar")
		(TruthValueOf (Concept "foo"))))
(define product
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
(define set-eval
	(SetTV
		(Concept "bar")
		(Evaluation
			(DefinedPredicate "has a reddish color")
			(List (Concept "A") (Concept "B")))))
(define set-direct
	(SetTV
		(Concept "bar")
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))
(define (reddish-tv A B)
	(SimpleTruthValue
		(- 1 (*  (cog-mean A) (cog-mean B)))
		(*  (cog-confidence A) (cog-confidence B))))
(define set-gpn
	(SetTV
		(Concept "martian rock")
		(GroundedPredicate "scm: reddish-tv")
		(List (Concept "A") (Concept "B"))))
(*unspecified*)