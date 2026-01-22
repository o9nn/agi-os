(use-modules (opencog) (opencog exec))
(define tv-stream
(FormulaTruthValue
(Minus
(Number 1)
(Times
(StrengthOf (Concept "A"))
(StrengthOf (Concept "B"))))
(Times
(ConfidenceOf (Concept "A"))
(ConfidenceOf (Concept "B")))))
(display tv-stream) (newline)
(cog-value->list tv-stream)
(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(cog-set-tv! (Concept "B") (stv 0.4 0.7))
(cog-value->list tv-stream)
(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(cog-value->list tv-stream)
(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(cog-value->list tv-stream)
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
(cog-set-tv! (Concept "A") (stv 0.3 0.7))
(cog-set-tv! (Concept "B") (stv 0.4 0.6))
(cog-evaluate! evlnk)
(cog-tv evlnk)
(define ev-stream (FormulaTruthValue evlnk))
(display ev-stream) (newline)
(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(cog-value->list ev-stream)
(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(cog-value->list ev-stream)
(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(cog-value->list ev-stream)
(define a-implies-b (Implication (Concept "A") (Concept "B")))
(cog-set-tv! a-implies-b tv-stream)
(cog-tv a-implies-b)
(format #t "A implies B has strength ~6F and confidence ~6F\n"
(cog-mean a-implies-b) (cog-confidence a-implies-b))
(cog-set-tv! (Concept "A") (stv 0.4 0.2))
(cog-set-tv! (Concept "B") (stv 0.7 0.8))
(format #t "A implies B has strength ~6F and confidence ~6F\n"
(cog-tv-mean (cog-tv a-implies-b))
(cog-tv-confidence (cog-tv a-implies-b)))
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
(cog-execute!
(SetTV
(Implication (Concept "A") (Concept "B"))
(DefinedPredicate "dynamic example")
(Concept "A") (Concept "B")))
(cog-tv a-implies-b)
(cog-set-tv! (Concept "A") (stv 0.1 0.9))
(cog-set-tv! (Concept "B") (stv 0.1 0.9))
(format #t "A implies B has strength ~6F and confidence ~6F\n"
(cog-mean a-implies-b) (cog-confidence a-implies-b))
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define akey (Predicate "some key"))
(define bkey (Predicate "other key"))
(cog-set-value! foo akey (RandomStream 5))
(cog-value foo akey)
(cog-execute! (StreamValueOf foo akey))
(define fstream (FormulaStream (Plus (Number 10) (FloatValueOf foo akey))))
(cog-set-value! bar bkey fstream)
(cog-value bar bkey)
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))