(use-modules (opencog) (opencog exec))
(define word-list-a
	(LinkValue (Concept "this") (Concept "is") (Concept "a") (Concept "test")))
(define word-list-b
	(LinkValue (Concept "moar") (Concept "stuffs") (Concept "ok")))
(define edge-a1 (Edge (Bond "pair") (List (Concept "this") (Concept "is"))))
(define edge-a2 (Edge (Bond "pair") (List (Concept "is") (Concept "test"))))
(define edge-a3 (Edge (Bond "pair") (List (Concept "a") (Concept "test"))))
(define edge-b1 (Edge (Bond "pear") (List (Concept "moar") (Concept "stuffs"))))
(define edge-b2 (Edge (Bond "pear") (List (Concept "moar") (Concept "ok"))))
(define tree-list
	(LinkValue
		(LinkValue
			(LinkValue (Concept "sentence") word-list-a)
			(LinkValue (Concept "parse") edge-a1 edge-a2 edge-a3))
		(LinkValue
			(LinkValue (Concept "sentence") word-list-b)
			(LinkValue (Concept "parse") edge-b1 edge-b2))))
(cog-set-value!
	(Node "some place") (Predicate "some key") tree-list)
(define get-parse-sentences
	(Filter
		(Lambda
			(Variable "$x")
			(LinkSignature
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "sentence")
					(Variable "$x"))
				(Type 'LinkValue)))
		(ValueOf (Node "some place") (Predicate "some key")))
)
(cog-execute! get-parse-sentences)
(define get-parse-edges
	(Filter
		(Lambda
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x"))))
		(ValueOf (Node "some place") (Predicate "some key")))
)
(cog-execute! get-parse-edges)
(define rewrite-parse-edges
	(Filter
		(Rule
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x")))
			(OrderedLink (Concept "bunch of edges") (Glob "$x")))
		(ValueOf (Node "some place") (Predicate "some key")))
)
(cog-execute! rewrite-parse-edges)
(define (incr-cnt edge)
	(SetValue edge (Predicate "count")
		(Plus (Number 0 0 1)
			(FloatValueOf edge (Predicate "count")
				(FloatValueOf (Number 0 0 0))))))
(define (extract stuff)
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(incr-cnt (Variable "$edge")))
		stuff))
(define increment-parse-edges
	(Filter
		(Rule
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x")))
			(extract (Glob "$x")))
		(ValueOf (Node "some place") (Predicate "some key")))
)
(cog-execute! increment-parse-edges)
(define e (Edge (Bond "pair") (List (Concept "this") (Concept "is"))))
(cog-keys e)
(cog-value e (Predicate "count"))
(cog-execute! increment-parse-edges)
(cog-value e (Predicate "count"))
(cog-execute! increment-parse-edges)
(cog-value e (Predicate "count"))