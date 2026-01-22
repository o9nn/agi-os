(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "filter-value-test")
(test-begin tname)
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
(define wrd-list (cog-execute! get-parse-sentences))
(test-assert "pair of sentences"
	(equal? wrd-list (LinkValue word-list-a word-list-b)))
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
(define edge-list (cog-execute! get-parse-edges))
(test-assert "edge list"
	(equal? edge-list
		(LinkValue
			(LinkValue edge-a1 edge-a2 edge-a3)
			(LinkValue edge-b1 edge-b2))))
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
(define key (Predicate "count"))
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 1)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 1)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 1)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 1)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 1)))
(cog-execute! increment-parse-edges)
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 2)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 2)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 2)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 2)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 2)))
(cog-execute! increment-parse-edges)
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 3)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 3)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 3)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 3)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 3)))
(test-end tname)
(opencog-test-end)