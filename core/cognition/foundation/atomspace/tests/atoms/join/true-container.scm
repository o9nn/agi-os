(use-modules (opencog) (opencog exec))
(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))
(Similarity (Concept "foo") (Concept "bar"))
(define min-top
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(TypeChoice
			(Type 'EvaluationLink)
			(Type 'SimilarityLink))))
(define max-top
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(TypeChoice
			(Type 'EvaluationLink)
			(Type 'SimilarityLink))))