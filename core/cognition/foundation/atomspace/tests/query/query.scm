(use-modules (opencog) (opencog exec))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "joke")))
(define query
(Query
(TypedVariable (Variable "$x") (Type 'ConceptNode))
(Evaluation
(Predicate "foobar")
(List (Concept "funny") (Variable "$x")))
(ListLink
(Anchor "*-query results-*")
(Implication (Variable "$x") (Concept "laughable")))
))