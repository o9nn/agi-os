(use-modules (opencog) (opencog exec))
(define (show-eval-links)
(cog-map-type (lambda (h) (display h) #f) 'EvaluationLink))
(show-eval-links)
(define to-be-added
(Put
(Evaluation
(Predicate "some property") (Variable "$x"))
(ListLink
(Concept "thing A")
(Concept "B-dom-ness"))))
(show-eval-links)
(cog-execute! to-be-added)
(show-eval-links)
(define get-property
(Get (Evaluation (Predicate "some property") (Variable "$x"))))
(cog-execute! get-property)
(define remove-thing-ab
(Put
(Delete
(Evaluation (Predicate "some property") (Variable "$x")))
(ListLink (Concept "thing A") (Concept "B-dom-ness"))))
(cog-execute! remove-thing-ab)
(cog-execute! get-property)
(show-eval-links)
(cog-execute! to-be-added)
(cog-execute! get-property)
(cog-execute! remove-thing-ab)
(cog-execute! get-property)
(cog-execute! to-be-added)
(cog-execute! get-property)
(define remove-some-property
(PutLink
(DeleteLink
(EvaluationLink
(PredicateNode "some property")
(VariableNode "$x")))
(GetLink
(EvaluationLink
(PredicateNode "some property")
(VariableNode "$x")))))
(cog-execute! remove-some-property)
(cog-execute! get-property)
(cog-execute! to-be-added)
(cog-execute! get-property)
(cog-execute! remove-some-property)
(cog-execute! get-property)
(cog-execute! to-be-added)
(cog-execute! get-property)
(cog-execute! remove-some-property)
(cog-execute! get-property)
(StateLink
(PredicateNode "some property")
(ListLink
(ConceptNode "thing A")
(ConceptNode "alternative B")))
(define get-state
(GetLink
(StateLink
(PredicateNode "some property")
(VariableNode "$x"))))
(cog-execute! get-state)
(StateLink
(PredicateNode "some property")
(ListLink
(ConceptNode "thing A")
(ConceptNode "The V alternative")))
(cog-execute! get-state)
(StateLink
(PredicateNode "some property")
(ListLink
(ConceptNode "thing A")
(ConceptNode "first alternative again")))
(cog-execute! get-state)
(Define
(DefinedSchema "colored things")
(Lambda (Inheritance (Variable "$yyy") (Concept "color"))))
(cog-execute!
(PutLink
(DefinedSchema "colored things")
(Concept "green")))