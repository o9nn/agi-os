(use-modules (opencog) (opencog exec))
(use-modules (opencog type-utils))
(Inheritance (Concept "foo") (Concept "bingo"))
(Inheritance (Concept "bar") (Concept "bingo"))
(Inheritance (Concept "baz") (Concept "bonk"))
(define get-foo
	(GetLink
		(TypedVariable (Variable "$x")
			(Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))
		(Variable "$x")
	)
)
(cog-execute! get-foo)
(define get-foobar
	(GetLink
		(TypedVariable (Variable "$x")
			(TypeChoice
				(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
				(Signature (Inheritance (Concept "bar") (Type "ConceptNode")))))
		(Variable "$x")
	)
)
(cog-execute! get-foobar)
(Evaluation
	(PredicateNode "foo")
	(List (ConceptNode "bingo") (ConceptNode "yes!")))
(Evaluation
	(AnchorNode "bar")
	(List (ConceptNode "hurrah") (ConceptNode "yay!")))
(Evaluation
	(ConceptNode "baz")
	(List (ConceptNode "oops") (ConceptNode "Oh no, Mr. Bill!")))
(define predicate-search
	(Get
		(TypedVariable
			(Variable "$x")
			(Signature
				(Evaluation
					(TypeChoice
						(Type "PredicateNode")
						(Type "AnchorNode"))
					(List
						(Type "ConceptNode") (Type "ConceptNode")))))
		(And (Variable "$x"))))
(cog-execute! predicate-search)
(cog-value-is-type?
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
	(Inheritance (Concept "foo") (ConceptNode "bar")))
(cog-value-is-type?
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
	(Inheritance (Concept "failure-mode") (ConceptNode "bar")))
(DefineLink
	(DefinedType "My foo type")
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))
(cog-value-is-type?
	(DefinedType "My foo type")
	(Inheritance (Concept "foo") (ConceptNode "bar")))
(cog-value-is-type?
	(DefinedType "My foo type")
	(Inheritance (Concept "failure-mode") (ConceptNode "bar")))