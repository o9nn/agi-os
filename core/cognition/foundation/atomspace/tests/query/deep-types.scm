(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog type-utils))
(DefineLink
   (DefinedType "My foo type")
   (Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))
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
(define get-foobar
	(GetLink
		(TypedVariable (Variable "$x")
			(TypeChoice
				(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
				(Signature (Inheritance (Concept "bar") (Type "ConceptNode")))))
		(Variable "$x")
	)
)
(EvaluationLink
	(PredicateNode "foo")
	(ListLink (ConceptNode "bingo") (ConceptNode "yes!")))
(EvaluationLink
	(AnchorNode "bar")
	(ListLink (ConceptNode "hurrah") (ConceptNode "yay!")))
(EvaluationLink
	(ConceptNode "baz")
	(ListLink (ConceptNode "oops") (ConceptNode "Oh no, Mr. Bill!")))
(define predicate-search
	(GetLink
		(TypedVariable
			(Variable "$x")
			(Signature
				(EvaluationLink
					(TypeChoice
						(TypeNode "PredicateNode")
						(TypeNode "AnchorNode"))
					(ListLink
						(Type "ConceptNode") (Type "ConceptNode")))))
		(AndLink (Variable "$x"))))
(DefineLink
	(DefinedType "predicate-type")
	(Signature
		(EvaluationLink
			(TypeChoice
				(TypeNode "PredicateNode")
				(TypeNode "AnchorNode"))
			(ListLink
				(Type "ConceptNode") (Type "ConceptNode")))))
(define predicate-search-typed
	(GetLink
		(TypedVariable
			(Variable "$x")
			(DefinedType "predicate-type"))
		(AndLink (Variable "$x"))))
(define constant-a
	(Get
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))
(define constant-zappa
	(Get
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present
			(Evaluation (Predicate "Aybe Sea")
				(ListLink
					(Variable "X") (Concept "B") (Concept "C"))))))
(Evaluation (Predicate "Aybe Sea")
	(ListLink (Concept "A") (Concept "B") (Concept "C")))
(define deep-disconnect
   (Get
      (VariableList
         (TypedVariable (Variable "X") (Signature (Concept "A")))
         (TypedVariable (Variable "Y") (Signature (Concept "B"))))
      (And
         (Present (Variable "X"))
         (Present (Variable "Y")))))