(define or-visible-put
	(SatisfactionLink
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "yes-visible")
					(ListLink (VariableNode "$x"))))
			(TrueLink (PutLink
					(StateLink (AnchorNode "state") (VariableNode "$yy"))
					(ConceptNode "ohhh noot visible")))
		)))
(define or-put
	(SatisfactionLink
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "or-visible")
					(ListLink (VariableNode "$x"))))
			(TrueLink (PutLink
					(StateLink (AnchorNode "state") (VariableNode "$yy"))
					(ConceptNode "not-vis")))
		)))
(define trig 0)
(define (incr-trig) (set! trig (+ trig 1)) (stv 1 1))
(define or-presence
	(SatisfactionLink
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x"))))
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))
(define and-absence
	(SatisfactionLink
		(TypedVariable (VariableNode "$x") (Type 'Concept))
		(SequentialAndLink
			(AbsentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x"))))
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))
(define and-not-present
	(SatisfactionLink
		(SequentialAndLink
			(NotLink (PresentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x")))))
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))
(define or-not-absent
	(SatisfactionLink
		(SequentialOrLink
			(NotLink (AbsentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x")))))
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))