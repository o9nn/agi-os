(use-modules (opencog) (opencog exec))
(ListLink
	(Concept "I")
	(Concept "love")
	(Concept "you"))
(ListLink
	(Concept "I")
	(Concept "really")
	(Concept "totally")
	(Concept "need")
	(Concept "you"))
(ListLink
	(Concept "I")
	(Concept "love")
	(Concept "teddy")
	(Concept "bears")
	(Concept "a")
	(Concept "lot"))
(ListLink
	(Concept "I")
	(Concept "need")
	(Concept "you")
	(Concept "now"))
(ListLink
	(Concept "they")
	(Concept "think")
	(Concept "I")
	(Concept "hate")
	(Concept "you"))
(ListLink
	(Concept "I")
	(Concept "love")
	(Number 42))
(ListLink
	(Concept "hi"))
(ListLink
	(Concept "hi")
	(Concept "Sophia"))
(ListLink
	(Concept "they")
	(Concept "really")
	(Concept "want")
	(Concept "it"))
(ListLink
	(Concept "they")
	(Concept "want")
	(Concept "it"))
(ListLink
	(Concept "the")
	(Concept "man")
	(Concept "we")
	(Concept "saw")
	(Concept "saw")
	(Concept "a")
	(Concept "saw"))
(SetLink
	(Concept "honeydew")
	(Concept "lime")
	(Concept "apple"))
(define glob-you
	(BindLink
		(ListLink
			(Concept "I") (Glob "$star") (Concept "you"))
		(ListLink
			(Concept "I") (Glob "$star") (Concept "you") (Concept "too"))))
(define love-glob
	(BindLink
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-type-glob
	(BindLink
		(TypedVariable (Glob "$star") (Type "NumberNode"))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-interval-glob
	(BindLink
		(TypedVariable (Glob "$star") (IntervalLink (Number 0) (Number 1)))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-typeset-glob
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeIntersectionLink (IntervalLink (Number 0) (Number -1)) (Type "ConceptNode")))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-interval-glob-empty-intersection
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeIntersectionLink
				(IntervalLink (Number 0) (Number -1))
				(IntervalLink (Number 1) (Number 0))
				(Type "ConceptNode")))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-interval-glob-empty-union
	(BindLink
		(TypedVariable (Glob "$star")
			(TypeChoice
				(IntervalLink (Number 1) (Number 1))
				(IntervalLink (Number 1) (Number 0))
				(Type "ConceptNode")
				(Type "NumberNode")))
		(ListLink
			(Concept "I")
			(Concept "love")
			(Glob "$star"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Concept "like")
			(Glob "$star")
			(Concept "also"))))
(define love-three-globs
	(BindLink
		(VariableList
			(TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
			(TypedVariable (Glob "$y")
				(TypeIntersectionLink (Type "ConceptNode") (IntervalLink (Number 1) (Number 1))))
			(TypedVariable (Glob "$z") (IntervalLink (Number 0) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "I")
			(Glob "$y")
			(Concept "you")
			(Glob "$z"))
		(ListLink
			(Concept "Hey!")
			(Concept "I")
			(Glob "$y")
			(Concept "you")
			(Concept "also"))))
(define greet
	(BindLink
		(VariableList
			(TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode") (IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z") (IntervalLink (Number 0) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "hi")
			(Glob "$y")
			(Glob "$z"))
		(ListLink
			(Concept "hi")
			(Concept "I")
			(Concept "am")
			(Glob "$y"))))
(define exact
	(Bind
		(TypedVariable (Glob "$x") (IntervalLink (Number 3) (Number 3)))
		(ListLink
			(Concept "they")
			(Glob "$x"))
		(ListLink
			(Concept "I")
			(Glob "$x")
			(Concept "too"))))
(define greedy
	(Bind
		(TypedVariable (Glob "$x")
			(TypeIntersection (Type "ConceptNode")
				(IntervalLink (Number 1) (Number -1))))
		(ListLink
			(Glob "$x")
			(Concept "saw"))
		(ListLink
			(Glob "$x")
			(Concept "cat")
			(Concept "too"))))
(define unorder
	(Bind
		(TypedVariable (Glob "$x") (Type "ConceptNode"))
		(SetLink
			(Glob "$x")
			(Concept "apple"))
		(ListLink
			(Glob "$x"))))
(Evaluation
	(Predicate "Some Seq")
	(List
		(Concept "Some Node")
		(List
			(Concept "A")
			(Concept "B")
			(Concept "C")
			(Concept "D")
			(Concept "E")
			(Concept "F")
			(Concept "G")
			(Concept "H")
			(Concept "I"))))
(define-public (match-c ATOM)
	(if (equal? (Concept "C") ATOM)
		(stv 1 1)
		(stv 0 1)))
(define-public (match-def ATOM)
	(if (equal? (List (Concept "D") (Concept "E") (Concept "F")) ATOM)
		(stv 1 1)
		(stv 0 1)))
(define backtrack
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number 1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(List (Glob "$x") (Glob "$y") (Glob "$z"))
			(Evaluation (GroundedPredicate "scm: match-c") (List (Glob "$y"))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))
(define backtoo
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(List (Glob "$x") (Glob "$y") (Glob "$z"))
			(Evaluation (GroundedPredicate "scm: match-def")
				(List (List (Glob "$y")))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))
(define backmore
	(Bind
		(VariableList
			(TypedVariable (Glob "$x")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1))))
			(TypedVariable (Glob "$y")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 1) (Number -1))))
			(TypedVariable (Glob "$z")
				(TypeIntersection (Type "ConceptNode")
					(IntervalLink (Number 0) (Number -1)))))
		(And
			(Evaluation (Predicate "Some Seq")
				(List (Concept "Some Node")
					(List (Glob "$x") (Glob "$y") (Glob "$z"))))
			(Evaluation (GroundedPredicate "scm: match-def")
				(List (List (Glob "$y")))))
		(List
			(List (Glob "$x"))
			(List (Glob "$y"))
			(List (Glob "$z")))))
(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "c")
	(ConceptNode "d")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "g")
	(ConceptNode "h")
	(ConceptNode "i")
	(ConceptNode "j")
	(ConceptNode "k"))
(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "FOO")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "FOO")
	(ConceptNode "j")
	(ConceptNode "k"))
(ListLink
	(ConceptNode "a")
	(ConceptNode "b")
	(ConceptNode "FOO")
	(ConceptNode "e")
	(ConceptNode "f")
	(ConceptNode "FOO")
	(ConceptNode "BAR")
	(ConceptNode "j")
	(ConceptNode "k"))
(define get-ma
	(GetLink
		(GlobNode "star")
		(ListLink
			(ConceptNode "a")
			(ConceptNode "b")
			(GlobNode "star")
			(ConceptNode "e")
			(ConceptNode "f")
			(GlobNode "star")
			(ConceptNode "j")
			(ConceptNode "k"))))