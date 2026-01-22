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
(ListLink (Concept "I") (Concept "love") (Number 42))
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
            (TypeSetLink (IntervalLink (Number 0) (Number -1)) (Type "ConceptNode")))
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
(define love-three-globs
    (BindLink
        (VariableList
            (TypedVariable (Glob "$x") (IntervalLink (Number 0) (Number -1)))
            (TypedVariable (Glob "$y")
                (TypeSetLink (Type "ConceptNode") (IntervalLink (Number 1) (Number 1))))
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