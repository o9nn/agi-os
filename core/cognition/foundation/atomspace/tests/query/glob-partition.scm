(use-modules (opencog) (opencog exec))
(define partition
(BindLink
	(VariableList
		(TypedVariableLink
			(GlobNode "$begin")
			(IntervalLink (NumberNode 0) (NumberNode -1)))
		(TypedVariableLink
			(GlobNode "$end")
			(IntervalLink (NumberNode 0) (NumberNode -1))))
	(List
		(GlobNode "$begin")
		(Concept "foo")
		(GlobNode "$end"))
	(OrderedLink
		(Concept "begin")
		(GlobNode "$begin")
		(Concept "end")
		(GlobNode "$end")))
)
(List (Concept "foo")(Concept "foo")(Concept "foo")(Concept "foo"))
(define part-deeper
(BindLink
	(VariableList
		(TypedVariableLink
			(GlobNode "$begin")
			(IntervalLink (NumberNode 0) (NumberNode -1)))
		(TypedVariableLink
			(GlobNode "$end")
			(IntervalLink (NumberNode 0) (NumberNode -1))))
	(List (List
		(GlobNode "$begin")
		(Concept "bar")
		(GlobNode "$end")))
	(OrderedLink
		(Concept "begin")
		(GlobNode "$begin")
		(Concept "end")
		(GlobNode "$end")))
)
(List (List (Concept "bar")(Concept "bar")(Concept "bar")(Concept "bar")))