(use-modules (opencog) (opencog exec))
(define node-from-node
	(cog-execute! (StringOf (Type 'Concept) (Predicate "bar"))))
(format #t "Node from node created ~A\n" node-from-node)
(cog-set-value! (Anchor "anch") (Predicate "key")
	(StringValue "a" "b" "c"))
(define node-from-string
	(cog-execute! (StringOf (Type 'Concept)
		(ValueOf (Anchor "anch") (Predicate "key")))))
(format #t "Node from string got ~A\n" node-from-string)
(cog-execute!
	(SetValue (Anchor "anch") (Predicate "strkey")
		(StringOf (Type 'StringValue)
			(Concept "do-da"))))
(define string-from-node
	(cog-value (Anchor "anch") (Predicate "strkey")))
(format #t "Got string from node ~A\n" string-from-node)
(cog-set-value! (Anchor "anch") (Predicate "flokey")
	(LinkValue
		(StringValue "here")
		(StringValue "is")
		(StringValue "a")
		(StringValue "sequence")
		(StringValue "of")
		(StringValue "words")))
(define tag-sentence-words
	(Filter
		(Rule
			(Variable "$strv")
			(Variable "$strv")
			(Edge (Predicate "sentence word")
				(StringOf (Type 'Concept)
					(ValueOf (Variable "$strv")))))
		(ValueOf (Anchor "anch") (Predicate "flokey"))))
(cog-execute! tag-sentence-words)
(define query
	(Meet
		(TypedVariable (Variable "$word") (Type 'Concept))
		(Edge (Predicate "sentence word") (Variable "$word"))))
(define observed-words (cog-execute! query))
(format #t "These words were seen: ~A\n" observed-words)