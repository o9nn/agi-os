(use-modules (opencog) (opencog exec) (opencog logger))
(ListLink
	(LambdaLink (VariableList (VariableNode "$W"))
		(PresentLink
			(InheritanceLink (VariableNode "$W") (Concept "A"))
			(InheritanceLink (Concept "A") (Concept "B"))))
	(NumberNode "5.000000"))
(AndLink
	(InheritanceLink (VariableNode "$W") (Concept "A"))
	(InheritanceLink (Concept "A") (Concept "B")))
(define query
	(GetLink (PresentLink
		(ListLink (QuoteLink
			(LambdaLink (UnquoteLink (VariableNode "$f-vardecl"))
				(PresentLink
					(UnquoteLink (VariableNode "$cnj-bodies-1"))
					(UnquoteLink (VariableNode "$cnj-bodies-0")))))
			(VariableNode "$ms-0"))
		(ListLink (QuoteLink
			(LambdaLink (UnquoteLink (VariableNode "$f-vardecl"))
				(PresentLink
					(UnquoteLink (VariableNode "$cnj-bodies-1"))
					(UnquoteLink (VariableNode "$cnj-bodies-0")))))
			(VariableNode "$ms-1")))))