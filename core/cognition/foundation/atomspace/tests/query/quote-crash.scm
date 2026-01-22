(use-modules (opencog))
(use-modules (opencog exec))
(define crasher
	(BindLink
		(VariableNode "$x")
		(VariableNode "$x")
		(ListLink
			(ConceptNode "And the answer is ...")
			(QuoteLink (VariableNode "$x")))))
(define infloop
	(BindLink
		(VariableNode "$x")
		(VariableNode "$x")
		(ListLink
			(ConceptNode "And the answer is ...")
			(VariableNode "$x"))))