(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "filter-irc-test")
(test-begin tname)
(cog-set-value! (Anchor "IRC Bot") (Predicate "echo")
	(LinkValue
		(LinkValue
			(StringValue "linas")
			(StringValue "echobot")
			(StringValue "bunch o text"))))
(define make-private-reply
	(Filter
		(Rule
			(VariableList
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Item "PRIVMSG")
				(Variable "$from")
				(Item "you said: ")
				(Variable "$msg")))
		(ValueOf (Anchor "IRC Bot") (Predicate "echo"))))
(define priv-rep (cog-execute! make-private-reply))
(test-assert "response test"
	(equal? priv-rep
		(LinkValue
			(LinkValue
				(Item "PRIVMSG")
				(StringValue "linas")
				(Item "you said: ")
				(StringValue "bunch o text")))))
(cog-set-value!
	(Anchor "IRC Bot") (Predicate "bot-name") (StringValue "echobot"))
(define is-pub?
	(Filter
		(Rule
			(VariableList
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Cond
					(Equal (Variable "$to")
						(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
					(Item "private message")
					(Item "public message"))))
		(ValueOf (Anchor "IRC Bot") (Predicate "echo"))))
(define is-pub (cog-execute! is-pub?))
(format #t "Private reply was ~A\n" is-pub)
(test-assert "private test"
	(equal? is-pub
		(LinkValue (LinkValue (Item "private message")))))
(test-end tname)
(opencog-test-end)