(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "filter-irc-eltof-test")
(test-begin tname)
(define is-pub?
	(Cond
		(Equal (Variable "$to")
			(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
		(Item "private message")
		(Item "public message")))
(define is-callout?
	(Cond
		(Equal
			(ElementOf (Number 0) (Variable "$msg"))
			(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
		(Item "calls out the bot")
		(Item "is just a message")))
(define callout-reply
	(list (Item "PRIVMSG") (Variable "$from")
	(Item "Message to ")
	(Variable "$to")
	(Item " is a ")
	is-pub?
	(Item " from ")
	(Variable "$from")
	(Item "that ")
	is-callout?
	(Item ": ")
	(Variable "$msg")))
(cog-set-value! (Anchor "IRC Bot")
	(Predicate "bot-name") (StringValue "echobot"))
(cog-set-value! (Concept "echobot") (Predicate "*-stream-*")
	(LinkValue
		(LinkValue
			(StringValue "linas")
			(StringValue "echobot")
			(StringValue "bing bang boom bomb"))))
(define do-callout
	(SetValue
		(Concept "echobot")
		(Predicate "*-write-*")
		(Filter
			(Rule
				(VariableList
					(Variable "$from") (Variable "$to") (Variable "$msg"))
				(LinkSignature (Type "LinkValue")
					(Variable "$from") (Variable "$to") (Variable "$msg"))
				(LinkSignature (Type "LinkValue")
					(Item "PRIVMSG")
					(Variable "$from")
					(Item "Message to ")
					(Variable "$to")
					(Item " is a ")
					(Cond
						(Equal
							(Variable "$to")
							(ValueOf
								(Anchor "IRC Bot")
								(Predicate "bot-name")))
						(Item "private message")
						(Item "public message"))
					(Item " from ")
					(Variable "$from")
					(Item "that ")
					(Cond
						(Equal
							(ValueOf
								(Anchor "IRC Bot")
								(Predicate "bot-name"))
							(ElementOf
								(Number "0")
								(Variable "$msg")))
						(Item "calls out the bot")
						(Item "is just a message"))
					(Item ": ")
					(Variable "$msg")))
			(ValueOf
				(Concept "echobot")
				(Predicate "*-stream-*")))))
(define reply (cog-execute! do-callout))
(format #t "Reply is ~A\n" reply)
(test-assert "callout test"
   (equal? reply
      (LinkValue
			(LinkValue
				(Item "PRIVMSG")
				(StringValue "linas")
				(Item "Message to ")
				(StringValue "echobot")
				(Item " is a ")
				(Item "private message")
				(Item " from ")
				(StringValue "linas")
				(Item "that ")
				(Item "is just a message")
				(Item ": ")
				(StringValue "bing bang boom bomb")))))
(test-end tname)
(opencog-test-end)