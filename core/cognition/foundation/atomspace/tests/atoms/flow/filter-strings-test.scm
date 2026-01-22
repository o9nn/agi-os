(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "filter-strings-test")
(test-begin tname)
(define stream (LinkValue
	(LinkValue (StringValue "/usr") (StringValue "dir"))
	(LinkValue (StringValue "/usr/lib") (StringValue "dir"))
	(LinkValue (StringValue "/etc") (StringValue "dir"))
	(LinkValue (StringValue "/etc/motd") (StringValue "reg"))
	(LinkValue (StringValue "/dev/sda") (StringValue "block"))
	(LinkValue (StringValue "/dev/tty0") (StringValue "char"))))
(cog-set-value! (Anchor "rock") (Predicate "key") stream)
(define find-files
	(Filter
		(Rule
			(Variable "$filename")
			(LinkSignature (Type 'LinkValue)
				(Variable "$filename")
				(StringOf (Type 'StringValue) (Node "reg")))
			(Edge
				(Predicate "is-a file URL")
				(StringOf (Type 'ItemNode) (Variable "$filename"))))
		(ValueOf (Anchor "rock") (Predicate "key"))))
(define fili (cog-execute! find-files))
(test-assert "find-files  rule"
	(equal? fili (LinkValue
		(Edge
			(Predicate "is-a file URL")
			(Item "/etc/motd")))))
(test-end tname)
(opencog-test-end)