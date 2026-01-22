(use-modules (opencog) (opencog exec))
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
(cog-execute! find-files)