(use-modules (opencog) (opencog exec))
(StateLink (SchemaNode "start-interaction-timestamp") (NumberNode 0))
(StateLink (SchemaNode "current expression duration") (NumberNode 2.0))
(DefineLink
	(DefinedSchemaNode "set timestamp")
	(PutLink
		(StateLink (SchemaNode "start-interaction-timestamp")
			(VariableNode "$x"))
		(TimeLink)))
(DefineLink
	(DefinedSchemaNode "get timestamp")
	(GetLink
		(StateLink (SchemaNode "start-interaction-timestamp")
			(VariableNode "$x"))))
(DefineLink
	(DefinedPredicateNode "Time to change expression")
	(GreaterThanLink
		(MinusLink
			(TimeLink)
			(DefinedSchemaNode "get timestamp"))
		(GetLink (StateLink (SchemaNode "current expression duration")
			(VariableNode "$x")))
	))