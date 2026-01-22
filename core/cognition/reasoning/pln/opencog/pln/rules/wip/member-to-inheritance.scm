(define member-to-inheritance-rule
	(BindLink
		(VariableList
			(VariableNode "$B")
			(VariableNode "$C"))
		(MemberLink
			(VariableNode "$B")
			(VariableNode "$C"))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: member-to-inheritance-formula")
			(ListLink
				(InheritanceLink
					(VariableNode "$B")
					(VariableNode "$C"))
				(MemberLink
					(VariableNode "$B")
					(VariableNode "$C"))))))
(define (member-to-inheritance-formula IBC MBC)
	(cog-set-tv!
		IBC
		(member-to-inheritance-side-effect-free-formula
			IBC
			MBC)))
(define (member-to-inheritance-side-effect-free-formula IBC MBC)
	(stv
		(cog-mean MBC)
		(* (cog-confidence MBC) 0.9)))
(define member-to-inheritance-rule-name
  (DefinedSchemaNode "member-to-inheritance-rule"))
(DefineLink
  member-to-inheritance-rule-name
  member-to-inheritance-rule)