(define inheritance-to-member-rule
	(BindLink
		(VariableList
			(VariableNode "$B")
			(VariableNode "$C"))
		(InheritanceLink
			(VariableNode "$B")
			(VariableNode "$C"))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: inheritance-to-member-formula")
			(ListLink
				(MemberLink
					(VariableNode "$B")
					(VariableNode "$C"))
				(InheritanceLink
					(VariableNode "$B")
					(VariableNode "$C"))))))
(define (inheritance-to-member-formula MBC IBC)
	(cog-set-tv!
		MBC
		(inheritance-to-member-side-effect-free-formula
			MBC
			IBC)))
(define (inheritance-to-member-side-effect-free-formula MBC IBC)
	(stv
		(cog-mean IBC)
		(* (cog-confidence IBC) 0.9)))
(define inheritance-to-member-rule-name
  (DefinedSchemaNode "inheritance-to-member-rule"))
(DefineLink
  inheritance-to-member-rule-name
  inheritance-to-member-rule)