(use-modules (opencog))
(use-modules (opencog exec))
(SexprAst "(gunk (junk) stunk)")
(SexprAst (quote (ork ("asdf") stunk)))
(SexprAst (quote (some stunk)))
(SexprAst (quote (Mork from ork)))
(Inheritance (Concept "tree-like stuffs") (SexprAst "ork"))
(cog-prt-atomspace)
(cog-get-atoms 'SexprAst)
(cog-incoming-set (SexprAst 'ork))
(cog-incoming-by-type (SexprAst 'ork) 'SexprAst)
(define qry-pair
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Variable "$x") (SexprAst 'stunk)))))
(cog-execute! qry-pair)
(define qry-list
	(Meet (TypedVariable (Glob "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Glob "$x") (SexprAst 'stunk)))))
(cog-execute! qry-list)