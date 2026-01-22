(use-modules (opencog) (opencog exec))
(use-modules (opencog csv-table))
(define tab (Concept "My foo Table"))
(load-table tab "table.csv")
(cog-keys tab)
(define colkeys (Predicate "*-column-keys-*"))
(cog-value tab colkeys)
(for-each
	(lambda (KEY) 
		(format #t "The key ~A   holds data ~A\n" KEY (cog-value tab KEY)))
	(cog-value->list (cog-value tab colkeys)))
(cog-value tab (PredicateNode "flt1"))
(cog-execute! (ValueOf tab (PredicateNode "flt1")))
(cog-execute!
	(Minus
		(FloatValueOf tab (PredicateNode "flt2"))
		(FloatValueOf tab (PredicateNode "flt1"))))
(DefineLink
	(DefinedSchema "col diffs")
   (Lambda
      (Variable "$tbl-name")
		(SetValue
			(Variable "$tbl-name") (Predicate "f2 minus f1")
			(Minus
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt2"))
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt1"))))))
(cog-execute! (Put (DefinedSchema "col diffs") tab))
(cog-keys tab)
(cog-value tab (Predicate "f2 minus f1"))
(cog-execute!
	(Accumulate (FloatValueOf tab (Predicate "f2 minus f1"))))
(DefineLink
	(DefinedSchema "compute score")
   (Lambda
      (Variable "$tbl-name")
		(Accumulate
			(Minus
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt2"))
				(FloatValueOf (Variable "$tbl-name") (PredicateNode "flt1"))))))
(cog-execute! (Put (DefinedSchema "compute score") tab))