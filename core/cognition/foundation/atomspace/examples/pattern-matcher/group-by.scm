(use-modules (opencog) (opencog exec))
(Edge (Predicate "property") (List (Item "green") (Item "colors")))
(Edge (Predicate "property") (List (Item "brown") (Item "colors")))
(Edge (Predicate "property") (List (Item "black") (Item "colors")))
(Edge (Predicate "property") (List (Item "round") (Item "shapes")))
(Edge (Predicate "property") (List (Item "square") (Item "shapes")))
(Edge (Predicate "property") (List (Item "trident") (Item "shapes")))
(Edge (Predicate "le grande foobar") (List (Item "blob") (Item "shapes")))
(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))
(define grp-query
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))
			(Group (Variable "$Y")))
		(Evaluation (Concept "things that go together")
			(Implication (Variable "$Y") (Variable "$X")))))
(define query-results (cog-execute! grp-query))
(format #t "There are ~A results.\n" (length (cog-value->list query-results)))
(format #t "The query results are:\n~A\n" query-results)
(define grp-set
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))
			(Group (Variable "$Y")))
		(Variable "$X")))
(define set-results (cog-execute! grp-set))
(format #t "The groupings are:\n~A\n" set-results)
(define grp-range
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))
			(Group
				(Variable "$Y")
				(Interval (Number 2) (Number 5))))
		(Variable "$X")))
(define range-results (cog-execute! grp-range))
(format #t "The groupings are:\n~A\n" range-results)
(define grp-collapse
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))
			(Group
				(Variable "$Y")
				(Interval (Number 2) (Number 5))))
		(Variable "$Y")))
(define collapse-results (cog-execute! grp-collapse))
(format #t "The group names are:\n~A\n" collapse-results)