(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "group-by-test")
(test-begin tname)
(Edge (Predicate "property") (List (Item "green") (Item "colors")))
(Edge (Predicate "property") (List (Item "brown") (Item "colors")))
(Edge (Predicate "property") (List (Item "black") (Item "colors")))
(Edge (Predicate "property") (List (Item "round") (Item "shapes")))
(Edge (Predicate "property") (List (Item "square") (Item "shapes")))
(Edge (Predicate "property") (List (Item "trident") (Item "shapes")))
(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))
(define grp-meet
(Meet
(VariableList (Variable "$X") (Variable "$Y"))
(And
(Group (Variable "$Y"))
(Present
(Edge (Predicate "property")
(List (Variable "$X") (Variable "$Y")))))))
(define meet-results (cog-execute! grp-meet))
(test-assert "meet group size"
(equal? 3 (length (cog-value->list meet-results))))
(define grp-query
(Query
(VariableList (Variable "$X") (Variable "$Y"))
(And
(Group (Variable "$Y"))
(Present
(Edge (Predicate "property")
(List (Variable "$X") (Variable "$Y")))))
(Edge (Predicate "go together")
(List (Variable "$Y") (Variable "$X")))))
(define query-results (cog-execute! grp-query))
(test-assert "query group size"
(equal? 3 (length (cog-value->list query-results))))
(define grp-range
(Query
(VariableList (Variable "$X") (Variable "$Y"))
(And
(Group
(Variable "$Y")
(Interval (Number 2) (Number 4)))
(Present
(Edge (Predicate "property")
(List (Variable "$X") (Variable "$Y")))))
(Variable "$X")))
(define range-results (cog-execute! grp-range))
(test-assert "range group size"
(equal? 2 (length (cog-value->list range-results))))
(define collapse-range
(Query
(VariableList (Variable "$X") (Variable "$Y"))
(And
(Group
(Variable "$Y")
(Interval (Number 2) (Number 4)))
(Present
(Edge (Predicate "property")
(List (Variable "$X") (Variable "$Y")))))
(Variable "$Y")))
(define collapse-results (cog-execute! collapse-range))
(test-assert "range collapse size"
(equal? 2 (length (cog-value->list collapse-results))))
(define unbounded-range
(Query
(VariableList (Variable "$X") (Variable "$Y"))
(And
(Group
(Variable "$Y")
(Interval (Number 2) (Number -1)))
(Present
(Edge (Predicate "property")
(List (Variable "$X") (Variable "$Y")))))
(Variable "$Y")))
(define unbounded-results (cog-execute! unbounded-range))
(test-assert "range unbounded size"
(equal? 2 (length (cog-value->list unbounded-results))))
(test-end tname)
(opencog-test-end)