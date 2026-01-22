(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "float-column-test")
(test-begin tname)
(define num (NumberNode 1 2 3 4))
(define ncol (FloatColumn num))
(define nvec (cog-execute! ncol))
(format #t "number vect: ~A\n" nvec)
(test-assert "number vect" (equal? nvec (FloatValue 1 2 3 4)))
(define numli (List
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))
(define nlicol (FloatColumn numli))
(define nlivec (cog-execute! nlicol))
(format #t "number list vect: ~A\n" nlivec)
(test-assert "number list vect" (equal? nlivec (FloatValue 1 2 3 4)))
(define numset (list
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))
(define nsetcol (FloatColumn numset))
(define nsetvec (cog-execute! nsetcol))
(format #t "number set vect: ~A\n" nsetvec)
(test-assert "number set vect" (equal? nlivec (FloatValue 1 2 3 4)))
(define floli (LinkValue
	(FloatValue 1)
	(FloatValue 2)
	(FloatValue 3)
	(FloatValue 4)))
(cog-set-value! (Anchor "heavy") (Predicate "weight") floli)
(define flocol
	(FloatColumn (ValueOf (Anchor "heavy") (Predicate "weight"))))
(define flovec (cog-execute! flocol))
(format #t "Float vect: ~A\n" flovec)
(test-assert "float list vect" (equal? flovec (FloatValue 1 2 3 4)))
(Edge (Predicate "word-pair") (List (Item "Paul") (Item "bit")))
(Edge (Predicate "word-pair") (List (Item "bit") (Item "the")))
(Edge (Predicate "word-pair") (List (Item "the") (Item "dog")))
(Edge (Predicate "word-pair") (List (Item "dog") (Item "in")))
(Edge (Predicate "word-pair") (List (Item "in") (Item "the")))
(Edge (Predicate "word-pair") (List (Item "the") (Item "leg")))
(Edge (Predicate "word-pair") (List (Item "leg") (Item "and")))
(Edge (Predicate "word-pair") (List (Item "and") (Item "it")))
(Edge (Predicate "word-pair") (List (Item "it") (Item "hurt")))
(Edge (Predicate "word-pair") (List (Item "hurt") (Item "a")))
(Edge (Predicate "word-pair") (List (Item "a") (Item "lot")))
(Edge (Predicate "word-pair") (List (Item "lot") (Item ".")))
(define mtxpr
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))
		(Edge (Predicate "word-pair")
			(List (Variable "$left-word") (Variable "$right-word")))))
(cog-execute! mtxpr)
(cog-set-value!
	(Anchor "heavy") (Predicate "randgen 1") (RandomStream 1))
(define tag-pairs-randomly
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "weight")
				(StreamValueOf (Anchor "heavy") (Predicate "randgen 1"))))
		(ValueOf mtxpr mtxpr)))
(cog-execute! tag-pairs-randomly)
(define datacol
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(FloatValueOf (Variable "$edge") (Predicate "weight")))
		(ValueOf mtxpr mtxpr))))
(define datavec (cog-execute! datacol))
(format #t "Data vect: ~A\n" datavec)
(test-assert "data list length" (equal? 12
	(length (cog-value->list datavec))))
(define edge-weight
	(FloatValueOf (Variable "$edge") (Predicate "weight")))
(define tag-pairs-w-stats
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "stats")
				(FloatColumn
					edge-weight
					(Times edge-weight edge-weight)
					(Times edge-weight edge-weight edge-weight))))
		(ValueOf mtxpr mtxpr)))
(cog-execute! tag-pairs-w-stats)
(define (grab-col COLNO)
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(ElementOf (Number COLNO)
					(FloatValueOf (Variable "$edge") (Predicate "stats"))))
		(ValueOf mtxpr mtxpr))))
(define cubecol (grab-col 2))
(define cubevec (cog-execute! cubecol))
(format #t "Cube vect: ~A\n" cubevec)
(test-assert "cube list length" (equal? 12
	(length (cog-value->list cubevec))))
(define squarecol (grab-col 1))
(define squarevec (cog-execute! squarecol))
(format #t "Square vect: ~A\n" squarevec)
(test-assert "square list length" (equal? 12
	(length (cog-value->list squarevec))))
(define origcol (grab-col 0))
(define origvec (cog-execute! origcol))
(format #t "Orig vect: ~A\n" origvec)
(test-assert "orig list length" (equal? 12
	(length (cog-value->list origvec))))
(test-assert "orig and data equal" (equal? datavec origvec))
(define four-col
	(LinkColumn
		(SexprColumn (ValueOf mtxpr mtxpr))
		(grab-col 0) (grab-col 1) (grab-col 2)))
(define four-vec (cog-execute! four-col))
(format #t "Four vect: ~A\n" four-vec)
(define four-list (cog-value->list four-vec))
(test-assert "Four Columns" (equal? 4 (length four-list)))
(test-assert "s-expressions" (equal? (list-ref four-list 0)
	(cog-execute! (SexprColumn (ValueOf mtxpr mtxpr)))))
(test-assert "orig col" (equal? (list-ref four-list 1) datavec))
(test-assert "square col" (equal? (list-ref four-list 2) squarevec))
(test-assert "cube col" (equal? (list-ref four-list 3) cubevec))
(test-end tname)
(opencog-test-end)