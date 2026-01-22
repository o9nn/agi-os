(use-modules (opencog) (opencog exec))
(define numli (List
(NumberNode 1)
(NumberNode 2)
(NumberNode 3)
(NumberNode 4)))
(FloatColumn numli)
(define numvec (cog-execute! (FloatColumn numli)))
(format #t "A vector of floating point numbers: ~A\n" numvec)
(define floli (LinkValue
(FloatValue 5)
(FloatValue 6)
(FloatValue 7)
(FloatValue 8)))
(cog-set-value! (Anchor "heavy") (Predicate "weight") floli)
(define flocol
(FloatColumn (ValueOf (Anchor "heavy") (Predicate "weight"))))
(define flovec (cog-execute! flocol))
(format #t "The float vector is: ~A\n" flovec)
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
(cog-set-value!
(Anchor "heavy") (Predicate "randgen") (RandomStream 1))
(define item-query
(Meet
(TypedVariable (Variable "$word") (Type 'ItemNode))
(Present (Variable "$word"))))
(cog-execute! item-query)
(cog-value item-query item-query)
(cog-execute! (ValueOf item-query item-query))
(define tag-items-randomly
(Filter
(Rule
(TypedVariable (Variable "$item") (Type 'ItemNode))
(Variable "$item")
(SetValue (Variable "$item") (Predicate "i-weight")
(StreamValueOf (Anchor "heavy") (Predicate "randgen"))))
(ValueOf item-query item-query)))
(cog-execute! tag-items-randomly)
(cog-keys (Item "leg"))
(cog-value (Item "leg") (Predicate "i-weight"))
(format #t "The item ~A has a weight of ~A\n"
(Item "leg")
(cog-execute! (ValueOf (Item "leg") (Predicate "i-weight"))))
(define matrix-of-pairs
(Query (VariableList
(TypedVariable (Variable "$left-word") (Type 'ItemNode))
(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
(Present
(Edge (Predicate "word-pair")
(List (Variable "$left-word") (Variable "$right-word"))))
(Edge (Predicate "word-pair")
(List (Variable "$left-word") (Variable "$right-word")))))
(cog-execute! matrix-of-pairs)
(cog-execute! (ValueOf matrix-of-pairs matrix-of-pairs))
(define tag-pairs-randomly
(Filter
(Rule
(Variable "$edge")
(Variable "$edge")
(SetValue (Variable "$edge") (Predicate "weight")
(StreamValueOf (Anchor "heavy") (Predicate "randgen"))))
(ValueOf matrix-of-pairs matrix-of-pairs)))
(cog-execute! tag-pairs-randomly)
(define edge-weights
(FloatColumn
(Filter
(Rule
(Variable "$edge")
(Variable "$edge")
(FloatValueOf (Variable "$edge") (Predicate "weight")))
(ValueOf matrix-of-pairs matrix-of-pairs))))
(define edge-vec (cog-execute! edge-weights))
(format #t "Vector of edge weights: ~A\n" edge-vec)
(define left-word-weights
(FloatColumn
(Filter
(Rule
(VariableList
(Variable "$left-word") (Variable "$right-word"))
(Edge (Predicate "word-pair")
(List (Variable "$left-word") (Variable "$right-word")))
(FloatValueOf (Variable "$left-word") (Predicate "i-weight")))
(ValueOf matrix-of-pairs matrix-of-pairs))))
(define left-vec (cog-execute! left-word-weights))
(format #t "Vector of left-word weights: ~A\n" left-vec)
(define right-word-weights
(FloatColumn
(Filter
(Rule
(VariableList
(Variable "$left-word") (Variable "$right-word"))
(Edge (Predicate "word-pair")
(List (Variable "$left-word") (Variable "$right-word")))
(FloatValueOf (Variable "$right-word") (Predicate "i-weight")))
(ValueOf matrix-of-pairs matrix-of-pairs))))
(define right-vec (cog-execute! right-word-weights))
(format #t "Vector of right-word weights: ~A\n" right-vec)
(cog-execute! (SexprColumn (ValueOf matrix-of-pairs matrix-of-pairs)))
(define four-column-set
(LinkColumn
(SexprColumn (ValueOf matrix-of-pairs matrix-of-pairs))
left-word-weights
edge-weights
right-word-weights))
(cog-execute! four-column-set)
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
(Log2 edge-weight))))
(ValueOf matrix-of-pairs matrix-of-pairs)))
(cog-execute! tag-pairs-w-stats)
(define (grab-column COLNO)
(FloatColumn
(Filter
(Rule
(Variable "$edge")
(Variable "$edge")
(ElementOf (Number COLNO)
(FloatValueOf (Variable "$edge") (Predicate "stats"))))
(ValueOf matrix-of-pairs matrix-of-pairs))))
(define log2-p (grab-column 2))
(define log2-pvec (cog-execute! log2-p))
(format #t "Log2 Probabilities: ~A\n" log2-pvec)