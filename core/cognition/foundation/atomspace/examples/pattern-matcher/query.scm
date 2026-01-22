(use-modules (opencog) (opencog exec))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
(Predicate "foobar") (List (Concept "funny") (Concept "joke")))
(define query
(Query
(TypedVariable (Variable "$x") (Type 'ConceptNode))
(Evaluation
(Predicate "foobar")
(List (Concept "funny") (Variable "$x")))
(ListLink
(Anchor "*-query results-*")
(Implication (Variable "$x") (Concept "laughable")))
))
(cog-execute! query)
(cog-incoming-set (Anchor "*-query results-*"))
(define absurd
(Query
(TypedVariable (Variable "$x") (Type 'ConceptNode))
(And
(Present (ListLink
(Anchor "*-query results-*")
(Implication (Variable "$x") (Concept "laughable"))))
(True (Delete (ListLink
(Anchor "*-query results-*")
(Implication (Variable "$x") (Concept "laughable"))))))
(ListLink
(Anchor "*-risible results-*")
(Implication (Variable "$x") (Concept "ludicrous")))
))
(cog-execute! absurd)
(cog-incoming-set (Anchor "*-query results-*"))
(cog-incoming-set (Anchor "*-risible results-*"))
(define (report-stuff NODE-A NODE-B)
(format #t "I think that ~A is ~A. -- ~A\n"
(cog-name NODE-A) (cog-name NODE-B)
(strftime "%c" (localtime (current-time)))
)
(SimpleTruthValue 1 1))
(define output
(Query
(VariableList
(TypedVariable (Variable "$x") (Type 'ConceptNode))
(TypedVariable (Variable "$y") (Type 'ConceptNode)))
(And
(Present (ListLink
(Anchor "*-risible results-*")
(Implication (Variable "$x") (Variable "$y"))))
(True (Delete (ListLink
(Anchor "*-risible results-*")
(Implication (Variable "$x") (Variable "$y"))))))
(ExecutionOutput
(GroundedSchema "scm:report-stuff")
(ListLink (Variable "$x") (Variable "$y")))
))
(cog-execute! output)
(cog-execute! output)
(cog-incoming-set (AnchorNode "*-risible results-*"))
(define (prti N)
(format #t "Thread ~A. -- ~A\n" (cog-name N)
(strftime "%c" (localtime (current-time))))
(SimpleTruthValue 1 1))
(define (prtime STR)
(Evaluation
(GroundedPredicate "scm:prti")
(Concept STR)))
(define threads
(Parallel
(SequentialAnd
(prtime "step one A")
(True query)
(True (Sleep (Number 4)))
(prtime "step one B")
(True query)
(True (Sleep (Number 4)))
(prtime "step one C")
(True query))
(SequentialAnd
(True (Sleep (Number 1)))
(prtime "step two A")
(True absurd)
(True (Sleep (Number 4)))
(prtime "step two B")
(True absurd)
(True (Sleep (Number 4)))
(prtime "step two C")
(True absurd))
(SequentialAnd
(True (Sleep (Number 2)))
(prtime "step three A")
(True output)
(True (Sleep (Number 4)))
(prtime "step three B")
(True output)
(True (Sleep (Number 4)))
(prtime "step three C")
(True output))
))