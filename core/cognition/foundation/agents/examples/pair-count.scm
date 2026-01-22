(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog sensory))
(use-modules (srfi srfi-1))
(cog-execute!
	(LgParseBonds (Phrase "this is a test") (LgDict "any") (Number 1)))
(cog-report-counts)
(cog-get-atoms 'WordNode)
(for-each cog-extract-recursive! (cog-get-atoms 'WordNode))
(extract-type 'WordNode)
(define exo
	(ExecutionOutput
		(GroundedSchema "scm: foo")
		(List (Concept "bar") (Concept "baz"))))
(define (foo x y)
	(format #t "I got ~A and ~A\n" x y)
	(Concept "this is the foo reply"))
(cog-execute! exo)
(define (print-atom x) (format #t "Got ~A" x) x)
(define (debug-prt x)
	(ExecutionOutput (GroundedSchema "scm: print-atom") x))
(define demo-filter
	(Filter
		(Variable "$x")
		(LgParseBonds (Phrase "this is a test") (LgDict "any") (Number 1))))
(cog-execute! demo-filter)
(define demo-filter
	(Filter
		(Rule
			(Variable "$x")
			(Variable "$x"))
		(LgParseBonds (Phrase "this is a test") (LgDict "any") (Number 1))))
(cog-execute! demo-filter)
(define demo-filter
	(Filter
		(Rule
			(LinkSignature
				(Type 'LinkValue)
				(Variable "$words")
				(Variable "$edges"))
			(Variable "$words"))
		(LgParseBonds (Phrase "this is a test") (LgDict "any") (Number 1))))
(cog-execute! demo-filter)
(define (edge-filter PASRC FUNKY)
	(Filter
		(Rule
			(LinkSignature
				(Type 'LinkValue)
				(Variable "$words")
				(Variable "$edge-list"))
			(FUNKY (Variable "$edge-list")))
		PASRC))
(define parse-stuff
	(LgParseBonds (Phrase "this is a test") (LgDict "any") (Number 1)))
(cog-execute! (edge-filter parse-stuff debug-prt))
(define (incr-cnt edge)
	(SetValue edge (Predicate "count")
		(Plus (Number 0 0 1)
			(FloatValueOf edge (Predicate "count")
				(FloatValueOf (Number 0 0 0))))))
(cog-execute! (incr-cnt (Concept "foobar")))
(cog-value (Concept "foobar") (Predicate "count"))
(cog-execute! (ValueOf (Concept "foobar") (Predicate "count")))
(define (edge-counter EDGE-LIST)
	(Filter
		(Rule
			(TypedVariable (Variable "$edge") (Type 'Edge))
			(Variable "$edge")
			(incr-cnt (Variable "$edge")))
		EDGE-LIST))
(cog-execute! (edge-filter parse-stuff edge-counter))
(cog-execute!
	(SetValue (Anchor "pipe demo") (Predicate "text src")
		(Open (Type 'TextFileStream)
			(Sensory "file:///tmp/demo.txt"))))
(define parse-stream
	(LgParseBonds
		(ValueOf (Anchor "pipe demo") (Predicate "text src"))
		(LgDict "any") (Number 1)))
(cog-execute! parse-stream)
(cog-execute! (edge-filter parse-stream edge-counter))
(cog-execute!
	(SetValue (Anchor "pipe demo") (Predicate "text src")
		(Open (Type 'TextFileStream)
			(Sensory "file:///tmp/demo.txt"))))