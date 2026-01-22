(use-modules (opencog) (opencog exec))
(cog-set-value!
(Anchor "some place")
(Predicate "number key")
(FloatValue 0 0 0))
(cog-execute! (ValueOf (Anchor "some place") (Predicate "number key")))
(define increment
(SetValue (Anchor "some place") (Predicate "number key")
(PlusLink
(NumberNode 1 2 3)
(FloatValueOf (Anchor "some place") (Predicate "number key")))))
(cog-execute! increment)
(cog-execute! (ValueOf (Anchor "some place") (Predicate "number key")))
(cog-execute! increment)
(cog-execute! increment)
(cog-execute! increment)
(define (my-print-func arg)
(format #t "hi there! ~A" (cog-execute! arg))
(sleep 1)
(VoidValue))
(define print-stuff
(ExecutionOutput
(GroundedSchema "scm:my-print-func")
(List
(ValueOf (Anchor "some place") (Predicate "number key")))))
(cog-execute! print-stuff)
(Define
(DefinedProcedure "simple-tail")
(PureExec (cog-atomspace)
increment
print-stuff
(DefinedProcedure "simple-tail")))
(cog-execute! (DefinedProcedure "simple-tail"))
(cog-set-value!
(Anchor "some place")
(Predicate "randgen")
(RandomStream 1))
(Define (DefinedPredicate "keep going?")
(GreaterThan (Number 0.9)
(FloatValueOfLink (Anchor "some place") (Predicate "randgen"))))
(define (print-done)
(display "We are done now!\n")
(VoidValue))
(Define
(DefinedProcedure "stop-randomly")
(PureExec (cog-atomspace)
increment
print-stuff
(CondLink
(DefinedPredicate "keep going?")
(DefinedProcedure "stop-randomly")
(ExecutionOutput (GroundedSchema "scm:print-done") (List)))))
(cog-execute! (DefinedProcedure "stop-randomly"))
(cog-execute! (DefinedProcedure "stop-randomly"))
(cog-execute! (DefinedProcedure "stop-randomly"))
(cog-execute! (DefinedProcedure "stop-randomly"))
(define (print-A) (display "I am procedure A!\n") (VoidValue))
(define (print-B) (display "I am procedure B!\n") (VoidValue))
(define (print-C) (display "I am procedure C!\n") (VoidValue))
(Define
(DefinedProcedure "procedure A")
(PureExec (cog-atomspace)
increment
(ExecutionOutput (GroundedSchema "scm:print-A") (List))
(DefinedProcedure "procedure B")))
(Define
(DefinedProcedure "procedure B")
(PureExec (cog-atomspace)
increment
(ExecutionOutput (GroundedSchema "scm:print-B") (List))
(DefinedProcedure "procedure C")))
(Define
(DefinedProcedure "procedure C")
(PureExec (cog-atomspace)
increment
(ExecutionOutput (GroundedSchema "scm:print-C") (List))
(CondLink
(DefinedPredicate "keep going?")
(DefinedProcedure "procedure A")
(ExecutionOutput (GroundedSchema "scm:print-done") (List)))))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))