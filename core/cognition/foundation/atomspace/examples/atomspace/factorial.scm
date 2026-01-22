(use-modules (opencog) (opencog exec))
(Define
	(DefinedSchema "factorial")
	(Lambda
		(TypedVariable (Variable "$n") (Type "NumberNode"))
		(Cond
			(GreaterThan (Variable "$n") (Number 0))
			(Times
				(Variable "$n")
				(ExecutionOutput
					(DefinedSchema "factorial")
					(Minus (Variable "$n") (Number 1))))
			(Number 1)))
)
#! ----------
How fast is this? Well, its slowwwww, but still, you can find out:
Just cut-n-paste the below. It takes about 16 seconds on my cheap
Intel Celeron laptop.
(define nrep 5000)
(define start (get-internal-real-time))
(for-each
	(lambda (x)
		(cog-execute! (ExecutionOutput (DefinedSchema "factorial") (Number 100))))
	(iota nrep))
(define end (get-internal-real-time))
(define elapsed
	(exact->inexact
		(/ (- end start) internal-time-units-per-second)))
(format #t "Total run time=~6F seconds.  Each call took ~6F millisecs\n"
	elapsed (* 1000 (/ elapsed nrep)))
Note that the run-time does not depend on the AtomSpace size:
Lets create 400K Atoms:
(for-each (lambda (n)
   (Times (Number n) (Plus (Number (* n 3.14) (Number 1.57)))))
   (iota 100000))
and run the measurement again
---- !#