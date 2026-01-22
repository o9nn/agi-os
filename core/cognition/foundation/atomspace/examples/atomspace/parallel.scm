(use-modules (opencog) (opencog exec))
(define (make-counter)
	(let ((nnn 1))
		(lambda ()
			(format #t "Finished running the thread; exiting!\n")
			(format #t " -- This is the ~A'th we've finished!\n" nnn)
			(format #t " -- The time is ~A\n\n"
				(strftime "%c" (localtime (current-time))))
			(set! nnn (+ nnn 1))
			(stv 1 0))
	))
(define incr (make-counter))
(define pllel
	(Parallel
		(SequentialAnd
			(True (Sleep (Number 1)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 3)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
	))
(cog-evaluate! pllel)
(cog-evaluate! pllel)
(define wait
	(ThreadJoin
		(SequentialAnd
			(True (Sleep (Number 1)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 3)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
	))
(cog-evaluate! wait)
(cog-evaluate! wait)
(cog-evaluate! wait)