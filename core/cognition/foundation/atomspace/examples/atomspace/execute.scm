(use-modules (opencog) (opencog exec) (opencog python))
(python-eval "exec(open('my_py_func.py').read())")
(cog-execute!
	(ExecutionOutput
		(GroundedSchema "py:my_py_func")
		(List
			(Concept "1")
			(Concept "2"))))
(cog-evaluate!
	(Evaluation
		(GroundedPredicate "py:my_py_predicate")
		(List
			(Concept "3")
			(Concept "4"))))
(define (my-scm-func atoma atomb)
	(display "My func called with atom arguments\n")
	(display atoma) (display atomb)
	(newline)
	(Concept "I'm returning this atom")
)
(cog-execute!
	(ExecutionOutput
		(GroundedSchema "scm:my-scm-func")
		(List
			(Concept "1")
			(Concept "2"))))
(DefineLink
	(DefinedSchema "x+y*10")
	(Lambda
		(VariableList
			(Variable "$X")
			(Variable "$Y"))
		(Plus
			(Variable "$X")
			(Times
				(Variable "$Y")
				(Number 10)))))
(cog-execute!
	(ExecutionOutput
		(DefinedSchema "x+y*10")
		(List
			(Number "2")
			(Number "4"))))
(cog-execute!
   (Put
      (DefinedSchema "x+y*10")
      (List
         (Number "2")
         (Number "4"))))
(cog-execute!
	(ExecutionOutput
		(Lambda
			(VariableList
				(Variable "$X")
				(Variable "$Y"))
			(Plus
				(Variable "$X")
				(Times
					(Variable "$Y")
					(Number 10))))
		(List
			(Number "2")
			(Number "4"))))