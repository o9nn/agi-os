(use-modules (opencog) (opencog exec))
(define (incr-counts THING-A THING-B)
	(cog-inc-count! (List THING-A THING-B) 1.0)
	(cog-inc-count! (List (AnyNode "left wildcard") THING-B) 1.0)
	(cog-inc-count! (List THING-A (AnyNode "right wildcard")) 1.0)
	(cog-inc-count! (AnyNode "grand total") 1.0))
(define (observe STRING-A STRING-B)
	(incr-counts (Concept STRING-A) (Concept STRING-B)))
(observe "hello" "world")
(observe "hello" "Sue")
(observe "hello" "Adrian")
(observe "goodbye" "Adrian")
(observe "goodbye" "Mike")
(define tvp (PredicateNode "*-TruthValueKey-*"))
(DefineLink
	(DefinedProcedure "dynamic MI")
	(Lambda
		(VariableList (Variable "$L") (Variable "$R"))
		(Log2
			(Divide
				(Times
					(FloatValueOf (List (Variable "$L") (Variable "$R")) tvp)
					(FloatValueOf (AnyNode "grand total") tvp))
				(Times
					(FloatValueOf (List (Variable "$L") (Any "right wildcard")) tvp)
					(FloatValueOf (List (Any "left wildcard") (Variable "$R")) tvp))))))
(define (install-formula THING-A THING-B)
	(define pair (List THING-A THING-B))
	(cog-set-value! pair (Predicate "MI Key")
		(FormulaStream
			(ExecutionOutput (DefinedProcedure "dynamic MI") pair))))
(define (install-mi STRING-A STRING-B)
	(install-formula (Concept STRING-A) (Concept STRING-B)))
(define (get-computed-value THING-A THING-B)
	(cog-value (List THING-A THING-B) (Predicate "MI Key")))
(define (get-mi-stream STRING-A STRING-B)
	(get-computed-value (Concept STRING-A) (Concept STRING-B)))
(install-mi "hello" "world")
(cog-set-value! (Concept "someplace") (Predicate "mask key")
	(BoolValue 0 0 1))
(define (make-deci ATOM)
	(Decimate
		(BoolValueOf (Concept "someplace") (Predicate "mask key"))
		(FloatValueOf ATOM tvp)))
(DefineLink
	(DefinedProcedure "scalar MI")
	(Lambda
		(VariableList (Variable "$L") (Variable "$R"))
		(Log2
			(Divide
				(Times
					(make-deci (List (Variable "$L") (Variable "$R")))
					(make-deci (AnyNode "grand total")))
				(Times
					(make-deci (List (Variable "$L") (Any "right wildcard")))
					(make-deci (List (Any "left wildcard") (Variable "$R"))))))))
(define (install-scalar THING-A THING-B)
	(define pair (List THING-A THING-B))
	(cog-set-value! pair (Predicate "Alt MI Key")
		(FormulaStream
			(ExecutionOutput (DefinedProcedure "scalar MI") pair))))
(define (install-scalar-mi STRING-A STRING-B)
	(install-scalar (Concept STRING-A) (Concept STRING-B)))
(define (get-computed-scalar THING-A THING-B)
	(cog-value (List THING-A THING-B) (Predicate "Alt MI Key")))
(define (get-mi-scalar STRING-A STRING-B)
	(get-computed-scalar (Concept STRING-A) (Concept STRING-B)))
(install-scalar-mi "hello" "world")