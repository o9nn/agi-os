(use-modules (opencog) (opencog exec))
(Inheritance    (Concept "A")    (Concept "B"))
(Inheritance    (Concept "B")    (Concept "C"))
(Inheritance    (Concept "B")    (Concept "F"))
(Inheritance    (Concept "C")    (Concept "D"))
(define into-form
	(Evaluation (Predicate "yikes")
		(ListLink (Variable "$head") (Variable "$tail"))))
(DefineLink
	(DefinedSchemaNode "make-an-edge")
	(Lambda
		(VariableList (Variable "$h") (Variable "$t"))
		(PutLink
			(VariableList (Variable "$head") (Variable "$tail"))
			into-form
			(List (Variable "$h") (Variable "$t")))))
(define mk-edge
	(ExecutionOutput
		(DefinedSchemaNode "make-an-edge")
		(List (Concept "X") (Concept "Y"))))
(define get-form
	(Inheritance (Variable "$head") (Variable "$tail")))
(DefineLink
	(DefinedSchemaNode "get-the-tail")
	(Lambda
		(Variable "$head")
		(GetLink
			(TypedVariable (Variable "$tail") (Type 'ConceptNode))
			get-form)))
(define get-tl
	(ExecutionOutput
		(DefinedSchemaNode "get-the-tail")
		(List (Concept "A"))))
(DefineLink
	(DefinedSchemaNode "rewrite-one")
	(Lambda
		(Variable "$hd")
		(ExecutionOutput
			(DefinedSchemaNode "make-an-edge")
			(List
				(Variable "$hd")
				(ExecutionOutput
					(DefinedSchemaNode "get-the-tail")
					(List (Variable "$hd")))))))
(define rw-one
	(ExecutionOutput
		(DefinedSchemaNode "rewrite-one")
		(List (Concept "A"))))
(DefineLink
	(DefinedSchemaNode "no-op")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(List (Variable "$hd") (Variable "$out"))))
(DefineLink
	(DefinedSchemaNode "test-rewrite")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(ExecutionOutput
			(DefinedSchemaNode "no-op")
			(List
				(ExecutionOutputLink
					(DefinedSchema "get-the-tail")
					(List
						(Variable "$hd")))
				(ExecutionOutput
					(DefinedSchemaNode "make-an-edge")
					(List
						(Variable "$hd")
						(Variable "$out")))))))
(define nest
	(ExecutionOutput
		(DefinedSchema "test-rewrite")
		(List (Concept "A") (Concept "root"))))
(DefineLink
	(DefinedSchemaNode "reversive-rewrite")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(Cond
			(Equal (Variable "$hd") (Set))
			(Variable "$out")
			(ExecutionOutput
				(DefinedSchemaNode "reversive-rewrite")
					(List
						(ExecutionOutputLink
							(DefinedSchema "get-the-tail")
							(List
								(Variable "$hd")))
						(ExecutionOutput
							(DefinedSchemaNode "make-an-edge")
							(List
								(Variable "$hd")
								(Variable "$out"))))))))
(define reversive
	(ExecutionOutput
		(DefinedSchema "reversive-rewrite")
		(List (Concept "A") (Concept "root"))))
(define reversive-result
	(SetLink
		(EvaluationLink (PredicateNode "yikes") (ListLink
			(ConceptNode "F")
			(EvaluationLink (PredicateNode "yikes") (ListLink
				(ConceptNode "B")
				(EvaluationLink (PredicateNode "yikes") (ListLink
					(ConceptNode "A")
					(ConceptNode "root")))))))
		(EvaluationLink (PredicateNode "yikes") (ListLink
			(ConceptNode "D")
			(EvaluationLink (PredicateNode "yikes") (ListLink
				(ConceptNode "C")
				(EvaluationLink (PredicateNode "yikes") (ListLink
					(ConceptNode "B")
					(EvaluationLink (PredicateNode "yikes") (ListLink
						(ConceptNode "A")
						(ConceptNode "root"))))))))))
)
(DefineLink
	(DefinedSchemaNode "unwrap")
	(Lambda
		(VariableList (Variable "$set"))
		(Cond
			(Equal (Set) (Bind (Glob "$elts")
				(Equal (Variable "$set") (Set (Glob "$elts")))
				(List (Glob "$elts"))))
			(Variable "$set")
			(Bind (Glob "$elts")
				(Equal (Variable "$set") (Set (Glob "$elts")))
				(List (Glob "$elts"))))))
(define unwrap-set
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Set (Concept "X") (Concept "Y"))))
(define unwrap-singleton
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Concept "X")))
(define unwrap-natural
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Get (TypedVariable (Variable "$x") (Type 'ConceptNode))
				(Inheritance (Concept "B") (Variable "$x")))))
(DefineLink
	(DefinedSchemaNode "make-a-tree")
	(Lambda
		(VariableList (Variable "$h") (Variable "$set"))
		(PutLink
			(VariableList (Variable "$head") (Variable "$tail"))
			into-form
			(List (Variable "$h")
				(ExecutionOutput
					(DefinedSchema "unwrap")
						(Variable "$set"))))))
(define mk-tree
	(ExecutionOutput
		(DefinedSchemaNode "make-a-tree")
		(List (Concept "head") (Set (Concept "X") (Concept "Y") (Concept"Z")))))
(define mk-tree-indirect
	(ExecutionOutput
		(DefinedSchemaNode "make-a-tree")
		(List (Concept "B")
			(Get (TypedVariable (Variable "$x") (Type 'ConceptNode))
				(Inheritance (Concept "B") (Variable "$x"))))))
(DefineLink
	(DefinedSchemaNode "recursive-rewrite")
	(Lambda
		(VariableList (Variable "$hd"))
		(Cond
			(Equal (Set)
				(ExecutionOutputLink
					(DefinedSchema "get-the-tail")
						(List
							(Variable "$hd"))))
			(Variable "$hd")
			(ExecutionOutput
				(DefinedSchemaNode "make-a-tree")
				(List
					(Variable "$hd")
					(ExecutionOutput
						(DefinedSchemaNode "recursive-rewrite")
						(List
							(ExecutionOutputLink
								(DefinedSchema "get-the-tail")
								(List
									(Variable "$hd"))))))))))
(define recursive
	(ExecutionOutput
		(DefinedSchema "recursive-rewrite")
		(List (Concept "A"))))
*unspecified*