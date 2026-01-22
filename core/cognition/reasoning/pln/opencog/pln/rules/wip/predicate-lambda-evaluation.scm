(define predicate-lambda-evaluation-vardecl
  (VariableList
     (TypedVariable
        (Variable "$V")
        (TypeChoice
           (Type "TypedVariableLink")
           (Type "VariableList")
           (Type "VariableNode")))
     (Variable "$P")
     (Variable "$Args")))
(define predicate-lambda-evaluation-clause
  (Quote (Lambda
    (Unquote (VariableNode "$V"))
    (Unquote (EvaluationLink
      (Variable "$P")
      (Variable "$Args"))))))
(define predicate-lambda-evaluation-precondition
  (Evaluation
    (GroundedPredicate "scm: gt-zero-confidence")
    (Variable "$P")))
(define predicate-lambda-evaluation-pattern
  (And
    predicate-lambda-evaluation-clause
    predicate-lambda-evaluation-precondition))
(define predicate-lambda-evaluation-rewrite
  (ExecutionOutputLink
     (GroundedSchemaNode "scm: predicate-lambda-evaluation-formula")
     (ListLink
       predicate-lambda-evaluation-clause
       (Variable "$P"))))
(define predicate-lambda-evaluation-rule
  (BindLink
     predicate-lambda-evaluation-vardecl
     predicate-lambda-evaluation-pattern
     predicate-lambda-evaluation-rewrite))
(define (predicate-lambda-evaluation-formula lamb pred)
    (if (< 0 (cog-confidence pred))
        (cog-merge-hi-conf-tv! lamb (cog-tv pred))))
(define predicate-lambda-evaluation-rule-name
  (DefinedSchemaNode "predicate-lambda-evaluation-rule"))
(DefineLink predicate-lambda-evaluation-rule-name
  predicate-lambda-evaluation-rule)