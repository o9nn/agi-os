(define (get-timestamp)
   (NumberNode (current-time)))
(DefineLink
   (DefinedSchemaNode "set timestamp")
   (PutLink
      (EvaluationLink (PredicateNode "event-timestamp")
         (ListLink (VariableNode "$ts")))
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: get-timestamp")
         (ListLink))))