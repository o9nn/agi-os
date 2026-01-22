(load "miner-rule-utils.scm")
(define shallow-abstraction-rule
  (let* (
         (g (Variable "$g"))
         (db (Variable "$db"))
         (ms (Variable "$ms"))
         (LambdaT (Type "LambdaLink"))
         (PutT (Type "PutLink"))
         (ConceptT (Type "ConceptNode"))
         (NumberT (Type "NumberNode"))
         (g-decl (TypedVariable g (TypeChoice LambdaT PutT)))
         (db-decl (TypedVariable db ConceptT))
         (ms-decl (TypedVariable ms NumberT))
         (minsup-g (minsup-eval g db ms)))
  (Bind
    (VariableSet
      g-decl
      db-decl
      ms-decl)
    (And
      (Present minsup-g)
      (absolutely-true-eval minsup-g))
    (ExecutionOutput
      (GroundedSchema "scm-eager: shallow-abstraction-formula")
      (List
        (Set)
        minsup-g)))))
(define (shallow-abstraction-formula conclusion . premises)
  (if (= (length premises) 1)
      (let* ((minsup-g (car premises))
             (g (get-pattern minsup-g))
             (db (get-db minsup-g))
             (ms (get-ms minsup-g))
             (shabs-lists (cog-shallow-abstract g db ms))
             (list->eval (lambda (x) (cog-set-tv!
                                      (abstraction-eval x minsup-g)
                                      (stv 1 1))))
             (shabs-evals (map list->eval (cog-outgoing-set shabs-lists))))
        (Set shabs-evals))))
(define shallow-abstraction-rule-name
  (DefinedSchemaNode "shallow-abstraction-rule"))
(DefineLink shallow-abstraction-rule-name
  shallow-abstraction-rule)