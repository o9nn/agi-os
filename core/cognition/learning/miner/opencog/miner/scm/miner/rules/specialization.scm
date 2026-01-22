(load "miner-rule-utils.scm")
(define specialization-rule
  (let* (
         (g (Variable "$g"))
         (db (Variable "$db"))
         (ms (Variable "$ms"))
         (xs-f (Variable "$xs-f"))
         (NumberT (Type "NumberNode"))
         (LambdaT (Type "LambdaLink"))
         (PutT (Type "PutLink"))
         (ConceptT (Type "ConceptNode"))
         (g-decl (TypedVariable g (TypeChoice LambdaT PutT)))
         (db-decl (TypedVariable db ConceptT))
         (ms-decl (TypedVariable ms NumberT))
         (xs-f-decl xs-f)
         (vardecl (VariableSet g-decl db-decl ms-decl xs-f-decl))
         (minsup-g (minsup-eval g db ms))
         (shabs-eval (abstraction-eval xs-f minsup-g))
         (precond-1 (absolutely-true-eval minsup-g))
         (precond-2 (absolutely-true-eval shabs-eval))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm-eager: specialization-formula")
                    (List
                      (minsup-eval
                        (Quote (Put
                          (Unquote g)
                          (Unquote xs-f)))
                        db
                        ms)
                      minsup-g
                      shabs-eval))))
    (Bind
      vardecl
      (And (Present shabs-eval) precond-1 precond-2)
      rewrite)))
(define (specialization-formula conclusion . premises)
  (if (= (length premises) 2)
      (let* ((con-minsup-args (gdr conclusion))
             (pre-minsup-pred (car premises))
             (pre-minsup-pred-tv (cog-tv pre-minsup-pred))
             (gf (cog-outgoing-atom con-minsup-args 0))
             (db (cog-outgoing-atom con-minsup-args 1))
             (ms-atom (cog-outgoing-atom con-minsup-args 2))
             (conclusion-tv (if (and (tv->bool pre-minsup-pred-tv)
                                     (cog-enough-support? gf db ms-atom))
                                (stv 1 1)
                                #f))
             (reduced-conclusion (cog-execute! conclusion)))
        (if conclusion-tv
            (cog-set-tv! reduced-conclusion conclusion-tv)))))
(define specialization-rule-name
  (DefinedSchemaNode "specialization-rule"))
(DefineLink specialization-rule-name
  specialization-rule)