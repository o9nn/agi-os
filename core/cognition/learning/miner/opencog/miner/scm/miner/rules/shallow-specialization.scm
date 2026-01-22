(load "miner-rule-utils.scm")
(define (gen-shallow-specialization-rule nary mv enable-type enable-glob ignore-vars)
  (let* (
         (pattern-vardecl (Variable "$vardecl"))
         (cnjs (gen-variables "$cnj" nary))
         (db (Variable "$db"))
         (ms (Variable "$ms"))
         (LambdaT (Type "LambdaLink"))
         (ConceptT (Type "ConceptNode"))
         (NumberT (Type "NumberNode"))
         (pattern-vardecl-decl pattern-vardecl)
         (cnjs-decls cnjs)
         (db-decl (TypedVariable db ConceptT))
         (ms-decl (TypedVariable ms NumberT))
         (pattern (Quote (Lambda (Unquote pattern-vardecl)
                                 (Present (map Unquote cnjs)))))
         (minsup-pattern (minsup-eval pattern db ms)))
  (define (btoi b) (if b 1 0))
  (Bind
    (VariableSet
      pattern-vardecl-decl
      cnjs-decls
      db-decl
      ms-decl)
    (And
      (Present minsup-pattern)
      (absolutely-true-eval minsup-pattern))
    (ExecutionOutput
      (GroundedSchema (string-append "scm-eager: shallow-specialization-mv-"
                                     (number->string mv)
                                     "-formula"))
      (List
        (Set)
        minsup-pattern
        (cog-new-node 'PredicateNode "enable-type" (cog-new-stv (btoi enable-type) 1))
        (cog-new-node 'PredicateNode "enable-glob" (cog-new-stv (btoi enable-glob) 1))
        ignore-vars)))))
(define (gen-shallow-specialization-formula mv)
  (lambda (conclusion . premises)
    (if (= (length premises) 4)
        (let* ((minsup-pattern (car premises))
               (enable-type (cadr premises))
               (enable-glob (caddr premises))
               (ignore-vars (cadddr premises))
               (pattern (get-pattern minsup-pattern))
               (db (get-db minsup-pattern))
               (ms (get-ms minsup-pattern))
               (shaspes (cog-shallow-specialize pattern db ms (Number mv) enable-type enable-glob ignore-vars))
               (minsup-shaspe (lambda (x) (cog-set-tv!
                                           (minsup-eval x db ms)
                                           (stv 1 1))))
               (minsup-shaspes (map minsup-shaspe (cog-outgoing-set shaspes))))
          (Set minsup-shaspes)))))
(define shallow-specialization-mv-1-formula (gen-shallow-specialization-formula 1))
(define shallow-specialization-mv-2-formula (gen-shallow-specialization-formula 2))
(define shallow-specialization-mv-3-formula (gen-shallow-specialization-formula 3))
(define shallow-specialization-mv-4-formula (gen-shallow-specialization-formula 4))
(define shallow-specialization-mv-5-formula (gen-shallow-specialization-formula 5))
(define shallow-specialization-mv-6-formula (gen-shallow-specialization-formula 6))
(define shallow-specialization-mv-7-formula (gen-shallow-specialization-formula 7))
(define shallow-specialization-mv-8-formula (gen-shallow-specialization-formula 8))
(define shallow-specialization-mv-9-formula (gen-shallow-specialization-formula 9))