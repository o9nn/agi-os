(load "miner-rule-utils.scm")
(define (gen-i-surprisingness-rule mode nary db-ratio)
  (define f-vardecl (Variable "$f-vardecl"))
  (define db (Variable "$db"))
  (define ms (Variable "$ms"))
  (define db-ratio-n (Number db-ratio))
  (define VariableT (Type "VariableNode"))
  (define VariableSetT (Type "VariableSet"))
  (define VariableListT (Type "VariableList"))
  (define varT (TypeChoice VariableT VariableSetT VariableListT))
  (define NumberT (Type "NumberNode"))
  (define ConceptT (Type "ConceptNode"))
  (define typed-f-vardecl (TypedVariable f-vardecl varT))
  (define typed-db (TypedVariable db ConceptT))
  (define typed-ms (TypedVariable ms NumberT))
  (define formula-name (string-append "scm-eager: " (symbol->string mode) "-formula"))
  (define formula (GroundedSchema formula-name))
  (if (< 1 nary)
      (let* ((cnj-bodies (gen-variables "$cnj-bodies" nary))
             (f (Quote
                  (Lambda
                    (Unquote f-vardecl)
                    (Present
                      (map Unquote cnj-bodies)))))
             (f-minsup (minsup-eval f db ms))
             (f-isurp (surp-eval mode f db)))
        (Bind
          (VariableSet
            typed-f-vardecl
            cnj-bodies
            typed-db
            typed-ms)
          (And
            (Present
               f-minsup)
            (Absent
               f-isurp)
            (absolutely-true-eval f-minsup))
          (ExecutionOutput
            formula
            (List
              f-isurp
              f-minsup
	      db-ratio-n))))))
(define (gen-i-surprisingness-formula mode)
  (lambda (conclusion . premises)
    (if (= 2 (length premises))
        (let* ((pat-isurp conclusion)
               (pat-minsup (car premises))
	       (db-ratio-n (cadr premises))
               (pat (get-pattern pat-minsup))
               (db (get-db pat-minsup))
               (isurp-op (cond ((equal? mode 'isurp-old) cog-isurp-old)
                               ((equal? mode 'nisurp-old) cog-nisurp-old)
                               ((equal? mode 'isurp) cog-isurp)
                               ((equal? mode 'nisurp) cog-nisurp)))
               (isurp (isurp-op pat db db-ratio-n)))
          (cog-set-tv! pat-isurp (stv isurp 1))))))
(define isurp-old-formula (gen-i-surprisingness-formula 'isurp-old))
(define nisurp-old-formula (gen-i-surprisingness-formula 'nisurp-old))
(define isurp-formula (gen-i-surprisingness-formula 'isurp))
(define nisurp-formula (gen-i-surprisingness-formula 'nisurp))