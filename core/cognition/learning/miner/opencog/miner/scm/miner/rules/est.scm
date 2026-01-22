(load "miner-rule-utils.scm")
(define (gen-est-rule)
(let* (
(pattern (Variable "$pattern"))
(db (Variable "$db"))
(ms (Variable "$ms"))
(LambdaT (Type "LambdaLink"))
(ConceptT (Type "ConceptNode"))
(NumberT (Type "NumberNode"))
(pattern-decl (TypedVariable pattern LambdaT))
(db-decl (TypedVariable db ConceptT))
(ms-decl (TypedVariable ms NumberT))
(minsup-pattern (minsup-eval pattern db ms)))
(Bind
(VariableSet
pattern-decl
db-decl
ms-decl)
(And
(Present minsup-pattern)
(absolutely-true-eval minsup-pattern))
(ExecutionOutput
(GroundedSchema "scm-eager: est-formula")
(List
(est-eval pattern db)
minsup-pattern)))))
(define (est-formula conclusion . premises)
(if (= (length premises) 1)
(let* ((minsup-pattern (car premises))
(pattern (get-pattern minsup-pattern))
(db (get-db minsup-pattern))
(est-tv (cog-ji-tv-est pattern db)))
(cog-set-tv! conclusion est-tv))))