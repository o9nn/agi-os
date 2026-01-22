(load "miner-rule-utils.scm")
(define (gen-emp-rule db-ratio)
(let* (
[db-ratio-n (Number db-ratio)]
[pattern (Variable "$pattern")]
[db (Variable "$db")]
[ms (Variable "$ms")]
[LambdaT (Type "LambdaLink")]
[ConceptT (Type "ConceptNode")]
[NumberT (Type "NumberNode")]
[pattern-decl (TypedVariable pattern LambdaT)]
[db-decl (TypedVariable db ConceptT)]
[ms-decl (TypedVariable ms NumberT)]
[minsup-pattern (minsup-eval pattern db ms)])
(Bind
(VariableSet
pattern-decl
db-decl
ms-decl)
(And
(Present minsup-pattern)
(absolutely-true-eval minsup-pattern))
(ExecutionOutput
(GroundedSchema "scm-eager: emp-formula")
(List
(emp-eval pattern db)
minsup-pattern
db-ratio-n)))))
(define (emp-formula conclusion . premises)
(if (= (length premises) 2)
(let* ([minsup-pattern (car premises)]
[db-ratio (cadr premises)]
[pattern (get-pattern minsup-pattern)]
[db (get-db minsup-pattern)]
[emp-tv (cog-emp-tv pattern db db-ratio)])
(cog-set-tv! conclusion emp-tv))))