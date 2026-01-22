(use-modules (srfi srfi-1))
(define negation-introduction-rule
  (let* ((X (Variable "$X"))
         (EvaluationT (Type "EvaluationLink"))
         (InheritanceT (Type "InheritanceLink"))
         (PredicateT (Type "PredicateNode"))
         (ConceptT (Type "ConceptNode"))
         (AndT (Type "AndLink"))
         (OrT (Type "OrLink"))
         (type (TypeChoice EvaluationT InheritanceT PredicateT ConceptT))
         (gen-typed-variable (lambda (x) (TypedVariable x type)))
         (vardecl (TypedVariable X type))
         (pattern (And
                    X
                    (Evaluation
                      (GroundedPredicate "scm: gt-zero-confidence")
                      X)))
         (rewrite (ExecutionOutput
                    (GroundedSchema "scm: negation-introduction-formula")
                    (List (Not X) X))))
    (Bind
      vardecl
      pattern
      rewrite)))
(define (negation-introduction-formula N A)
  (let* ((A-s (cog-mean A))
         (A-c (cog-confidence A)))
    (cog-merge-hi-conf-tv! N (stv (- 1 A-s) A-c))))
(define negation-introduction-rule-name
  (DefinedSchema "negation-introduction-rule"))
(DefineLink
  negation-introduction-rule-name
  negation-introduction-rule)