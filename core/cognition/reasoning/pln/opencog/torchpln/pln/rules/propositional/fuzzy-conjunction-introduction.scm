(use-modules (srfi srfi-1))
(use-modules (opencog ure))
(define (gen-fuzzy-conjunction-introduction-rule nary)
(let* ((variables (gen-variables "$X" nary))
(EvaluationT (Type "EvaluationLink"))
(InheritanceT (Type "InheritanceLink"))
(OrT (Type "OrLink"))
(NotT (Type "NotLink"))
(ExecutionT (Type "ExecutionLink"))
(type (TypeChoice EvaluationT InheritanceT OrT NotT ExecutionT))
(gen-typed-variable (lambda (x) (TypedVariable x type)))
(vardecl (VariableList (map gen-typed-variable variables)))
(pattern (Present variables))
(rewrite (ExecutionOutput
(GroundedSchema "py:pln.rules.propositional.fuzzy_conjunction_introduction_formula")
(List (And variables) (Set variables)))))
(Bind
vardecl
pattern
rewrite)))
(define (fuzzy-conjunction-introduction-formula A S)
(let* ((andees (cog-outgoing-set S))
(min-s-atom (min-element-by-key andees cog-mean))
(min-c-atom (min-element-by-key andees cog-confidence))
(min-s (cog-mean min-s-atom))
(min-c (cog-confidence min-c-atom)))
(cog-merge-hi-conf-tv! A (stv min-s min-c))))
(define fuzzy-conjunction-introduction-1ary-rule-name
(DefinedSchema "fuzzy-conjunction-introduction-1ary-rule"))
(DefineLink
fuzzy-conjunction-introduction-1ary-rule-name
(gen-fuzzy-conjunction-introduction-rule 1))
(define fuzzy-conjunction-introduction-2ary-rule-name
(DefinedSchema "fuzzy-conjunction-introduction-2ary-rule"))
(DefineLink
fuzzy-conjunction-introduction-2ary-rule-name
(gen-fuzzy-conjunction-introduction-rule 2))
(define fuzzy-conjunction-introduction-3ary-rule-name
(DefinedSchema "fuzzy-conjunction-introduction-3ary-rule"))
(DefineLink
fuzzy-conjunction-introduction-3ary-rule-name
(gen-fuzzy-conjunction-introduction-rule 3))
(define fuzzy-conjunction-introduction-4ary-rule-name
(DefinedSchema "fuzzy-conjunction-introduction-4ary-rule"))
(DefineLink
fuzzy-conjunction-introduction-4ary-rule-name
(gen-fuzzy-conjunction-introduction-rule 4))
(define fuzzy-conjunction-introduction-5ary-rule-name
(DefinedSchema "fuzzy-conjunction-introduction-5ary-rule"))
(DefineLink
fuzzy-conjunction-introduction-5ary-rule-name
(gen-fuzzy-conjunction-introduction-rule 5))