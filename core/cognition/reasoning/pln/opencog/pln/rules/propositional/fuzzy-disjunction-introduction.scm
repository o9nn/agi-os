(use-modules (srfi srfi-1))
(use-modules (opencog ure))
(define (gen-fuzzy-disjunction-introduction-rule nary)
(let* ((variables (gen-variables "$X" nary))
(EvaluationT (Type "EvaluationLink"))
(InheritanceT (Type "InheritanceLink"))
(AndT (Type "AndLink"))
(NotT (Type "NotLink"))
(type (TypeChoice EvaluationT InheritanceT AndT NotT))
(gen-typed-variable (lambda (x) (TypedVariable x type)))
(vardecl (VariableList (map gen-typed-variable variables)))
(pattern (Present variables))
(rewrite (ExecutionOutput
(GroundedSchema "scm: fuzzy-disjunction-introduction-formula")
(List (Or variables) (Set variables)))))
(Bind
vardecl
pattern
rewrite)))
(define (fuzzy-disjunction-introduction-formula A S)
(let* ((orees (cog-outgoing-set S))
(max-s-atom (max-element-by-key orees cog-mean))
(min-c-atom (min-element-by-key orees cog-confidence))
(max-s (cog-mean max-s-atom))
(min-c (cog-confidence min-c-atom)))
(cog-merge-hi-conf-tv! A (stv max-s min-c))))
(define fuzzy-disjunction-introduction-1ary-rule-name
(DefinedSchema "fuzzy-disjunction-introduction-1ary-rule"))
(DefineLink
fuzzy-disjunction-introduction-1ary-rule-name
(gen-fuzzy-disjunction-introduction-rule 1))
(define fuzzy-disjunction-introduction-2ary-rule-name
(DefinedSchema "fuzzy-disjunction-introduction-2ary-rule"))
(DefineLink
fuzzy-disjunction-introduction-2ary-rule-name
(gen-fuzzy-disjunction-introduction-rule 2))
(define fuzzy-disjunction-introduction-3ary-rule-name
(DefinedSchema "fuzzy-disjunction-introduction-3ary-rule"))
(DefineLink
fuzzy-disjunction-introduction-3ary-rule-name
(gen-fuzzy-disjunction-introduction-rule 3))
(define fuzzy-disjunction-introduction-4ary-rule-name
(DefinedSchema "fuzzy-disjunction-introduction-4ary-rule"))
(DefineLink
fuzzy-disjunction-introduction-4ary-rule-name
(gen-fuzzy-disjunction-introduction-rule 4))
(define fuzzy-disjunction-introduction-5ary-rule-name
(DefinedSchema "fuzzy-disjunction-introduction-5ary-rule"))
(DefineLink
fuzzy-disjunction-introduction-5ary-rule-name
(gen-fuzzy-disjunction-introduction-rule 5))