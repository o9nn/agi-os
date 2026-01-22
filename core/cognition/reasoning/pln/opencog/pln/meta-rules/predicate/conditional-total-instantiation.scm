(use-modules (srfi srfi-1))
(use-modules (opencog exec))
(use-modules (opencog logger))
(define (conditional-total-instantiation-tv-formula Pinst-tv Impl-tv P-tv)
(let* ((Impl-s (cog-tv-mean Impl-tv))
(Impl-c (cog-tv-confidence Impl-tv))
(P-s (cog-tv-mean P-tv))
(P-c (cog-tv-confidence P-tv))
(P-s (if (and (< 0.99 P-s) (<= P-c 0)) 0.25 P-s))
(Pinst-s (cog-tv-mean Pinst-tv))
(Pinst-c (cog-tv-confidence Pinst-tv))
(Qinst-s (* Impl-s Pinst-s))
(Qinst-c (* Impl-c Pinst-c (if (< 0.99 Qinst-s) 1 (- 1 P-s)))))
(stv Qinst-s Qinst-c)))
(define conditional-total-instantiation-implication-scope-meta-rule
(let* ((V (Variable "$V"))
(VariableT (Type "VariableNode"))
(VariableListT (Type "VariableList"))
(TypedVariableT (Type "TypedVariableLink"))
(VardeclT (TypeChoice VariableT VariableListT TypedVariableT))
(P (Variable "$P"))
(Q (Variable "$Q"))
(meta-vardecl (VariableList
(TypedVariable V VardeclT)
P Q))
(implication (Quote
(ImplicationScope
(Unquote V)
(Unquote P)
(Unquote Q))))
(meta-precondition (Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
implication))
(meta-pattern (And (Present implication) meta-precondition))
(produced-vardecl V)
(produced-precondition (Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
P))
(produced-pattern (And P produced-precondition))
(produced-rewrite (ExecutionOutput
(GroundedSchema "scm: conditional-total-instantiation-scope-formula")
(Unquote
(List
Q
P
implication))))
(meta-rewrite (Quote (Bind
(Unquote produced-vardecl)
(Unquote produced-pattern)
produced-rewrite
))))
(Bind
meta-vardecl
meta-pattern
meta-rewrite)))
(define (conditional-total-instantiation-scope-formula Qinst Pinst Impl)
(let* ((Impl-outgoings (cog-outgoing-set Impl))
(P (cadr Impl-outgoings))
(Pinst-tv (cog-tv Pinst))
(Impl-tv (cog-tv Impl))
(P-tv (cog-tv P))
(Qinst-tv (conditional-total-instantiation-tv-formula Pinst-tv Impl-tv P-tv)))
(if (< 0 (cog-tv-confidence Qinst-tv))
(cog-merge-hi-conf-tv! Qinst Qinst-tv))))
(define conditional-total-instantiation-implication-scope-meta-rule-name
(DefinedSchemaNode "conditional-total-instantiation-implication-scope-meta-rule"))
(DefineLink conditional-total-instantiation-implication-scope-meta-rule-name
conditional-total-instantiation-implication-scope-meta-rule)
(define (impl-to-eval-type impl-type)
(if (cog-subtype? 'ImplicationLink impl-type)
'EvaluationLink
'MemberLink))
(define (impl-to-var-type impl-type)
(if (cog-subtype? 'ImplicationLink impl-type)
(Type "PredicateNode")
(Type "ConceptNode")))
(define (eval-type-to-swap eval-type)
(equal? 'MemberLink eval-type))
(define (gen-conditional-total-instantiation-meta-rule impl-type)
(let* ((impl-arg-type-atom (impl-to-var-type impl-type))
(eval-type (impl-to-eval-type impl-type))
(swapped (eval-type-to-swap eval-type))
(eval (lambda (A X) (if swapped
(cog-new-link eval-type X A)
(cog-new-link eval-type A X))))
(X (Variable "$X"))
(A (Variable "$A"))
(B (Variable "$B"))
(UA (Unquote A))
(UB (Unquote B))
(UA_X (eval UA X))
(UB_X (eval UB X))
(meta-vardecl (VariableList
(TypedVariable A impl-arg-type-atom)
(TypedVariable B impl-arg-type-atom)))
(AB (cog-new-link impl-type A B))
(meta-precondition (Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
AB))
(meta-pattern (And AB meta-precondition))
(produced-vardecl X)
(produced-clause UA_X)
(produced-precondition (Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
UA_X))
(produced-pattern (And
produced-clause
produced-precondition))
(UAUB (cog-new-link impl-type UA UB))
(produced-rewrite (ExecutionOutput
(GroundedSchema "scm: conditional-total-instantiation-formula")
(List
UB_X
UA_X
UAUB)))
(meta-rewrite (Quote
(Bind
produced-vardecl
produced-pattern
produced-rewrite))))
(Bind
meta-vardecl
meta-pattern
meta-rewrite)))
(define (conditional-total-instantiation-formula Qinst Pinst Impl)
(let* ((Impl-outgoings (cog-outgoing-set Impl))
(P (car Impl-outgoings))
(Pinst-tv (cog-tv Pinst))
(Impl-tv (cog-tv Impl))
(P-tv (cog-tv P))
(Qinst-tv (conditional-total-instantiation-tv-formula Pinst-tv Impl-tv P-tv)))
(if (< 0 (cog-tv-confidence Qinst-tv))
(cog-merge-hi-conf-tv! Qinst Qinst-tv))))
(define conditional-total-instantiation-implication-meta-rule
(gen-conditional-total-instantiation-meta-rule 'ImplicationLink))
(define conditional-total-instantiation-implication-meta-rule-name
(DefinedSchemaNode "conditional-total-instantiation-implication-meta-rule"))
(DefineLink conditional-total-instantiation-implication-meta-rule-name
conditional-total-instantiation-implication-meta-rule)
(define conditional-total-instantiation-inheritance-meta-rule
(gen-conditional-total-instantiation-meta-rule 'InheritanceLink))
(define conditional-total-instantiation-inheritance-meta-rule-name
(DefinedSchemaNode "conditional-total-instantiation-inheritance-meta-rule"))
(DefineLink conditional-total-instantiation-inheritance-meta-rule-name
conditional-total-instantiation-inheritance-meta-rule)