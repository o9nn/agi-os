(use-modules (srfi srfi-1))
(use-modules (opencog logger))
(define implication-scope-direct-evaluation-vardecl
(VariableSet
(TypedVariable
(Variable "$P")
(Type "PredicateNode"))
(TypedVariable
(Variable "$Q")
(Type "PredicateNode"))
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))))
(define implication-scope-direct-evaluation-pattern
(And
(PresentLink
(Evaluation
(Variable "$P")
(Variable "$X"))
(Evaluation
(Variable "$Q")
(Variable "$X")))
(Not
(Equal
(Variable "$P")
(Variable "$Q")))))
(define implication-scope-direct-evaluation-rewrite
(ExecutionOutput
(GroundedSchema "scm-eager: implication-scope-direct-evaluation")
(List
(Variable "$P")
(Variable "$Q"))))
(define implication-scope-direct-evaluation-rule
(Bind
implication-scope-direct-evaluation-vardecl
implication-scope-direct-evaluation-pattern
implication-scope-direct-evaluation-rewrite))
(define (true-enough? A)
(and (> (cog-mean A) 0.5) (> (cog-conf A) 0)))
(define (implication-scope-direct-evaluation P Q)
(let* (
(K 800)
(X (Variable "$X"))
(vardecl (TypedVariable X (Type "ConceptNode")))
(term->instance (lambda (p x) (Evaluation p x)))
(true-enough-term? (lambda (p x) (true-enough? (term->instance p x))))
(fetch-true-enough-terms
(lambda (p)
(let* ((query (Get vardecl (term->instance p X)))
(terms (cog-outgoing-set (cog-execute! query))))
(filter (lambda (x) (true-enough-term? p x)) terms))))
(P-true-enough-terms (fetch-true-enough-terms P))
(Q-true-enough-terms (fetch-true-enough-terms Q))
(P-length (length P-true-enough-terms))
(P-inter-Q-terms (lset-intersection equal?
P-true-enough-terms
Q-true-enough-terms))
(P-inter-Q-length (length P-inter-Q-terms))
(TV-strength (if (> P-length 0)
(exact->inexact (/ P-inter-Q-length P-length))
0))
(TV-confidence (exact->inexact (/ P-length K)))
(P-body (Evaluation P X))
(Q-body (Evaluation Q X)))
(if (> TV-confidence 0)
(ImplicationScope (stv TV-strength TV-confidence) vardecl P-body Q-body))))
(define implication-scope-direct-evaluation-rule-name
(DefinedSchemaNode "implication-scope-direct-evaluation-rule"))
(DefineLink implication-scope-direct-evaluation-rule-name
implication-scope-direct-evaluation-rule)