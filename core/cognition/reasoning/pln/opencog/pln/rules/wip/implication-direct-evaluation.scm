(use-modules (srfi srfi-1))
(use-modules (opencog logger))
(define implication-direct-evaluation-vardecl
(VariableList
(TypedVariable
(Variable "$P")
(Type "PredicateNode"))
(TypedVariable
(Variable "$Q")
(Type "PredicateNode"))
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))))
(define implication-direct-evaluation-pattern
(And
(Evaluation
(Variable "$P")
(Variable "$X"))
(Evaluation
(Variable "$Q")
(Variable "$X"))
(Not
(Identical
(Variable "$P")
(Variable "$Q")))))
(define implication-direct-evaluation-rewrite
(ExecutionOutput
(GroundedSchema "scm: implication-direct-evaluation-formula")
(List
(Variable "$P")
(Variable "$Q"))))
(define implication-direct-evaluation-rule
(Bind
implication-direct-evaluation-vardecl
implication-direct-evaluation-pattern
implication-direct-evaluation-rewrite))
(define (true-enough? A)
(let* (
(TV (cog-tv A))
(s (cog-tv-mean TV))
(c (cog-tv-confidence TV)))
(and (> s 0.5) (> c 0))))
(define (implication-direct-evaluation-formula P Q)
(let* (
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
(TV-confidence (count->confidence P-length)))
(if (> TV-confidence 0)
(Implication (stv TV-strength TV-confidence) P Q))))
(define implication-direct-evaluation-rule-name
(DefinedSchemaNode "implication-direct-evaluation-rule"))
(DefineLink implication-direct-evaluation-rule-name
implication-direct-evaluation-rule)