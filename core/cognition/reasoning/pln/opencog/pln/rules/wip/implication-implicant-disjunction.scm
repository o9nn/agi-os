(define implication-implicant-disjunction-rule
(let* ((A (VariableNode "$A"))
(B (VariableNode "$B"))
(C (VariableNode "$C"))
(AC (ImplicationLink A C))
(BC (ImplicationLink B C))
(PredicateT (TypeNode "PredicateNode")))
(BindLink
(VariableList
(TypedVariableLink A PredicateT)
(TypedVariableLink B PredicateT)
(TypedVariableLink C PredicateT))
(AndLink
AC
BC
(NotLink (IdenticalLink A B)))
(ExecutionOutputLink
(GroundedSchemaNode "scm: implication-implicant-disjunction-formula")
(ListLink
(SetLink
AC
BC)
(ImplicationLink
(OrLink A B)
C))))))
(define (implication-implicant-disjunction-formula premises ABC)
(let* ((AC (gar premises))
(BC (gdr premises)))
(cog-set-tv! ABC
(implication-implicant-disjunction-side-effect-free-formula AC BC))))
(define (implication-implicant-disjunction-side-effect-free-formula AC BC)
(let*
(
(A (gar AC))
(B (gar BC))
(C (gdr AC))
(sAC (cog-mean AC))
(sBC (cog-mean BC))
(sA (cog-mean A))
(sB (cog-mean B))
(sC (cog-mean C))
(cAC (cog-confidence AC))
(cBC (cog-confidence BC))
(CinterA (* sAC sA))
(CinterB (* sBC sB)))
(stv (/ (+ CinterA CinterB (* CinterA CinterB sA sB -1))
(+ sA sB (* sA sB -1)))
(min cAC cBC))))
(define implication-implicant-disjunction-rule-name
(DefinedSchemaNode "implication-implicant-disjunction-rule"))
(DefineLink implication-implicant-disjunction-rule-name
implication-implicant-disjunction-rule)