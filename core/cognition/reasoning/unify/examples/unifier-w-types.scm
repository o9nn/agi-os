(use-modules (opencog) (opencog exec))
(use-modules (opencog unify))
(define LHS
(Exists
(TypedVariable
(Variable "?MEMBER")
(TypeChoice
(Type 'ConceptNode)
(Type 'SchemaNode)
(Type 'PredicateNode)))
(MemberLink
(Variable "?MEMBER")
(Concept "Org1-1"))))
(define RHS
(ExistsLink
(TypedVariable
(Variable "?OBJ")
(TypeChoice
(Type 'ConceptNode)
(Type 'SchemaNode)
(Type 'PredicateNode)))
(Member
(Variable "?OBJ")
(Variable "?COLL-6c74a409"))))
(define RHS_vardecl
(TypedVariable
(Variable "?COLL-6c74a409")
(TypeChoice
(Type 'ConceptNode)
(Type 'SchemaNode)
(Type 'PredicateNode))))
(define joiner
(Unifier
LHS
(Lambda RHS_vardecl RHS)
(List (Concept "I found this:") (Variable "?COLL-6c74a409"))))
(cog-execute! joiner)
(define join-alt
(Unifier
(Lambda (VariableList) LHS)
(Lambda RHS_vardecl RHS)
(List (Concept "I found this:") (Variable "?COLL-6c74a409"))))
(cog-execute! join-alt)