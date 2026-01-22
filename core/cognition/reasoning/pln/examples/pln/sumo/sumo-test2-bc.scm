(ure-logger-set-level! "debug")
(load "pln-config2.scm")
(MemberLink (stv 1.000000 1.000000)
(ConceptNode "Entity2-2" (stv 0.010000 1.000000))
(ConceptNode "Organism" (stv 0.010000 1.000000))
)
(MemberLink (stv 1.000000 1.000000)
(ConceptNode "Entity2-1" (stv 0.010000 1.000000))
(ConceptNode "Organism" (stv 0.010000 1.000000))
)
(MemberLink (stv 1.000000 1.000000)
(ConceptNode "Inconsistent" (stv 0.010000 1.000000))
(ConceptNode "Attribute" (stv 0.010000 1.000000))
)
(MemberLink (stv 1.000000 1.000000)
(ConceptNode "TheKB2-1" (stv 0.010000 1.000000))
(ConceptNode "ComputerProgram" (stv 0.010000 1.000000))
)
(ImplicationScopeLink (stv 1.000000 1.000000)
(VariableList
(TypedVariableLink
(VariableNode "?ATTR1")
(TypeChoice
(TypeNode "ConceptNode")
(TypeNode "SchemaNode")
(TypeNode "PredicateNode")
)
)
(TypedVariableLink
(VariableNode "?ATTR2")
(TypeChoice
(TypeNode "ConceptNode")
(TypeNode "SchemaNode")
(TypeNode "PredicateNode")
)
)
(TypedVariableLink
(VariableNode "?X")
(TypeChoice
(TypeNode "ConceptNode")
(TypeNode "SchemaNode")
(TypeNode "PredicateNode")
)
)
)
(AndLink
(EvaluationLink
(PredicateNode "property" (stv 0.100000 1.000000))
(ListLink
(VariableNode "?X")
(VariableNode "?ATTR1")
)
)
(EvaluationLink
(PredicateNode "property" (stv 0.100000 1.000000))
(ListLink
(VariableNode "?X")
(VariableNode "?ATTR2")
)
)
(EvaluationLink
(PredicateNode "contraryAttribute" (stv 0.100000 1.000000))
(ListLink
(VariableNode "?ATTR1")
(VariableNode "?ATTR2")
)
)
)
(EvaluationLink
(PredicateNode "property" (stv 0.100000 1.000000))
(ListLink
(ConceptNode "TheKB2-1" (stv 0.010000 1.000000))
(ConceptNode "Inconsistent" (stv 0.010000 1.000000))
)
)
)
(EvaluationLink (stv 1.000000 1.000000)
(PredicateNode "father" (stv 0.100000 1.000000))
(ListLink
(ConceptNode "Entity2-1" (stv 0.010000 1.000000))
(ConceptNode "Entity2-2" (stv 0.010000 1.000000))
)
)
(EvaluationLink (stv 1.000000 1.000000)
(PredicateNode "mother" (stv 0.100000 1.000000))
(ListLink
(ConceptNode "Entity2-1" (stv 0.010000 1.000000))
(ConceptNode "Entity2-2" (stv 0.010000 1.000000))
)
)
(define target
(EvaluationLink
(PredicateNode "property" (stv 0.100000 1.000000))
(ListLink
(ConceptNode "TheKB2-1" (stv 0.010000 1.000000))
(ConceptNode "Inconsistent" (stv 0.010000 1.000000))
)
)
)
(pln-bc target)