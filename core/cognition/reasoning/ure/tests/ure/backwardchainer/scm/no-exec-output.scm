(use-modules (opencog ure))
(Inheritance (stv 1.0 1.0)
(Concept "American")
(Concept "person"))
(Inheritance (stv 1.0 1.0)
(Concept "cat")
(Concept "pet"))
(define keep-pet-rule
(let* ((kp (Predicate "keep-pet"))
(like (Predicate "like"))
(vA (Variable "$A"))
(vX (Variable "$X"))
(akx (Evaluation kp (List vA vX))))
(BindLink
(VariableSet
(TypedVariable
vA
(Type "ConceptNode"))
(TypedVariable
vX
(Type "ConceptNode")))
(Present
(Inheritance
vA
(Concept "person"))
(Inheritance
vX
(Concept "pet")))
akx)))
(define keep-pet-rule-name
(DefinedSchemaNode "keep-pet-rule"))
(Define keep-pet-rule-name
keep-pet-rule)
(define Einstein-rbs (ConceptNode "Einstein-rbs"))
(ure-add-rule Einstein-rbs keep-pet-rule-name)
(ure-set-maximum-iterations Einstein-rbs 30)
(define target
(Evaluation
(Predicate "keep-pet")
(List
(Variable "$who")
(Concept "cat"))))
(define vd
(TypedVariable (VariableNode "$who") (TypeNode "ConceptNode")))