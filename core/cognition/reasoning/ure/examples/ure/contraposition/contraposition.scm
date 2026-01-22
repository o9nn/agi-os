(Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person"))
(Inheritance (stv 1.0 1.0)
   (Concept "German")
   (Concept "person"))
(Inheritance (stv 1.0 1.0)
   (Concept "cat")
   (Concept "pet"))
(Inheritance (stv 1.0 1.0)
   (Concept "dog")
   (Concept "pet"))
(Evaluation (stv 0.0 1.0)
   (Predicate "keep-pet")
   (List
   	(Concept "German")
      (Concept "dog")))
(define (evaluation-absent predicate A B)
    (bool->tv (null? (cog-link "EvaluationLink" predicate (List A B))) )
)
(define keep-different-pet-rule
 (let* ((kp (Predicate "keep-pet"))
        (vA (Variable "$A"))
        (vB (Variable "$B"))
        (vX (Variable "$X"))
        (vY (Variable "$Y"))
        (akx (Evaluation kp (List vA vX)))
        (bky (Evaluation kp (List vB vY))))
  (BindLink
   (VariableSet
     (TypedVariable
        vA
        (Type "ConceptNode"))
     (TypedVariable
        vB
        (Type "ConceptNode"))
     (TypedVariable
        vX
        (Type "ConceptNode"))
     (TypedVariable
        vY
        (Type "ConceptNode")))
   (And
     (Present
       (Inheritance
         vA
         (Concept "person"))
       (Inheritance
         vB
         (Concept "person"))
       (Inheritance
         vX
         (Concept "pet"))
       (Inheritance
         vY
         (Concept "pet")))
     (NotLink
       (EqualLink
         vA
         vB))
     (NotLink
       (EqualLink
         vX
         vY))
     (Or
        (EvaluationLink
          (GroundedPredicateNode "scm: evaluation-absent")
          (ListLink kp vA vX))
        (EvaluationLink
          (GroundedPredicateNode "scm: absolutely-true")
          (ListLink akx)))
     (Or
        (EvaluationLink
	   (GroundedPredicateNode "scm: evaluation-absent")
	   (ListLink kp vB vY))
        (EvaluationLink
	   (GroundedPredicateNode "scm: absolutely-true")
	   (ListLink bky))))
   (ExecutionOutputLink
     (GroundedSchemaNode "scm: keep-different-pet")
     (ListLink akx bky)))))
(define (keep-different-pet akx bky)
    (cog-set-tv! akx (stv 1 1))
    (cog-set-tv! bky (stv 1 1)))
(define keep-different-pet-rule-name
  (DefinedSchemaNode "keep-different-pet-rule"))
(Define keep-different-pet-rule-name
        keep-different-pet-rule)
(define Einstein-rbs (ConceptNode "Einstein-rbs"))
(MemberLink (stv 1 1)
   keep-different-pet-rule-name
   Einstein-rbs)
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   Einstein-rbs
   (NumberNode "30"))
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   Einstein-rbs)
(define target
   (Evaluation
      (Predicate "keep-pet")
      (List
	 (Variable "$who")
	 (Concept "cat"))))
(define vd
  (TypedVariable (VariableNode "$who") (TypeNode "ConceptNode")))
(define source
  (Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person")))