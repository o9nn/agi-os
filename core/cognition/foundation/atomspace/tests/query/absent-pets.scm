(use-modules (opencog) (opencog exec))
(Inheritance (Concept "American") (Concept "person"))
(Inheritance (Concept "German") (Concept "person"))
(Inheritance (Concept "cat") (Concept "pet"))
(Inheritance (Concept "dog") (Concept "pet"))
(Evaluation (Predicate "keep-pet")
   (List (Concept "German") (Concept "dog")))
(define pet-keepers
 (let ((kp (Predicate "keep-pet"))
       (vA (Variable "$A"))        
       (vX (Variable "$X")) )
  (BindLink
     (VariableList
        (TypedVariable vA (Type "ConceptNode"))
        (TypedVariable vX (Type "ConceptNode")) )
     (And
        (Inheritance vA (Concept "person"))
        (Inheritance vX (Concept "pet"))
        (Absent (Evaluation kp (List vA vX)))
     )     
     (List kp vA vX))))
(define expected-result
(SetLink
   (ListLink
      (PredicateNode "keep-pet")
      (ConceptNode "German")
      (ConceptNode "cat")
   )
   (ListLink
      (PredicateNode "keep-pet")
      (ConceptNode "American")
      (ConceptNode "cat")
   )
   (ListLink
      (PredicateNode "keep-pet")
      (ConceptNode "American")
      (ConceptNode "dog")
   )
))
*unspecified*