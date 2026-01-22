(load "formulas.scm")
(define implication-implicant-conjunction-variables
  (VariableList
     (TypedVariable
        (Variable "$A")
        (TypeChoice
           (Type "PredicateNode")
           (Type "LambdaLink")))
     (TypedVariable
        (Variable "$B")
        (TypeChoice
           (Type "PredicateNode")
           (Type "LambdaLink")))
     (TypedVariable
        (Variable "$C")
        (TypeChoice
           (Type "PredicateNode")
           (Type "LambdaLink")))))
(define implication-implicant-conjunction-body
  (AndLink
     (ImplicationLink
        (VariableNode "$A")
        (VariableNode "$C"))
     (ImplicationLink
        (VariableNode "$B")
        (VariableNode "$C"))
     (NotLink (IdenticalLink (VariableNode "$A") (VariableNode "$B")))))
(define implication-implicant-conjunction-rewrite
  (ExecutionOutput
     (GroundedSchema "scm: implication-implicant-conjunction-formula")
     (List
        (Implication
           (And
              (Variable "$A")
              (Variable "$B"))
           (Variable "$C"))
        (VariableNode "$A")
        (VariableNode "$B")
        (VariableNode "$C")
        (ImplicationLink
           (VariableNode "$A")
           (VariableNode "$C"))
        (ImplicationLink
           (VariableNode "$B")
           (VariableNode "$C")))))
(define implication-implicant-conjunction-rule
  (Bind
     implication-implicant-conjunction-variables
     implication-implicant-conjunction-body
     implication-implicant-conjunction-rewrite))
(define (implication-implicant-conjunction-formula ABC A B C AC BC)
  (let* 
      ((sA (cog-mean A))
       (sB (cog-mean B))
       (sC (cog-mean C))
       (sAC (cog-mean AC))
       (sBC (cog-mean BC))
       (cA (cog-confidence A))
       (cB (cog-confidence B))
       (cC (cog-confidence C))
       (cAC (cog-confidence AC))
       (cBC (cog-confidence BC)))
    (if
       (and
           (conditional-probability-consistency sA sC sAC)
           (conditional-probability-consistency sB sC sBC))
       (let ((AnB (gar ABC)))
         (cog-set-tv! AnB (stv (* sA sB) (min cA cB)))
         (cog-set-tv!
            ABC
            (stv (implication-implicant-conjunction-strength sC sAC sBC)
                 (implication-implicant-conjunction-confidence cAC cBC)))))))
(define (implication-implicant-conjunction-strength sC sAC sBC)
  (min (/ (* sAC sBC) sC) 1))
(define (implication-implicant-conjunction-confidence cAC cBC)
  (min cAC cBC))
(define implication-implicant-conjunction-rule-name
  (DefinedSchemaNode "implication-implicant-conjunction-rule"))
(DefineLink implication-implicant-conjunction-rule-name
  implication-implicant-conjunction-rule)