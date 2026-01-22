(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))
(define back-predictive-implication-scope-conditional-conjunction-introduction-rule
  (let* ((V (Variable "$V"))
     (T (Variable "$T"))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (R (Variable "$R"))
     (NaturalT (TypeInh 'NaturalLink))
     (VardeclT (TypeChoice
        (TypeInh 'VariableNode)
        (Type 'VariableSet)
        (Type 'VariableList)
        (Type 'TypedVariableLink)))
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T)
              (Unquote P)
              (Unquote Q))))
     (P↝R (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T)
              (Unquote P)
              (Unquote R))))
     (Q∧R (And Q R))
     (P↝Q∧R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T)
                (Unquote P)
                (Unquote Q∧R)))))
  (Bind
    (VariableSet
      (TypedVariable V VardeclT)
      (TypedVariable T NaturalT)
      P
      Q
      R)
    (And
      (Present P↝Q P↝R)
      (Not (Identical Q R))
      (EvaluationLink
        (GroundedPredicate "scm: check_preconditions")
        (List
          Q
          R)
      )
    )
    (ExecutionOutput
      (GroundedSchema "scm: back-predictive-implication-scope-conditional-conjunction-introduction")
      (List
        P↝Q∧R
        (Set
          P↝Q
          P↝R))))))
(define (check_preconditions Q R)
  (define (andlink? atom)
    (equal? (cog-type atom) 'AndLink))
  (if (or (and (andlink? Q) (member R (cog-outgoing-set Q)))
          (and (andlink? R) (member Q (cog-outgoing-set R))))
    (stv 0 1)
    (stv 1 1)))
(define (back-predictive-implication-scope-conditional-conjunction-introduction conclusion . premises)
  (cog-logger-fine "(back-predictive-implication-scope-conditional-conjunction-introduction conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 1)
      (let* ((premises (car premises))
        (P↝Q (gar premises))
        (P↝R (gdr premises))
        (sP↝Q (cog-mean P↝Q))
        (cP↝Q (cog-confidence P↝Q))
        (sP↝R (cog-mean P↝R))
        (cP↝R (cog-confidence P↝R))
        (sPQR (* sP↝Q sP↝R))
        (cPQR (min cP↝Q cP↝R))
        (tv (stv sPQR cPQR)))
       (if (< 0 cPQR)
           (cog-merge-hi-conf-tv! conclusion tv)))))
(define back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-conditional-conjunction-introduction-rule"))
(DefineLink back-predictive-implication-scope-conditional-conjunction-introduction-rule-name
  back-predictive-implication-scope-conditional-conjunction-introduction-rule)