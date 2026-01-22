(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))
(define back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule
  (let* ((V (Variable "$vardecl"))
     (T1 (Variable "$lag-1"))
     (T2 (Variable "$lag-2"))
     (P (Variable "$P"))
     (Q (Glob "$Q"))
     (semi-open (Interval (Number 0) (Number -1)))
     (R (Variable "$R"))
     (A (Variable "$A"))
     (ExecutionT (Type 'ExecutionLink))
     (NaturalT (TypeInh 'NaturalLink))
     (VariableT (TypeInh 'VariableNode))
     (VariableSetT (Type 'VariableSet))
     (VariableListT (Type 'VariableList))
     (TypedVariableT (Type 'TypedVariableLink))
     (VardeclT (TypeChoice
             VariableT
             VariableSetT
             VariableListT
             TypedVariableT))
     (vardecl (VariableSet
            (TypedVariable V VardeclT)
            (TypedVariable T1 NaturalT)
            (TypedVariable T2 NaturalT)
            (TypedVariable A ExecutionT)
            P
            (TypedVariable Q semi-open)
            R))
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T1)
              (Unquote P)
              (Unquote (And Q)))))
     (Q∧A (And Q A))
     (Q∧A↝R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T2)
                (Unquote Q∧A)
                (Unquote R))))
     (present-clauses (Present P↝Q Q∧A↝R))
     (precondition-clauses (IsClosed P↝Q Q∧A↝R))
     (P≺A (BackSequentialAnd T1 P A))
     (P≺A↝R (Quote
               (BackPredictiveImplicationScope
                 (Unquote V)
                 (Unquote T2)
                 (Unquote P≺A)
                 (Unquote R)))))
    (Bind
      vardecl
      (And
        present-clauses
        precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-deduction-cogscm")
        (List
            P≺A↝R
            P≺A
            Q∧A
            R
            P↝Q
            Q∧A↝R)))))
(define back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule
  (let* ((V (Variable "$vardecl"))
     (T1 (Variable "$lag-1"))
     (T2 (Variable "$lag-2"))
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     (R (Variable "$R"))
     (A (Variable "$A"))
     (ExecutionT (Type 'ExecutionLink))
     (NaturalT (TypeInh 'NaturalLink))
     (VariableT (TypeInh 'VariableNode))
     (VariableSetT (Type 'VariableSet))
     (VariableListT (Type 'VariableList))
     (TypedVariableT (Type 'TypedVariableLink))
     (EvaluationT (Type 'EvaluationLink))
     (VardeclT (TypeChoice
             VariableT
             VariableSetT
             VariableListT
             TypedVariableT))
     (vardecl (VariableSet
            (TypedVariable V VardeclT)
            (TypedVariable T1 NaturalT)
            (TypedVariable T2 NaturalT)
            (TypedVariable A ExecutionT)
            P
            (TypedVariable Q EvaluationT)
            R))
     (P↝Q (Quote
            (BackPredictiveImplicationScope
              (Unquote V)
              (Unquote T1)
              (Unquote P)
              (Unquote Q))))
     (Q∧A (And Q A))
     (Q∧A↝R (Quote
              (BackPredictiveImplicationScope
                (Unquote V)
                (Unquote T2)
                (Unquote Q∧A)
                (Unquote R))))
     (present-clauses (Present P↝Q Q∧A↝R))
     (precondition-clauses (IsClosed P↝Q Q∧A↝R))
     (P≺A (BackSequentialAnd T1 P A))
     (P≺A↝R (Quote
               (BackPredictiveImplicationScope
                 (Unquote V)
                 (Unquote T2)
                 (Unquote P≺A)
                 (Unquote R)))))
    (Bind
      vardecl
      (And
        present-clauses
        precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-deduction-cogscm")
        (List
            P≺A↝R
            P≺A
            Q∧A
            R
            P↝Q
            Q∧A↝R)))))
(define (back-predictive-implication-scope-deduction-cogscm conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-scope-deduction-cogscm conclusion=~a . premises=~a)" conclusion premises)
  (if (= (length premises) 5)
     (let* ((P≺A↝R conclusion)
            (P≺A (list-ref premises 0))
            (Q∧A (list-ref premises 1))
            (R (list-ref premises 2))
            (P↝Q (list-ref premises 3))
            (Q∧A↝R (list-ref premises 4))
            (A P≺A)
            (B Q∧A)
            (C R)
            (AB P↝Q)
            (BC Q∧A↝R)
            (AC P≺A↝R)
            (A-tv (cog-tv A))
            (B-tv (cog-tv B))
            (C-tv (cog-tv C))
            (AB-tv (cog-tv AB))
            (BC-tv (cog-tv BC))
            (AC-tv (deduction-formula A-tv B-tv C-tv AB-tv BC-tv)))
        (if (< 0 (cog-tv-confidence AC-tv))
            (cog-merge-hi-conf-tv! AC AC-tv)))))
(define (limit x l u)
  (max l (min u x)))
(define (deduction-formula A-tv B-tv C-tv AB-tv BC-tv)
  (define sA (cog-tv-mean A-tv))
  (define cA (cog-tv-confidence A-tv))
  (define sB (cog-tv-mean B-tv))
  (define cB (cog-tv-confidence B-tv))
  (define sC (cog-tv-mean C-tv))
  (define cC (cog-tv-confidence C-tv))
  (define sAB (cog-tv-mean AB-tv))
  (define cAB (cog-tv-confidence AB-tv))
  (define sBC (cog-tv-mean BC-tv))
  (define cBC (cog-tv-confidence BC-tv))
  (if
     (and
      (or (= 0 cA) (conditional-probability-consistency sA sB sAB))
      (conditional-probability-consistency sB sC sBC))
     (stv (naive-deduction-strength-formula sA sB sC sAB sBC)
          (naive-deduction-confidence-formula cAB cBC))
     (stv 1 0)))
(define (naive-deduction-strength-formula sA sB sC sAB sBC)
  (+ (* sAB sBC) (/ (* (- 1 sAB) (- sC (* sB sBC))) (- 1 sB))))
(define (naive-deduction-confidence-formula cAB cBC)
  (define α 0.9)
  (* (min cAB cBC) α))
(define (smallest-intersection-probability sA sB)
  (limit (/ (+ sA sB -1) sA) 0 1))
(define (largest-intersection-probability sA sB)
  (limit (/ sB sA) 0 1))
(define (conditional-probability-consistency sA sB sAB)
  (and (< 0 sA)
       (<= (smallest-intersection-probability sA sB) sAB)
       (<= sAB (largest-intersection-probability sA sB))))
(define back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule"))
(DefineLink back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule-name
  back-predictive-implication-scope-deduction-cogscm-Q-conjunction-rule)
(define back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule"))
(DefineLink back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule-name
  back-predictive-implication-scope-deduction-cogscm-Q-evaluation-rule)