(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))
(define predictive-implication-scope-deduction-rule
  (let* ((V (Variable "$vardecl"))
	 (T1 (Variable "$lag-1"))
	 (T2 (Variable "$lag-2"))
	 (P (Variable "$P"))
	 (Q (Variable "$Q"))
	 (R (Variable "$R"))
	 (A (Variable "$A"))
	 (ExecutionT (Type 'ExecutionLink))
	 (NaturalT (Type 'NaturalLink))
	 (VariableT (Type 'VariableNode))
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
		    Q
		    R))
	 (PQ (PredictiveImplicationScope V T1 P Q))
	 (QA (And Q A))
	 (QAR (PredictiveImplicationScope V T2 QA R))
	 (present-clauses (Present PQ QAR))
	 (precondition-clauses (IsClosed PQ QAR))
	 (PA (AltSequentialAnd T1 P A))
	 (PAR (PredictiveImplicationScope V T2 PA R)))
    (Bind
      vardecl
      (And
        present-clauses
	precondition-clauses)
      (ExecutionOutput
        (GroundedSchema "scm: predictive-implication-scope-deduction")
	(List
	  PAR
	  PQ
	  QAR)))))
(define (predictive-implication-scope-deduction conclusion . premises)
  (ure-logger-fine "(predictive-implication-scope-deduction conclusion=~a . premises=~a)" conclusion premises)
    (cog-merge-hi-conf-tv! conclusion (stv 0.123 0.0000123))
)
(define predictive-implication-scope-deduction-rule-name
  (DefinedSchemaNode "predictive-implication-scope-deduction-rule"))
(DefineLink predictive-implication-scope-deduction-rule-name
  predictive-implication-scope-deduction-rule)