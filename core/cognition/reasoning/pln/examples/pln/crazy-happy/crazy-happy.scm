(use-modules (opencog exec))
(use-modules (opencog query))
(add-to-load-path "../../../opencog/nlp/chatbot-psi")
(load-from-path "chatbot.scm")
(add-to-load-path "../../../opencog/pln/rules")
(load-from-path "wip/implication-direct-evaluation.scm")
(define (get-parse-nodes)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$P")
                           (Type "ParseNode"))
                         (Variable "$P"))))
(define (get-set-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$S")
                           (Type "SetLink"))
                         (Variable "$S"))))
(define (get-wordinstance-nodes)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$W")
                           (Type "WordInstanceNode"))
                         (Variable "$W"))))
(define (get-wordinstance-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$W")
                           (Type "WordInstanceLink"))
                         (Variable "$W"))))
(define (get-wordsequence-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$W")
                           (Type "WordSequenceLink"))
                         (Variable "$W"))))
(define (get-lemma-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$L")
                           (Type "LemmaLink"))
                         (Variable "$L"))))
(define (get-reference-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$R")
                           (Type "ReferenceLink"))
                         (Variable "$R"))))
(define (get-interpretation-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$I")
                           (Type "InterpretationLink"))
                         (Variable "$I"))))
(define (get-execution-links)
  (cog-satisfying-set (Get
                         (TypedVariable
                           (Variable "$E")
                           (Type "ExecutionLink"))
                         (Variable "$E"))))
(chat "Ben is happy")
(chat "Ben is crazy")
(chat "Eddie is happy")
(chat "Eddie is crazy")
(define unary-predicate-speech-act-l2s-vardecl
   (VariableList
      (TypedVariable
         (Variable "$element-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$element")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$predicate-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$predicate")
         (Type "PredicateNode"))))
(define unary-predicate-speech-act-l2s-pattern
   (And
      (Inheritance
         (Variable "$element-instance")
         (Variable "$element"))
      (Implication
         (Variable "$predicate-instance")
         (Variable "$predicate"))
      (Evaluation
         (Variable "$predicate-instance")
         (List
            (Variable "$element-instance")))))
(define unary-predicate-speech-act-l2s-rewrite
   (Evaluation (stv 1 0.1)
      (Variable "$predicate")
      (Variable "$element")))
(define unary-predicate-speech-act-l2s-rule
   (Bind
      unary-predicate-speech-act-l2s-vardecl
      unary-predicate-speech-act-l2s-pattern
      unary-predicate-speech-act-l2s-rewrite))
(define inheritance-to-evaluation-s2l-vardecl
   (VariableList
      (TypedVariable
         (Variable "$P")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$Q")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$P-element-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$P-element")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$P-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$P")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$Q-element-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$Q-element")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$Q-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$Q")
         (Type "PredicateNode"))))
(define inheritance-to-evaluation-s2l-pattern
   (And
      (Implication
         (Variable "$P")
         (Variable "$Q"))
      (Inheritance
         (Variable "$P-element-instance")
         (Variable "$P-element"))
      (Implication
         (Variable "$P-instance")
         (Variable "$P"))
      (Evaluation
         (Variable "$P-instance")
         (List
            (Variable "$P-element-instance")))
      (Inheritance
         (Variable "$Q-element-instance")
         (Variable "$Q-element"))
      (Implication
         (Variable "$Q-instance")
         (Variable "$Q"))
      (Evaluation
         (Variable "$Q-instance")
         (List
            (Variable "$Q-element-instance")))))
(define inheritance-to-evaluation-s2l-rewrite
   (ExecutionOutput
      (GroundedSchema "scm: inheritance-to-evaluation-s2l-formula")
      (List
         (Variable "$P")
         (Variable "$Q"))))
(define (inheritance-to-evaluation-s2l-formula P Q)
   (let ((P-name (cog-name P)))
       (Word "people")
       (Set
          (Evaluation
             Q
             (List
                (Concept "people")))
          (Inheritance
             (Concept "people")
             (Concept P-name)))))
(define inheritance-to-evaluation-s2l-rule
   (Bind
      inheritance-to-evaluation-s2l-vardecl
      inheritance-to-evaluation-s2l-pattern
      inheritance-to-evaluation-s2l-rewrite))
(cog-execute! unary-predicate-speech-act-l2s-rule)
(cog-execute! implication-direct-evaluation-rule)
(cog-execute! inheritance-to-evaluation-s2l-rule)
(chat "small cats are cute")
(Word "happy")
(Word "people")
(Word "crazy")
(sureal
   (SetLink
      (EvaluationLink
         (PredicateNode "happy" (stv 0.2857143 0.0024937657))
         (ListLink
            (ConceptNode "people")
         )
      )
      (InheritanceLink
         (ConceptNode "people")
         (ConceptNode "crazy")
      )
   )
)