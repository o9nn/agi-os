(add-to-load-path "../../../opencog/nlp/chatbot-psi")
(load-from-path "chatbot.scm")
(use-modules (opencog query))
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
(chat "Robert lives in China")
(chat "Robert likes chicken feet")
(chat "Jane lives in France")
(chat "Jane likes pizza")
(chat "Joe lives in China")
(chat "Joe likes chicken feet")
(chat "Jim lives in China")
(chat "What does Jim like?")
   (SetLink
      (InheritanceLink
         (ConceptNode "in@b8dbff0f-ffe4-4775-9905-f9c87f29f63d")
         (ConceptNode "in" (stv 0.032258064 0.0012484394))
      )
      (ImplicationLink
         (PredicateNode "bob@04c3177d-bb4d-4a44-ae30-f4acbf1ea59f")
         (PredicateNode "bob" (stv 0.16666667 0.0012484394))
      )
      (InheritanceLink
         (SatisfyingSetLink
            (PredicateNode "bob@04c3177d-bb4d-4a44-ae30-f4acbf1ea59f")
         )
         (ConceptNode "in@b8dbff0f-ffe4-4775-9905-f9c87f29f63d")
      )
      (InheritanceLink
         (ConceptNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         (ConceptNode "China" (stv 0.032258064 0.0012484394))
      )
      (ImplicationLink
         (PredicateNode "in@b8dbff0f-ffe4-4775-9905-f9c87f29f63d")
         (PredicateNode "in" (stv 0.16666667 0.0012484394))
      )
      (EvaluationLink
         (PredicateNode "in@b8dbff0f-ffe4-4775-9905-f9c87f29f63d")
         (ListLink
            (PredicateNode "bob@04c3177d-bb4d-4a44-ae30-f4acbf1ea59f")
            (ConceptNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         )
      )
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (ConceptNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         )
      )
      (EvaluationLink
         (PredicateNode "in@b8dbff0f-ffe4-4775-9905-f9c87f29f63d")
         (ListLink
            (ConceptNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         )
      )
      (InheritanceLink
         (InterpretationNode "sentence@0c2ab58f-7150-4259-8cd2-52ddf260d7f1_parse_0_interpretation_$X")
         (DefinedLinguisticConceptNode "ImperativeSpeechAct")
      )
      (InheritanceLink
         (SpecificEntityNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         (DefinedLinguisticConceptNode "female" (stv 0.050000001 0.0012484394))
      )
      (InheritanceLink
         (SpecificEntityNode "China@b5d7ee9c-4cb1-4a70-b7ea-4dec2db35558")
         (ConceptNode "China" (stv 0.032258064 0.0012484394))
      )
      (InheritanceLink
         (PredicateNode "bob@04c3177d-bb4d-4a44-ae30-f4acbf1ea59f")
         (DefinedLinguisticConceptNode "imperative")
      )
   )
   (SetLink
      (InheritanceLink
         (ConceptNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
         (ConceptNode "Bob" (stv 0.025641026 0.0012484394))
      )
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (ConceptNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
         )
      )
      (ImplicationLink
         (PredicateNode "likes@dc51995f-6e69-4d8e-91a7-0d5992dba6c6")
         (PredicateNode "like" (stv 0.125 0.0012484394))
      )
      (InheritanceLink
         (ConceptNode "feet@68a4daa2-1f53-4c0f-8443-806099311b49")
         (ConceptNode "foot" (stv 0.025641026 0.0012484394))
      )
      (EvaluationLink
         (PredicateNode "likes@dc51995f-6e69-4d8e-91a7-0d5992dba6c6")
         (ListLink
            (ConceptNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
            (ConceptNode "feet@68a4daa2-1f53-4c0f-8443-806099311b49")
         )
      )
      (EvaluationLink
         (PredicateNode "likes@dc51995f-6e69-4d8e-91a7-0d5992dba6c6")
         (ListLink
            (ConceptNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
         )
      )
      (InheritanceLink
         (InterpretationNode "sentence@e943fc5b-cf61-4ed1-a172-ab09d545ae67_parse_0_interpretation_$X")
         (DefinedLinguisticConceptNode "DeclarativeSpeechAct")
      )
      (InheritanceLink
         (ConceptNode "chicken@43d36c82-1eb0-4e42-b694-68e21023b5fa")
         (ConceptNode "chicken" (stv 0.025641026 0.0012484394))
      )
      (InheritanceLink
         (ConceptNode "feet@68a4daa2-1f53-4c0f-8443-806099311b49")
         (ConceptNode "chicken@43d36c82-1eb0-4e42-b694-68e21023b5fa")
      )
      (InheritanceLink
         (SpecificEntityNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
         (DefinedLinguisticConceptNode "male")
      )
      (InheritanceLink
         (SpecificEntityNode "Bob@c0bce0fd-d30f-402e-8897-259fa248473d")
         (ConceptNode "Bob" (stv 0.025641026 0.0012484394))
      )
      (InheritanceLink
         (PredicateNode "likes@dc51995f-6e69-4d8e-91a7-0d5992dba6c6")
         (DefinedLinguisticConceptNode "present")
      )
   )
   (SetLink
      (InheritanceLink
         (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
         (ConceptNode "Jane" (stv 0.032258064 0.0012484394))
      )
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
         )
      )
      (ImplicationLink
         (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
         (PredicateNode "like" (stv 0.25 0.0012484394))
      )
      (InheritanceLink
         (ConceptNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
         (ConceptNode "pizza" (stv 0.032258064 0.0012484394))
      )
      (EvaluationLink
         (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
         (ListLink
            (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            (ConceptNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
         )
      )
      (EvaluationLink
         (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
         (ListLink
            (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
         )
      )
      (InheritanceLink
         (InterpretationNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0_interpretation_$X")
         (DefinedLinguisticConceptNode "DeclarativeSpeechAct")
      )
      (InheritanceLink
         (SpecificEntityNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
         (DefinedLinguisticConceptNode "female" (stv 0.055555556 0.0012484394))
      )
      (InheritanceLink
         (SpecificEntityNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
         (ConceptNode "Jane" (stv 0.032258064 0.0012484394))
      )
      (InheritanceLink
         (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
         (DefinedLinguisticConceptNode "present")
      )
   )
   (LemmaLink (stv 1 1)
      (WordInstanceNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
      (WordNode "Jane" (stv 0.034482758 0.0012484394))
   )
   (LemmaLink (stv 1 1)
      (WordInstanceNode "likes@be81f834-60f1-4d91-8784-492353112606")
      (WordNode "like")
   )
   (LemmaLink (stv 1 1)
      (WordInstanceNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
      (WordNode "pizza" (stv 0.034482758 0.0012484394))
   )
   (WordInstanceLink (stv 1 1)
      (WordInstanceNode "LEFT-WALL@sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0")
      (ParseNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0" (stv 1 0.98799998))
   )
   (WordInstanceLink (stv 1 1)
      (WordInstanceNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
      (ParseNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0" (stv 1 0.98799998))
   )
   (WordInstanceLink (stv 1 1)
      (WordInstanceNode "likes@be81f834-60f1-4d91-8784-492353112606")
      (ParseNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0" (stv 1 0.98799998))
   )
   (WordInstanceLink (stv 1 1)
      (WordInstanceNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
      (ParseNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0" (stv 1 0.98799998))
   )
   (WordSequenceLink (stv 1 1)
      (WordInstanceNode "LEFT-WALL@sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0")
      (NumberNode "67.000000")
   )
   (WordSequenceLink (stv 1 1)
      (WordInstanceNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
      (NumberNode "68.000000")
   )
   (WordSequenceLink (stv 1 1)
      (WordInstanceNode "likes@be81f834-60f1-4d91-8784-492353112606")
      (NumberNode "69.000000")
   )
   (WordSequenceLink (stv 1 1)
      (WordInstanceNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
      (NumberNode "70.000000")
   )
   (InterpretationLink
      (InterpretationNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0_interpretation_$X")
      (ParseNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0" (stv 1 0.98799998))
   )
   (ReferenceLink
      (VariableNode "$wall-inst")
      (WordNode "###LEFT-WALL###" (stv 0.034482758 0.0012484394))
   )
   (ReferenceLink
      (VariableNode "$wall-inst")
      (WordNode ",")
   )
   (ReferenceLink
      (VariableNode "$l")
      (VariableNode "$x")
   )
   (ReferenceLink (stv 1 1)
      (WordInstanceNode "LEFT-WALL@sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0")
      (WordNode "###LEFT-WALL###" (stv 0.034482758 0.0012484394))
   )
   (ReferenceLink (stv 1 1)
      (WordInstanceNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
      (WordNode "Jane" (stv 0.034482758 0.0012484394))
   )
   (ReferenceLink (stv 1 1)
      (WordInstanceNode "likes@be81f834-60f1-4d91-8784-492353112606")
      (WordNode "likes" (stv 0.034482758 0.0012484394))
   )
   (ReferenceLink (stv 1 1)
      (WordInstanceNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
      (WordNode "pizza" (stv 0.034482758 0.0012484394))
   )
   (ReferenceLink
      (LgLinkInstanceNode "Ss@fa647385-5321-40e3-8584-9e72241bc40c")
      (LinkGrammarRelationshipNode "Ss")
   )
   (ReferenceLink
      (LgLinkInstanceNode "Ou@00a316a2-3ec7-4fd1-87b5-ea405dee4f19")
      (LinkGrammarRelationshipNode "Ou")
   )
   (ReferenceLink
      (LgLinkInstanceNode "Wd@3c7099c0-add9-4632-aa30-e2c04601aeac")
      (LinkGrammarRelationshipNode "Wd")
   )
   (ReferenceLink
      (LgLinkInstanceNode "WV@2ef19b9e-b6a9-4589-b104-a7f8e313a2be")
      (LinkGrammarRelationshipNode "WV")
   )
   (ReferenceLink
      (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
      (WordInstanceNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
   )
   (ReferenceLink
      (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
      (WordInstanceNode "likes@be81f834-60f1-4d91-8784-492353112606")
   )
   (ReferenceLink
      (ConceptNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
      (WordInstanceNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
   )
   (ReferenceLink
      (InterpretationNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0_interpretation_$X")
      (SetLink
         (InheritanceLink
            (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            (ConceptNode "Jane" (stv 0.032258064 0.0012484394))
         )
         (EvaluationLink
            (DefinedLinguisticPredicateNode "definite")
            (ListLink
               (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            )
         )
         (ImplicationLink
            (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
            (PredicateNode "like" (stv 0.25 0.0012484394))
         )
         (InheritanceLink
            (ConceptNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
            (ConceptNode "pizza" (stv 0.032258064 0.0012484394))
         )
         (EvaluationLink
            (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
            (ListLink
               (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
               (ConceptNode "pizza@0d996d7f-2522-45f0-95fd-7a3351caca27")
            )
         )
         (EvaluationLink
            (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
            (ListLink
               (ConceptNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            )
         )
         (InheritanceLink
            (InterpretationNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551_parse_0_interpretation_$X")
            (DefinedLinguisticConceptNode "DeclarativeSpeechAct")
         )
         (InheritanceLink
            (SpecificEntityNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            (DefinedLinguisticConceptNode "female" (stv 0.055555556 0.0012484394))
         )
         (InheritanceLink
            (SpecificEntityNode "Jane@e0ffaf00-8086-4682-b7ea-f318f9968516")
            (ConceptNode "Jane" (stv 0.032258064 0.0012484394))
         )
         (InheritanceLink
            (PredicateNode "likes@be81f834-60f1-4d91-8784-492353112606")
            (DefinedLinguisticConceptNode "present")
         )
      )
   )
   (ReferenceLink
      (ListLink
         (WordNode "Jane" (stv 0.034482758 0.0012484394))
         (WordNode "likes" (stv 0.034482758 0.0012484394))
         (WordNode "pizza" (stv 0.034482758 0.0012484394))
      )
      (SentenceNode "sentence@a88796c5-be60-4036-beb1-39bf193c1551")
   )
(ImplicationScopeLink (stv 1 1)
   (VariableList
      (TypedVariable
         (Variable "$individual")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$individual-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "live-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$in-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$China-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$China-specific-entity-instance")
         (Type "SpecificEntityNode"))
      (TypedVariable
         (Variable "$interpretation-instance")
         (Type "InterpretationNode"))
      (TypedVariable
         (Variable "$like-instance")
         (Type "PredicateNode"))
      (TypedVariable
         (Variable "$chicken-instance")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$foot-instance")
         (Type "ConceptNode")))
   (Set
      (Inheritance
         (Variable "$individual-instance")
         (Variable "$individual"))
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (Variable "$individual-instance")))
      (ImplicationLink
         (Variable "live-instance")
         (PredicateNode "live")
      )
      (EvaluationLink
         (Variable "live-instance")
         (ListLink
            (Variable "individual-instance")))
      (InheritanceLink
         (Variable "live-instance")
         (DefinedLinguisticConceptNode "present"))
      (ImplicationLink
         (Variable "$in-instance")
         (PredicateNode "in"))
      (EvaluationLink
         (Variable "$in-instance")
         (ListLink
            (Variable "live-instance")
            (Variable "China-instance")))
      (EvaluationLink
         (Variable "$in-instance")
         (ListLink
            (Variable "China-instance")))
      (Inheritance
         (Variable "$China-instance")
         (Concept "China"))
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (Variable "$China-instance")))
      (InheritanceLink
         (Variable "$China-specific-entity-instance")
         (DefinedLinguisticConceptNode "female"))
      (InheritanceLink
         (Variable "China-specific-entity-instance")
         (ConceptNode "China"))
      (InheritanceLink
         (Variable "$interpretation-instance")
         (DefinedLinguisticConceptNode "DeclarativeSpeechAct")))
   (Set
      (InheritanceLink
         (Variable "$individual-instance")
         (Variable "$individual"))
      (EvaluationLink
         (DefinedLinguisticPredicateNode "definite")
         (ListLink
            (Variable "$individual-instance")))
      (ImplicationLink
         (Variable "$like-instance")
         (PredicateNode "like"))
      (EvaluationLink
         (Variable "$like-instance")
         (ListLink
            (Variable "$individual-instance")
            (Variable "$foot-instance")))
      (EvaluationLink
         (Variable "$like-instance")
         (ListLink
            (ConceptNode "$individual-instance")))
      (InheritanceLink
         (Variable "$like-instance")
         (DefinedLinguisticConceptNode "present"))
      (InheritanceLink
         (Variable "$foot-instance")
         (ConceptNode "foot"))
      (InheritanceLink
         (Variable "$chicken-instance")
         (ConceptNode "chicken"))
      (InheritanceLink
         (ConceptNode "$foot-instance")
         (ConceptNode "chicken-instance"))))