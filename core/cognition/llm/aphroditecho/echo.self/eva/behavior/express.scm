(DefineLink
(DefinedSchema "Pick random expression")
(LambdaLink
(Variable "$emo")
(RandomChoice
(GetLink
(VariableList
(TypedVariable (Variable "$prob") (Type "NumberNode"))
(TypedVariable (Variable "$expr") (Type "ConceptNode")))
(AndLink
(Evaluation
(Predicate "Emotion-expression")
(ListLink (Variable "$emo") (Variable "$expr")))
(State
(ListLink
(Variable "$emo")
(Variable "$expr")
(Schema "probability"))
(Variable "$prob"))
)))))
(DefineLink
(DefinedSchema "Pick random gesture")
(LambdaLink
(Variable "$emo")
(RandomChoice
(GetLink
(VariableList
(TypedVariable (Variable "$prob") (Type "NumberNode"))
(TypedVariable (Variable "$expr") (Type "ConceptNode")))
(AndLink
(Evaluation
(Predicate "Emotion-gesture")
(ListLink (Variable "$emo") (Variable "$expr")))
(State
(ListLink
(Variable "$emo")
(Variable "$expr")
(Schema "gest probability"))
(Variable "$prob"))
)))))
(define (pick-value-in-range min-name max-name)
(LambdaLink
(VariableList (VariableNode "$emo") (VariableNode "$expr"))
(RandomNumberLink
(GetLink
(TypedVariable (Variable "$int-min") (Type "NumberNode"))
(StateLink (ListLink
(VariableNode "$emo") (VariableNode "$expr")
(SchemaNode min-name)) (VariableNode "$int-min")))
(GetLink
(TypedVariable (Variable "$int-max") (Type "NumberNode"))
(StateLink (ListLink
(VariableNode "$emo") (VariableNode "$expr")
(SchemaNode max-name)) (VariableNode "$int-max")))
)))
(DefineLink
(DefinedSchemaNode "get random intensity")
(pick-value-in-range "intensity-min" "intensity-max"))
(DefineLink
(DefinedSchemaNode "get random gest intensity")
(pick-value-in-range "gest intensity-min" "gest intensity-max"))
(DefineLink
(DefinedSchemaNode "get random duration")
(pick-value-in-range "duration-min" "duration-max"))
(DefineLink
(DefinedSchemaNode "get random repeat")
(pick-value-in-range "repeat-min" "repeat-max"))
(DefineLink
(DefinedSchemaNode "get random speed")
(pick-value-in-range "speed-min" "speed-max"))
(DefineLink
(DefinedPredicateNode "Show class expression")
(LambdaLink
(VariableList (VariableNode "$emo") (VariableNode "$expr"))
(PutLink (DefinedPredicate "Show facial expression")
(ListLink
(VariableNode "$expr")
(PutLink
(DefinedSchemaNode "get random duration")
(ListLink (VariableNode "$emo") (VariableNode "$expr")))
(PutLink
(DefinedSchemaNode "get random intensity")
(ListLink (VariableNode "$emo") (VariableNode "$expr")))
))
))
(DefineLink
(DefinedPredicateNode "Show class gesture")
(LambdaLink
(VariableList (VariableNode "$emo") (VariableNode "$gest"))
(PutLink (DefinedPredicate "Show gesture")
(ListLink
(VariableNode "$gest")
(PutLink
(DefinedSchemaNode "get random gest intensity")
(ListLink (VariableNode "$emo") (VariableNode "$gest")))
(PutLink
(DefinedSchemaNode "get random repeat")
(ListLink (VariableNode "$emo") (VariableNode "$gest")))
(PutLink
(DefinedSchemaNode "get random speed")
(ListLink (VariableNode "$emo") (VariableNode "$gest")))
))
))
(DefineLink
(DefinedPredicateNode "Show random expression")
(LambdaLink
(VariableNode "$emo")
(PutLink
(DefinedPredicateNode "Show class expression")
(ListLink
(VariableNode "$emo")
(PutLink
(DefinedSchemaNode "Pick random expression")
(VariableNode "$emo"))
))
))
(DefineLink
(DefinedPredicate "Show random gesture")
(LambdaLink
(Variable "$emo")
(Put
(DefinedPredicate "Show class gesture")
(ListLink
(Variable "$emo")
(Put
(DefinedSchema "Pick random gesture")
(Variable "$emo"))
))
))
(DefineLink
(DefinedPredicateNode "Show positive expression")
(PutLink (DefinedPredicateNode "Show random expression")
(ConceptNode "positive")))
(DefineLink
(DefinedPredicateNode "Show frustrated expression")
(PutLink (DefinedPredicateNode "Show random expression")
(ConceptNode "frustrated")))
(DefineLink
(DefinedPredicateNode "Pick random positive gesture")
(PutLink (DefinedPredicateNode "Show random gesture")
(ConceptNode "positive")))