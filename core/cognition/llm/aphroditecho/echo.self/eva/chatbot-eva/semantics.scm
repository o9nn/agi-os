(use-modules (opencog nlp relex2logic))
(define current-action (AnchorNode "*-action-*"))
(define current-imperative (AnchorNode "*-imperative-*"))
(define (obj-semantics-template VERB-GND-DECL OBJ-GND-DECL ACTION)
(BindLink
(VariableList
(var-decl "$verb" "WordNode")
(var-decl "$object" "WordNode")
VERB-GND-DECL
OBJ-GND-DECL
(var-decl "$ground-verb-type" "ConceptNode")
(var-decl "$ground-obj-type" "ConceptNode")
(var-decl "$linkage" "PredicateNode")
)
(AndLink
(StateLink current-imperative
(ActionLink
(Variable "$verb")
(ListLink (Variable "$object"))))
(ReferenceLink (Variable "$verb") (Variable "$verb-ground"))
(ReferenceLink (Variable "$object") (Variable "$obj-ground"))
(InheritanceLink (Variable "$verb-ground")
(VariableNode "$ground-verb-type"))
(InheritanceLink (Variable "$obj-ground")
(VariableNode "$ground-obj-type"))
(EvaluationLink
(VariableNode "$linkage")
(ListLink
(VariableNode "$ground-verb-type")
(VariableNode "$ground-obj-type")))
)
ACTION
))
(define obj-semantics-rule-1-ao
(obj-semantics-template
(var-decl "$verb-ground" "DefinedPredicateNode")
(var-decl "$obj-ground"  "DefinedSchemaNode")
(ListLink current-action
(EvaluationLink
(Variable "$verb-ground")
(Variable "$obj-ground")))
))
(define obj-semantic-model-rule-1
(obj-semantics-template
(var-decl "$verb-ground" "AnchorNode")
(var-decl "$obj-ground"  "ConceptNode")
(StateLink
(Variable "$verb-ground")
(Variable "$obj-ground"))
))
(define obj-semantic-model-rule-2
(obj-semantics-template
(var-decl "$verb-ground" "AnchorNode")
(var-decl "$obj-ground"  "DefinedSchemaNode")
(StateLink
(Variable "$verb-ground")
(Variable "$obj-ground"))
))