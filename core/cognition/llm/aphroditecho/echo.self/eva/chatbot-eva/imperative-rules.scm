(use-modules (opencog nlp relex2logic))
(define current-sentence (AnchorNode "*-eva-current-sent-*"))
(StateLink current-sentence (SentenceNode "foobar"))
(define current-imperative (AnchorNode "*-imperative-*"))
(StateLink current-imperative (WordNode "foobar"))
(define (print-msg node) (display (cog-name node)) (newline) (stv 1 1))
(define-public (show-arg node) (display node) node)
(define look-rule-1
(BindLink
(VariableList
(var-decl "$sent" "SentenceNode")
(var-decl "$parse" "ParseNode")
(var-decl "$verb-inst" "WordInstanceNode")
(var-decl "$direct-inst" "WordInstanceNode")
(var-decl "$direction" "WordNode")
)
(AndLink
(StateLink current-sentence (Variable "$sent"))
(parse-of-sent   "$parse" "$sent")
(word-in-parse   "$verb-inst" "$parse")
(LemmaLink (VariableNode "$verb-inst") (WordNode "look"))
(word-pos "$verb-inst" "verb")
(verb-tense "$verb-inst" "imperative")
(ChoiceLink
(lg-link "MVa" "$verb-inst" "$direct-inst")
(lg-link "Pa" "$verb-inst" "$direct-inst"))
(word-lemma "$direct-inst" "$direction")
)
(State current-imperative
(ActionLink
(WordNode "look")
(ListLink (Variable "$direction"))
))
)
)
(define look-rule-2
(BindLink
(VariableList
(var-decl "$sent" "SentenceNode")
(var-decl "$parse" "ParseNode")
(var-decl "$verb-inst" "WordInstanceNode")
(var-decl "$prep-inst" "WordInstanceNode")
(var-decl "$direct-inst" "WordInstanceNode")
(var-decl "$direction" "WordNode")
)
(AndLink
(StateLink current-sentence (Variable "$sent"))
(parse-of-sent   "$parse" "$sent")
(word-in-parse   "$verb-inst" "$parse")
(LemmaLink (VariableNode "$verb-inst") (WordNode "look"))
(word-pos "$verb-inst" "verb")
(verb-tense "$verb-inst" "imperative")
(lg-link "MVp" "$verb-inst" "$prep-inst")
(ChoiceLink
(lg-link "Js" "$prep-inst" "$direct-inst")
(lg-link "Ju" "$prep-inst" "$direct-inst"))
(word-lemma "$direct-inst" "$direction")
)
(State current-imperative
(ActionLink
(WordNode "look")
(ListLink (Variable "$direction"))
))
)
)
(define (imperative-object-rule-template VERB-LIST DECL LINKS)
(BindLink
(VariableList
(var-decl "$sent" "SentenceNode")
(var-decl "$parse" "ParseNode")
(var-decl "$verb-inst" "WordInstanceNode")
(var-decl "$verb" "WordNode")
DECL
(var-decl "$obj-inst" "WordInstanceNode")
(var-decl "$object" "WordNode")
)
(AndLink
(StateLink current-sentence (Variable "$sent"))
(parse-of-sent   "$parse" "$sent")
(word-in-parse   "$verb-inst" "$parse")
(word-lemma "$verb-inst" "$verb")
VERB-LIST
(word-pos "$verb-inst" "verb")
(verb-tense "$verb-inst" "imperative")
LINKS
(word-lemma "$obj-inst" "$object")
)
(State current-imperative
(ActionLink
(Variable "$verb")
(ListLink (Variable "$object"))
))
)
)
(define look-rule-1
(imperative-object-rule-template
(OrLink
(Equal (Variable "$verb") (WordNode "face"))
(Equal (Variable "$verb") (WordNode "look"))
(Equal (Variable "$verb") (WordNode "turn"))
)
'()
(ChoiceLink
(lg-link "MVa" "$verb-inst" "$obj-inst")
(lg-link "MVp" "$verb-inst" "$obj-inst")
(lg-link "Pa" "$verb-inst" "$obj-inst")
(lg-link "Ox" "$verb-inst" "$obj-inst"))
))
(define look-rule-2
(imperative-object-rule-template
(OrLink
(Equal (Variable "$verb") (WordNode "face"))
(Equal (Variable "$verb") (WordNode "look"))
(Equal (Variable "$verb") (WordNode "turn"))
)
(var-decl "$prep-inst" "WordInstanceNode")
(list
(lg-link "MVp" "$verb-inst" "$prep-inst")
(ChoiceLink
(lg-link "Js" "$prep-inst" "$obj-inst")
(lg-link "Ju" "$prep-inst" "$obj-inst")
(lg-link "J" "$prep-inst" "$obj-inst"))
)
))
(define (imperative-action-template ACTION-VERB VERB-LIST)
(BindLink
(VariableList
(var-decl "$sent" "SentenceNode")
(var-decl "$parse" "ParseNode")
(var-decl "$verb-inst" "WordInstanceNode")
(var-decl "$verb" "WordNode")
)
(AndLink
(StateLink current-sentence (Variable "$sent"))
(parse-of-sent   "$parse" "$sent")
(word-in-parse   "$verb-inst" "$parse")
(word-pos "$verb-inst" "verb")
(verb-tense "$verb-inst" "imperative")
(word-lemma "$verb-inst" "$verb")
VERB-LIST
)
(State current-imperative
(ActionLink
ACTION-VERB
(ListLink (Variable "$verb"))
))
)
)
(define single-word-express-rule
(imperative-action-template
(WordNode "express-action")
(OrLink
(Equal (Variable "$verb") (WordNode "frown"))
(Equal (Variable "$verb") (WordNode "recoil"))
(Equal (Variable "$verb") (WordNode "smile"))
)))
(define single-word-gesture-rule
(imperative-action-template
(WordNode "gesture-action")
(OrLink
(Equal (Variable "$verb") (WordNode "blink"))
(Equal (Variable "$verb") (WordNode "nod"))
(Equal (Variable "$verb") (WordNode "shake"))
(Equal (Variable "$verb") (WordNode "yawn"))
)))
(define show-rule-1
(imperative-object-rule-template
(OrLink
(Equal (Variable "$verb") (WordNode "act"))
(Equal (Variable "$verb") (WordNode "be"))
(Equal (Variable "$verb") (WordNode "look"))
(Equal (Variable "$verb") (WordNode "play"))
)
'()
(list
(ChoiceLink
(lg-link "MVa" "$verb-inst" "$obj-inst")
(lg-link "MVp" "$verb-inst" "$obj-inst")
(lg-link "Pa" "$verb-inst" "$obj-inst"))
(word-pos "$obj-inst" "adjective")
)
))
(define show-rule-2
(imperative-object-rule-template
(OrLink
(Equal (Variable "$verb") (WordNode "dramatize"))
(Equal (Variable "$verb") (WordNode "emote"))
(Equal (Variable "$verb") (WordNode "enact"))
(Equal (Variable "$verb") (WordNode "express"))
(Equal (Variable "$verb") (WordNode "feign"))
(Equal (Variable "$verb") (WordNode "impersonate"))
(Equal (Variable "$verb") (WordNode "mime"))
(Equal (Variable "$verb") (WordNode "mimic"))
(Equal (Variable "$verb") (WordNode "portray"))
(Equal (Variable "$verb") (WordNode "pretend"))
(Equal (Variable "$verb") (WordNode "show"))
)
'()
(lg-link "Ou" "$verb-inst" "$obj-inst")
))
(define (demo-rule-template VERB-LIST LINKS)
(BindLink
(VariableList
(var-decl "$sent" "SentenceNode")
(var-decl "$parse" "ParseNode")
(var-decl "$verb-inst" "WordInstanceNode")
(var-decl "$verb" "WordNode")
(var-decl "$obj-inst" "WordInstanceNode")
(var-decl "$object" "WordNode")
)
(AndLink
(StateLink current-sentence (Variable "$sent"))
(parse-of-sent   "$parse" "$sent")
(word-in-parse "$verb-inst" "$parse")
(word-in-parse "$obj-inst" "$parse")
(word-lemma "$verb-inst" "$verb")
(word-lemma "$obj-inst" "$object")
(word-pos "$verb-inst" "verb")
VERB-LIST
LINKS
)
(State current-imperative
(ActionLink
(Variable "$verb")
(ListLink (Variable "$object"))
))
)
)
(define demo-rule
(demo-rule-template
(Equal (Variable "$verb") (Word "show"))
(lg-link "Os" "$verb-inst" "$obj-inst")
)
)