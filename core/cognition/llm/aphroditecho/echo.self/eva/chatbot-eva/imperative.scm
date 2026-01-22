(use-modules (opencog) (opencog exec))
(define current-action (AnchorNode "*-action-*"))
(StateLink current-action (WordNode "foobar"))
(define action-rule-ao
(BindLink
(VariableList
(TypedVariable
(Variable "$action")
(Signature
(EvaluationLink
(Type "DefinedPredicateNode")
(TypeChoice
(Type "ListLink") (Type "SetLink")
(Type "ConceptNode")))))
)
(AndLink
(ListLink current-action (Variable "$action"))
)
(Variable "$action")
))
(define-public (imperative-process imp)
"
Process imperative IMP, which should be a SentenceNode.
"
(define do-dbg-prt #t)
(StateLink current-sentence imp)
(cog-execute! look-rule-1)
(cog-execute! look-rule-2)
(cog-execute! single-word-express-rule)
(cog-execute! single-word-gesture-rule)
(cog-execute! show-rule-1)
(cog-execute! show-rule-2)
(cog-execute! demo-rule)
(if do-dbg-prt (begin
(display "The current-imperative is\n")
(display (cog-execute! (Get (State current-imperative (Variable "$x")))))
))
(cog-execute! obj-semantics-rule-1-ao)
(if do-dbg-prt (begin
(display "The current-action is\n")
(display (cog-execute! (Get (List current-action (Variable "$x")))))
))
(cog-execute! obj-semantic-model-rule-1)
(cog-execute! obj-semantic-model-rule-2)
(let* ((act-do-do (cog-execute! action-rule-ao))
(action-list (cog-outgoing-set act-do-do))
)
(if do-dbg-prt (begin
(display "The set of actions to be performed are:\n")
(display act-do-do) (newline)
))
(catch #t
(lambda ()
(for-each cog-evaluate! action-list))
(lambda (key . args)
(display "Exception: ") (display key) (newline)
(display args) (newline)
(display "Bad eval: ") (display act-do-do) (newline)))
(for-each (lambda (x)
(cog-extract-recursive! (ListLink current-action x)))
action-list)
(if (null? action-list)
(begin
(State (Anchor "Chatbot: ChatbotEvaAction")
(Concept "Chatbot: NoResult"))
(display "I don't know how to do that.\n")))
(State (Anchor "Chatbot: ChatbotEva")
(Concept "Chatbot: ProcessFinished"))
)
(StateLink current-imperative (WordNode "foobar"))
(StateLink current-action (WordNode "foobar"))
*unspecified*
)