(use-modules (opencog openpsi))
(load "faces.scm")
(define-public face-tracking-state (AnchorNode "Face Tracking State"))
(define-public face-tracking-on (ConceptNode "FaceTrackingOn"))
(define-public face-tracking-off (ConceptNode "FaceTrackingOff"))
(StateLink face-tracking-state face-tracking-on)
(define-public soma-state (AnchorNode "Soma State"))
(define-public soma-sleeping (ConceptNode "Sleeping"))
(define-public soma-awake (ConceptNode "Awake"))
(define-public soma-bored (ConceptNode "Bored"))
(StateLink soma-state soma-sleeping)
(DefineLink
	(DefinedPredicate "Is sleeping?")
	(Equal (SetLink soma-sleeping)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State soma-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "Is bored?")
	(Equal (SetLink soma-bored)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State soma-state (Variable "$x")))))
(define-public face-expression-state (AnchorNode "Facial Expression State"))
(define-public expression-neutral (ConceptNode "neutral"))
(StateLink face-expression-state expression-neutral)
(DefineLink
	(DefinedSchema "Get Facial Expression")
	(Get
		(TypedVariable (Variable "$x") (Type "ConceptNode"))
		(AndLink
			(State face-expression-state (Variable "$x"))
		))
)
(define-public eye-contact-state (AnchorNode "Eye Contact State"))
(define-public no-interaction (NumberNode 0.0))
(StateLink eye-contact-state no-interaction)
(define-public glance-state (AnchorNode "Glance State"))
(StateLink glance-state no-interaction)
(define-public request-eye-contact-state (AnchorNode "Request Interaction"))
(StateLink request-eye-contact-state no-interaction)
(define neutral-direction (ListLink (Number 1) (Number 0) (Number 0)))
(define-public interaction-state (AnchorNode "Interaction State"))
(StateLink interaction-state no-interaction)
(define-public prev-interaction-state (AnchorNode "Previous Interaction State"))
(StateLink prev-interaction-state no-interaction)
(define last-speaker (ConceptNode "last person who spoke"))
(define prev-speaker (ConceptNode "previous person who spoke"))
(DefineLink
	(DefinedPredicate "Did Someone New Speak?")
	(SequentialAnd
		(NotLink
			(Equal
				(Get (State last-speaker (Variable "$fid")))
				(Get (State prev-speaker (Variable "$fid")))))
		(True
			(Put (State prev-speaker (Variable "$fid"))
				(Get (State last-speaker (Variable "$fid")))))
	)
)
(DefineLink
	(DefinedSchema "current-speaker")
   (Get (State last-speaker (Variable "$x"))))
(DefineLink
	(DefinedSchema "other-speaker")
   (Get (State last-speaker (Variable "$x"))))
(DefineLink
	(DefinedSchema "current-salient")
   (Concept "salient-point"))
(define-public chat-state (AnchorNode "Chat State"))
(define-public chat-listen (ConceptNode "Listening"))
(define-public chat-listen-start (ConceptNode "Listening Start"))
(define-public chat-listen-stop (ConceptNode "Listening Stop"))
(define-public chat-start  (ConceptNode "Start Talking"))
(define-public chat-talk   (ConceptNode "Talking"))
(define-public chat-stop   (ConceptNode "Stop Talking"))
(define-public chat-idle   (ConceptNode "Chat inactive"))
(StateLink chat-state chat-idle)
(DefineLink
	(DefinedPredicate "chatbot started talking?")
	(Equal (Set chat-start)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot is talking?")
	(Equal (Set chat-talk)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot stopped talking?")
	(Equal (Set chat-stop)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot started listening?")
	(Equal (Set chat-listen-start)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot is listening?")
	(Equal (Set chat-listen)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot stopped listening?")
	(Equal (Set chat-listen-stop)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-state (Variable "$x")))))
(define-public chat-affect (AnchorNode "Chat Affect"))
(define-public chat-happy (ConceptNode "Happy"))
(define-public chat-negative (ConceptNode "Negative"))
(StateLink chat-affect chat-happy)
(DefineLink
	(DefinedPredicate "chatbot is happy")
	(Equal
		(Set chat-happy)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-affect (Variable "$x")))))
(DefineLink
	(DefinedPredicate "chatbot is negative")
	(Equal
		(Set chat-negative)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State chat-affect (Variable "$x")))))
(define heard-sound (Anchor "Heard Something Recently"))
(define heard-nothing (SentenceNode ""))
(State heard-sound heard-nothing)
(DefineLink
	(DefinedPredicate "heard text")
	(LambdaLink
		(Variable "$text")
		(SequentialAnd
			(Evaluation (GroundedPredicate "scm: dispatch-text")
				(ListLink (Variable "$text")))
			(True (DefinedSchema "set heard-something timestamp"))
			(True (Put
					(State heard-sound (Variable "$noise"))
					(Variable "$text")))
		)
	)
)
(DefineLink
	(DefinedPredicate "Heard Something?")
	(SequentialAnd
		(NotLink (Equal (SetLink heard-nothing)
			(Get
				(TypedVariable (Variable "$x") (Type "SentenceNode"))
				(State heard-sound (Variable "$x")))))
		(True (Put (State heard-sound (Variable "$x")) heard-nothing))
	))
(define loud-sound  (AnchorNode "Sudden sound change value"))
(define no-loud-sound (Number 0.0))
(State loud-sound no-loud-sound)
(define decibel-value (AnchorNode "Decibel value"))
(define very-low-sound (Number 35))
(define normal-conversation (Number 65))
(define very-loud-sound (Number 84))
(define no-sound (Number 0.0))
(State decibel-value no-sound)
(DefineLink
	(DefinedPredicate "Heard Loud Voice?")
	(GreaterThan
		(Get (State loud-sound (Variable "$x"))) no-loud-sound))
(DefineLink
	(DefinedPredicate "Heard Loud Sound?")
	(GreaterThan
		(Get (State decibel-value (Variable "$y"))) very-loud-sound))
(DefineLink
	(DefinedPredicate "very low sound?")
	(NotLink (GreaterThan
		(Get (State decibel-value (Variable "$y"))) very-low-sound)))
(DefineLink
	(DefinedPredicate "normal conversation?")
	(NotLink (GreaterThan
		(Get (State decibel-value (Variable "$z"))) normal-conversation)))
(DefineLink
	(DefinedPredicate "Heard very loud sound?")
	(NotLink (GreaterThan
		(Get (State decibel-value (Variable "$a"))) very-loud-sound)))
(define salient-loc  (AnchorNode "Salient location"))
(State salient-loc (List (NumberNode 1.0) (NumberNode 0) (NumberNode 0)))
(define salient-degree (AnchorNode "Salient degree"))
(State salient-degree (Number 0))
(DefineLink
	(DefinedPredicate "saliency")
	(GreaterThan
		(Get (State salient-degree (Variable"$S"))) (Number 13)))
(DefineLink
	(DefinedPredicate "saliency required?")
	(SequentialAnd
		(EqualLink
			(DefinedSchemaNode "Num visible faces")
			(NumberNode 0))
		(DefinedPredicate "saliency")))
(define luminance-value (AnchorNode "luminance"))
(define bright (Number 40))
(State luminance-value (Number 25))
(DefineLink
	(DefinedPredicate "Room bright?")
	(GreaterThan
		(Get (State luminance-value (Variable "$x")))
		bright))
(define (timestamp-template name)
	(define ts-name (string-append "start-" name "-timestamp"))
	(define prev-ts (string-append "previous-" name "-call"))
	(State (Schema ts-name) (Number 0))
	(DefineLink
		(DefinedSchema (string-append "set " name " timestamp"))
		(Put (State (Schema ts-name) (Variable "$x")) (TimeLink)))
	(DefineLink
		(DefinedSchema (string-append "get " name " timestamp"))
		(Get
			(TypedVariable (Variable "$x") (Type "NumberNode"))
			(State (Schema ts-name) (Variable "$x"))))
	(State (Schema prev-ts) (Number 0))
)
(timestamp-template "interaction")
(timestamp-template "expression")
(timestamp-template "gesture")
(timestamp-template "bored")
(timestamp-template "sleep")
(timestamp-template "attn-search")
(timestamp-template "glance")
(timestamp-template "heard-something")
(DefineLink
	(DefinedPredicateNode "Did someone arrive?")
	(SatisfactionLink
		(TypedVariable (Variable "$face-id") (TypeNode "NumberNode"))
		(AndLink
			(PresentLink (EvaluationLink (PredicateNode "visible face")
					(ListLink (VariableNode "$face-id"))))
			(AbsentLink (EvaluationLink (PredicateNode "acked face")
					(ListLink (VariableNode "$face-id"))))
		)))
(DefineLink
	(DefinedPredicateNode "Did someone recognizable arrive?")
	(SatisfactionLink
		(VariableList
			(TypedVariableLink
				(VariableNode "$face-id")
				(TypeNode "ConceptNode"))
			(TypedVariableLink
				(VariableNode "$recog-id")
				(TypeNode "ConceptNode"))
			(TypedVariableLink
				(VariableNode "$face-id-nn")
				(TypeNode "NumberNode")))
		(AndLink
			(PresentLink (EvaluationLink (PredicateNode "visible face")
					(ListLink (VariableNode "$face-id-nn"))))
			(AbsentLink (EvaluationLink (PredicateNode "acked face")
					(ListLink (VariableNode "$face-id-nn"))))
			(PresentLink (EvaluationLink (PredicateNode "name")
					(ListLink
					(VariableNode "$face-id")
					(VariableNode "$recog-id"))))
			(Not (Equal (VariableNode "$recog-id") (ConceptNode "0")))
			(EvaluationLink
				(GroundedPredicate "scm: is_nn_equal_cn?")
				(ListLink
					(VariableNode "$face-id-nn")
					(VariableNode "$face-id")))
			)))
(define-public (is_nn_equal_cn? number-node concept-node)
	(if (equal? (cog-number number-node)
			(exact->inexact (cog-number concept-node)))
		(stv 1 1)
		(stv 0 1)
	)
)
(DefineLink
	(DefinedSchemaNode "New arrivals")
	(Get
		(TypedVariable (Variable "$face-id") (Type "NumberNode"))
		(AndLink
			(PresentLink (EvaluationLink (PredicateNode "visible face")
					(ListLink (VariableNode "$face-id"))))
			(AbsentLink (EvaluationLink (PredicateNode "acked face")
					(ListLink (VariableNode "$face-id"))))
	)))
(DefineLink
	(DefinedSchema "Current interaction target")
	(Get
		(TypedVariable (Variable "$x") (TypeNode "NumberNode"))
		(State interaction-state (Variable "$x"))))
(DefineLink
	(DefinedPredicateNode "Did someone leave?")
	(SatisfactionLink
		(TypedVariable (Variable "$face-id") (Type "NumberNode"))
		(AndLink
			(PresentLink (EvaluationLink (PredicateNode "acked face")
					(ListLink (VariableNode "$face-id"))))
			(AbsentLink (EvaluationLink (PredicateNode "visible face")
					(ListLink (VariableNode "$face-id"))))
		)))
(DefineLink
	(DefinedSchema "New departures")
	(Get
		(TypedVariable (Variable "$face-id") (Type "NumberNode"))
		(AndLink
			(PresentLink (Evaluation (Predicate "acked face")
					(ListLink (Variable "$face-id"))))
			(AbsentLink (Evaluation (Predicate "visible face")
					(List (Variable "$face-id"))))
	)))
(DefineLink
	(DefinedPredicateNode "was room empty?")
	(EqualLink
		(SetLink room-empty)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(StateLink room-state (VariableNode "$x")))
	))
(DefineLink
	(DefinedPredicateNode "Someone visible?")
	(SatisfactionLink
		(TypedVariable (Variable "$face-id") (Type "NumberNode"))
		(PresentLink
			(EvaluationLink (PredicateNode "acked face")
					(ListLink (VariableNode "$face-id")))
		)))
(DefineLink
	(DefinedSchema "Num visible faces")
	(Arity
		(Get
			(TypedVariable (Variable "$face-id") (Type "NumberNode"))
			(Evaluation (Predicate "acked face")
				(ListLink (Variable "$face-id"))))))
(DefineLink
	(DefinedPredicateNode "More than one face visible")
	(GreaterThanLink
		(DefinedSchemaNode "Num visible faces")
		(NumberNode 1)))
(DefineLink
	(DefinedSchema "Get acknowledged faces")
	(Get
	   (TypedVariable (Variable "$face-id") (Type "NumberNode"))
	   (Evaluation (Predicate "acked face")
		   (ListLink (Variable "$face-id")))
	))
(DefineLink
	(DefinedSchema "Get recognized faces")
	(Get
		(VariableList
			(TypedVariable
				(Variable "face-id")
				(TypeNode "ConceptNode"))
			(TypedVariable
				(Variable "recog-id")
				(TypeNode "ConceptNode")))
		(AndLink
			(Not (Equal (VariableNode "recog-id") (ConceptNode "0")))
			(EvaluationLink (PredicateNode "name")
				(ListLink
					(VariableNode "face-id")
					(VariableNode "recog-id"))))))
(DefineLink
	(DefinedSchema "Get recognized face's face id")
	(LambdaLink
		(VariableList
			(TypedVariable
				(Variable "face-id")
				(TypeNode "ConceptNode"))
			(TypedVariable
				(Variable "recog-id")
				(TypeNode "ConceptNode")))
		(ExecutionOutputLink
			(GroundedSchemaNode "scm: get-face-id")
			(ListLink
				(VariableNode "face-id")))))
(define-public (get-face-id face-concept)
"
  get-face-id FACE-CONCEPT - Cast ConceptNode to NumberNode
XXX FIXME this is a nasty ugly hack, and shold be replaced by
proper atomese.
"
	(NumberNode (cog-name face-concept))
)
(DefineLink
	(DefinedSchema "Select random face")
	(RandomChoice (DefinedSchema "Get acknowledged faces")))
(DefineLink
	(DefinedPredicate "Select random glance target")
	(SequentialAnd
		(TrueLink
			(PutLink (StateLink glance-state (VariableNode "$face-id"))
				(DefinedSchemaNode "Select random face")))
		(EqualLink
			(Get
				(TypedVariable (Variable "$face-id") (Type "NumberNode"))
				(StateLink glance-state (VariableNode "$face-id")))
			(Get
				(TypedVariable (Variable "$face-id") (Type "NumberNode"))
				(StateLink eye-contact-state (VariableNode "$face-id")))
		)
		(DefinedPredicateNode "More than one face visible")
		(DefinedPredicateNode "Select random glance target")
	))
(DefineLink
	(DefinedPredicate "Update status")
	(SequentialAnd
		(DefinedPredicate "Update room state")
		(True (Put
				(Evaluation (Predicate "acked face")
						(ListLink (Variable "$face-id")))
				(Get
					(TypedVariable (Variable "$x") (Type "NumberNode"))
					(State eye-contact-state (Variable "$x")))))
	))
(DefineLink
	(DefinedPredicateNode "Clear lost face")
	(TrueLink (PutLink
		(DeleteLink
			(EvaluationLink (PredicateNode "acked face")
				(ListLink (VariableNode "$face-id"))))
		(DefinedSchemaNode "New departures"))
	))
(DefineLink
	(DefinedPredicate "Is interacting with someone?")
	(OrLink
		(DefinedPredicate "chatbot is talking?")
		(NotLink (Equal
			(SetLink no-interaction)
			(Get
				(TypedVariable (Variable "$x") (Type "NumberNode"))
				(State interaction-state (Variable "$x"))))
	)))
(DefineLink
	(DefinedPredicate "Skip Interaction?")
	(Equal
		(SetLink face-tracking-off)
		(Get
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(State face-tracking-state (Variable "$x"))))
	)
(DefineLink
	(DefinedPredicate "Someone requests interaction?")
	(NotLink (Equal
		(SetLink no-interaction)
		(Get (State request-eye-contact-state (Variable "$x"))))
	))
(DefineLink
	(DefinedPredicate "look at person")
	(SequentialAnd
		(DefinedPredicate "Time to reset glance")
		(NotLink (Equal
			(Get (State eye-contact-state (Variable "$x")))
			(SetLink no-interaction)))
		(True (Put
			(Evaluation (DefinedPredicate "look-at-face")
				(ListLink (Variable "$face")))
			(Get (State eye-contact-state (Variable "$x")))))
	))
(DefineLink
	(DefinedPredicate "break eye contact")
	(True (Put (State eye-contact-state (Variable "$face-id"))
		no-interaction))
)
(DefineLink
	(DefinedPredicate "make eye contact")
	(True (Put (State eye-contact-state (Variable "$face-id"))
		(Get (State interaction-state (Variable "$x"))) ))
)
(DefineLink
	(DefinedPredicate "return to neutral")
	(SequentialAnd
		(Put (DefinedPredicate "Look at point") neutral-direction)
		(True (Put
			(State eye-contact-state (Variable "$face-id"))
			no-interaction))
		(True (Put
			(State interaction-state (Variable "$face-id"))
			no-interaction))
	))
(DefineLink
	(DefinedPredicate "glance and ack")
	(LambdaLink
		(Variable "$face-id")
		(SequentialAndLink
			(Evaluation (DefinedPredicate "glance-at-face")
				(ListLink (Variable "$face-id")))
			(True (DefinedSchemaNode "set glance timestamp"))
			(Evaluation (Predicate "acked face")
				(ListLink (Variable "$face-id")))
		)))
(DefineLink
	(DefinedPredicate "glance at random face")
	(SequentialAnd
		(DefinedPredicate "Select random glance target")
		(Put
			(DefinedPredicate "glance and ack")
			(Get
				(TypedVariable (Variable "$face-id") (Type "NumberNode"))
				(State glance-state (Variable "$face-id")))
		)
	))
(DefineLink
	(DefinedSchema "glance at new person")
	(Put
		(DefinedPredicate "glance and ack")
		(RandomChoice (DefinedSchema "New arrivals"))
	))
(DefineLink
	(DefinedPredicate "glance at departure")
	(LambdaLink
		(Variable "$face-id")
		(SequentialAndLink
			(Evaluation (GroundedPredicate "scm: glance-at-face")
				(ListLink (Variable "$face-id")))
			(True (DefinedSchemaNode "set glance timestamp"))
		)))
(DefineLink
	(DefinedSchema "glance at lost face")
	(Put
		(DefinedPredicate "glance at departure")
		(RandomChoice (DefinedSchema "New departures"))
	))
(DefineLink
	(DefinedPredicate "Set interaction target")
	(LambdaLink
		(Variable "$face-id")
		(SequentialAnd
			(True (StateLink eye-contact-state (VariableNode "$face-id")))
			(True (StateLink interaction-state (VariableNode "$face-id")))
			(True (DefinedSchema "set interaction timestamp"))
		)))
(DefineLink
	(DefinedPredicate "Change interaction")
	(SequentialAnd
		(True (Put
			(DefinedPredicate "Set interaction target")
			(DefinedSchema "Select random face")))
		(Evaluation (GroundedPredicate "scm: print-msg-face")
			(ListLink (Node "--- Start new interaction")))
	))
(DefineLink
	(DefinedPredicate "interact with new person")
	(SequentialAnd
		(DefinedPredicateNode "Did someone arrive?")
		(True (Put (DefinedPredicate "Set interaction target")
			(RandomChoice (DefinedSchema "New arrivals")))))
)
(DefineLink
	(DefinedPredicate "interact with requested person")
	(SequentialAnd
		(True (Put (DefinedPredicate "Set interaction target")
			(Get (State request-eye-contact-state (Variable "$x")))))
		(True (Put (State request-eye-contact-state (Variable "$face-id"))
			no-interaction))
	))
(define-public current-demo-mode (Anchor "Current Demo Mode"))
(define default-mode (Concept "Default Mode"))
(define reasoning-mode (Concept "Reasoning Mode"))
(define philosophy-mode (Concept "Philosophy Mode"))
(define saliency-mode (Concept "Saliency Mode"))
(State current-demo-mode default-mode)
(define-public (enable-all-demos)
"
  This is the default mode. All the rules are given a weight of 0.9.
"
	(define rules-not-to-be-enabled
		(map (lambda (s) (string-append psi-prefix-str s))
		(list "aiml" "random_sentence_blogs" "saliency-tracking")))
	(psi-controller-occupy)
	(for-each
		(lambda (r)
			(if (member (cog-name (car (psi-rule-alias r))) rules-not-to-be-enabled)
				(psi-rule-set-atomese-weight r 0)
				(psi-rule-set-atomese-weight r 0.9)
			))
		(psi-get-controlled-rules)
	)
	(psi-controller-release)
)
(define (enable-demo-rules rule-aliases)
	(define rules-to-be-enabled
		(map (lambda (s) (string-append psi-prefix-str s)) rule-aliases))
	(psi-controller-occupy)
	(for-each
		(lambda (r)
			(if (member (cog-name (car (psi-rule-alias r))) rules-to-be-enabled)
				(psi-rule-set-atomese-weight r 0.9)
				(psi-rule-set-atomese-weight r 0.0)
			))
		(psi-get-controlled-rules)
	)
	(psi-controller-release)
)
(define-public (disable-all-demos)
"
  This is run when disabling all the rules, when switching between modes.
  When disabling the rules, their weight is set to zero.
  When adding new demo modes, make sure you run (psi-halt) after calling
  this function.
"
	(psi-controller-occupy)
	(for-each
		(lambda (r) (psi-rule-set-atomese-weight r 0.0))
		(psi-get-controlled-rules)
	)
	(psi-controller-release)
)
(define-public (enable-saliency-demo)
"
  Enables the visual saliency rule.
"
	(enable-demo-rules (list "saliency-tracking"))
)
(define-public (enable-philosophy-demo)
"
  Enables the random_sentence_pkd and random_sentence_blogs rules.
"
	(enable-demo-rules (list "random_sentence_pkd" "random_sentence_kurzweil"))
)
(define-public (enable-pln-demo)
"
  Enables the openpsi-pln rule.
"
	(enable-demo-rules (list "select_pln_answer"))
)
(define-public (show-demo-state)
"
  Returns an a-list with rule aliases for keys and their weights for values.
"
	(define result '())
	(let ((rules (psi-get-controlled-rules)))
		(for-each (lambda (x) (set! result
			(assoc-set! result
				(psi-suffix-str (cog-name (car (psi-rule-alias x))))
				(cog-mean x))))
			rules
		)
		result
	)
)
(define-public (switch-demo-mode MODE)
"
  To go into a specific demo mode
"
	(define m (cog-name (gar MODE)))
	(cond
		((equal? m "reasoning-demo")
			(enable-pln-demo)
			(State current-demo-mode reasoning-mode)
		)
		((equal? m "philosophy-demo")
			(enable-philosophy-demo)
			(State current-demo-mode philosophy-mode)
		)
		((equal? m "saliency-demo")
			(enable-saliency-demo)
			(State current-demo-mode saliency-mode)
		)
	)
	(stv 1 1)
)
(define-public (back-to-default-mode)
"
  To exit the demo and return to the default mode
"
	(enable-all-demos)
	(State current-demo-mode default-mode)
)
(Define
	(DefinedPredicate "Do show demo")
	(Lambda
		(Variable "$demo-mode")
		(Put (DefinedPredicate "Show demo")
			(List (Variable "$demo-mode"))))
)
(Define
	(DefinedPredicate "Show demo")
	(Lambda
		(Variable "$demo-mode")
		(Evaluation
			(GroundedPredicate "scm: switch-demo-mode")
			(List (Variable "$demo-mode"))
		)
	)
)
(Define
	(DefinedPredicate "exit-demo-mode")
	(Evaluation
		(GroundedPredicate "scm: back-to-default-mode")
		(List)))
(Define
	(DefinedPredicate "is-in-any-demo-mode?")
	(Not (Equal
		(Set default-mode)
		(Get (State current-demo-mode (Variable "$x"))))))
(Define
	(DefinedPredicate "is-in-reasoning-mode?")
	(Equal
		(Set reasoning-mode)
		(Get (State current-demo-mode (Variable "$x"))))
)
*unspecified*