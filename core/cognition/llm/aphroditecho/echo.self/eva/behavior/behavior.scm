(define bhv-source (Concept "Behavior Tree"))
(DefineLink
	(DefinedPredicate "Request interaction with person who spoke")
	(True
		(PutLink
			(StateLink request-eye-contact-state (VariableNode "$fid"))
			(GetLink (TypedVariable (Variable "$fid") (TypeNode "NumberNode"))
				(StateLink (ConceptNode "previous person who spoke") (VariableNode "$fid"))
			)
		)
	)
)
(DefineLink
	(DefinedPredicate "Interact with face")
	(SequentialAnd
		(DefinedPredicate "look at person")
		(SequentialOr
			(SequentialAnd
				(SequentialOrLink
					(NotLink (DefinedPredicateNode "Time to change expression"))
					(DefinedPredicateNode "Show positive expression")
				)
				(SequentialOrLink
					(NotLink (DefinedPredicateNode "Time to make gesture"))
					(DefinedPredicateNode "Pick random positive gesture"))
		))
	))
(DefineLink
	(DefinedPredicate "Was Empty Sequence")
	(SequentialAnd
		(DefinedPredicate "was room empty?")
		(Put (DefinedPredicate "Request Set Face Expression")
			(ListLink bhv-source (Concept "new-arrival")))
		(DefinedPredicate "interact with new person")
		(DefinedPredicate "look at person")
		(Put (DefinedPredicate "Show random expression")
			(ConceptNode "new-arrival"))
		(Put (DefinedPredicate "Publish behavior")
			(Concept "Look at new arrival"))
		(Evaluation (GroundedPredicate "scm: print-msg-face")
			(ListLink (Node "--- Look at newly arrived person")))
	))
(DefineLink
	(DefinedPredicate "Interaction requested action")
	(SequentialAnd
		(True (DefinedPredicate "If sleeping then wake"))
		(True (DefinedPredicate "If bored then alert"))
		(DefinedPredicate "interact with requested person")
		(DefinedPredicate "look at person")
		(Put (DefinedPredicate "Publish behavior")
			(Concept "Look at requested face"))
		(Evaluation (GroundedPredicate "scm: print-msg-face")
			(ListLink (Node "--- Looking at requested face")))
	))
(DefineLink
	(DefinedPredicate "Interacting Sequence")
	(SequentialAnd
		(DefinedPredicate "Is interacting with someone?")
		(DefinedPredicate "dice-roll: glance new face")
		(True (DefinedSchema "glance at new person"))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Glance at new person")))
	))
(DefineLink
	(DefinedPredicate "Interacting Sequence for recognized person")
	(SequentialAnd
		(True (Put
			(DefinedPredicate "Set interaction target")
			(RandomChoice (Put
				(DefinedSchemaNode "Get recognized face's face id")
				(DefinedSchema "Get recognized faces")))))
		(DefinedPredicate "look at person")
		(DefinedPredicate "Update status")
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Glance at new recognized person")))
	))
(DefineLink
	(DefinedPredicate "Respond to new arrival")
	(SequentialOr
		(DefinedPredicate "Was Empty Sequence")
		(DefinedPredicate "Interacting Sequence")
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Ignoring new person")))
		(True)
	))
(DefineLink
	(DefinedPredicate "If sleeping then wake")
	(SequentialAnd
		(DefinedPredicate "Is sleeping?")
		(DefinedPredicate "Wake up")))
(DefineLink
	(DefinedPredicate "If bored then alert")
	(SequentialAnd
		(DefinedPredicate "Is bored?")
		(Evaluation (DefinedPredicate "Request Set Soma State")
			(ListLink bhv-source soma-awake))))
(DefineLink
	(DefinedPredicate "New arrival sequence")
	(SequentialAnd
		(True (DefinedPredicate "If sleeping then wake"))
		(True (DefinedPredicate "If bored then alert"))
		(DefinedPredicate "Respond to new arrival")
		(DefinedPredicate "Update status")
	))
(DefineLink
	(DefinedPredicate "Someone left action")
	(SequentialAnd
		(Put (DefinedPredicate "Publish behavior")
			(Concept "Someone left"))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Someone left")))
		(SequentialOr
			(SequentialAnd
				(Equal
					(DefinedSchema "New departures")
					(Get
						(TypedVariable (Variable "$x") (Type "NumberNode"))
						(State eye-contact-state (Variable "$x"))))
				(DefinedPredicate "Show frustrated expression")
				(DefinedPredicate "return to neutral")
			)
			(SequentialAnd
				(DefinedPredicate "Is interacting with someone?")
				(SequentialOr
					(NotLink (DefinedPredicate "dice-roll: glance lost face"))
					(FalseLink (DefinedSchema "glance at lost face"))
					(Evaluation (GroundedPredicate "scm: print-msg")
						(ListLink (Node "--- Glance at lost face"))))
				(TrueLink)
			)
			(Evaluation (GroundedPredicate "scm: print-msg")
				(ListLink (Node "--- Ignoring lost face")))
			(TrueLink)
		)
		(DefinedPredicate "Clear lost face")
		(DefinedPredicate "Update room state")
	))
(DefineLink
	(DefinedPredicate "Interact with people")
	(SequentialAnd
		(SequentialOr
			(SequentialAnd
				(SequentialOr
					(Not (DefinedPredicate "Is interacting with someone?"))
					(SequentialAnd
						(DefinedPredicate "More than one face visible")
						(DefinedPredicate "Time to change interaction")))
				(DefinedPredicate "Change interaction")
				(DefinedPredicate "Interact with face")
				(Put (DefinedPredicate "Publish behavior")
					(Concept "Interact with someone else"))
			)
			(SequentialAnd
				(SequentialOr
					(SequentialAnd
						(DefinedPredicate "More than one face visible")
						(DefinedPredicate "dice-roll: group interaction")
						(DefinedPredicate "glance at random face"))
					(True))
				(DefinedPredicateNode "Interact with face")
				(SequentialOr
					(SequentialAnd
						(DefinedPredicateNode "dice-roll: face study")
						(False)
					)
					(True))
			)
			(DefinedPredicate "Is interacting with someone?")
		)
	))
(DefineLink
	(DefinedPredicateNode "Search for attention")
	(SequentialAndLink
		(Put (DefinedPredicate "Request Set Face Expression")
			(ListLink bhv-source (ConceptNode "bored")))
		(True (SequentialAnd
			(DefinedPredicate "Heard Something?")
			(Put (DefinedPredicate "Publish behavior")
				(Concept "Who is there?"))
			(True (DefinedSchema "set bored timestamp"))
		))
		(SequentialOr
			(Not (DefinedPredicate "Time to change expression"))
			(False (Put (DefinedPredicate "Publish behavior")
				(Concept "Searching for attention")))
			(PutLink (DefinedPredicateNode "Show random expression")
				(ConceptNode "bored"))
		)
		(SequentialOr
			(Not (DefinedPredicate "Time to make gesture"))
			(PutLink (DefinedPredicateNode "Show random gesture")
				(ConceptNode "bored")))
		(SequentialOr
			(Not (DefinedPredicate "Time to change gaze"))
			(SequentialAnd
				(Evaluation (DefinedPredicate "Look at point")
					(ListLink
						(Number 1)
						(RandomNumber
							(DefinedSchema "gaze right max")
							(DefinedSchema "gaze left max"))
						(Number 0)))
				(True (DefinedSchema "set attn-search timestamp"))
			))
	))
(DefineLink
	(DefinedPredicate "Nothing is happening")
	(SequentialAnd
		(SequentialOr
			(DefinedPredicate "Is bored?")
			(DefinedPredicate "Is sleeping?")
			(SequentialAnd
				(DefinedPredicate "Heard Something?")
				(True (Put (DefinedPredicate "Publish behavior")
					(Concept "What was that sound?")))
				(True (DefinedSchema "set bored timestamp"))
			)
			(SequentialAnd
				(Evaluation (DefinedPredicate "Request Set Soma State")
					(ListLink bhv-source soma-bored))
				(True (DefinedSchema "set bored timestamp"))
				(Put (DefinedPredicate "Publish behavior")
					(Concept "This is boring"))
				(Evaluation (GroundedPredicate "scm: print-msg")
					(ListLink (Node "--- Bored! nothing is happening!")))
			))
		(SequentialOr
			(SequentialAnd
				(Not (DefinedPredicate "Is sleeping?"))
				(SequentialOr
					(SequentialAnd
						(DefinedPredicate "Bored too long")
						(DefinedPredicate "Go to sleep"))
					(DefinedPredicate "Search for attention")
				))
			(SequentialOr
				(SequentialAnd
					(SequentialOr
						(DefinedPredicate "Time to wake up")
						(SequentialAnd
							(DefinedPredicate "Heard Something?")
							(True (Put (DefinedPredicate "Publish behavior")
								(Concept "What was that sound?")))
							(True (DefinedSchema "set bored timestamp"))
						)
					)
					(DefinedPredicate "Wake up")
				)
				(SequentialAndLink
					(TrueLink)
				)
			)
		)
))
(DefineLink
	(DefinedPredicate "Speech started")
	(SequentialAnd
		(DefinedPredicate "Conversational Saccade")
		(Put (DefinedPredicate "Show random gesture")
			(ConceptNode "conversing"))
		(Put (DefinedPredicate "Show random gesture")
			(ConceptNode "chat-positive-nod"))
		(True (Put (State chat-state (Variable "$x")) chat-talk))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Start talking")))
))
(DefineLink
	(DefinedPredicate "Listening started")
	(SequentialAnd
		(DefinedPredicate "Listening Saccade")
		(Put (DefinedPredicate "Show random gesture")
			(ConceptNode "listening"))
		(Put (DefinedPredicate "Show random gesture")
			(ConceptNode "chat-positive-nod"))
		(True (Put (State chat-state (Variable "$x")) chat-listen))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Start Listen")))
))
(DefineLink
	(DefinedPredicate "Speech ongoing")
	(SequentialAnd
		(SequentialOr
			(SequentialAnd
				(DefinedPredicate "chatbot is happy")
				(SequentialOr
					(Not (DefinedPredicate "Time to change expression"))
					(Put (DefinedPredicateNode "Show random expression")
						(ConceptNode "neutral-speech")))
				(SequentialOr
					(Not (DefinedPredicate "Time to make gesture"))
					(SequentialAnd
						(Put (DefinedPredicate "Show random gesture")
							(ConceptNode "chat-positive-nod"))
						(Put (DefinedPredicate "Show random gesture")
							(ConceptNode "chat-pos-think"))
						(Evaluation (DefinedPredicate "Blink rate")
							(ListLink
								(DefinedSchema "blink chat fast mean")
								(DefinedSchema "blink chat fast var")))
				))
			)
			(SequentialAnd
				(DefinedPredicate "chatbot is negative")
				(SequentialOr
					(Not (DefinedPredicate "Time to change expression"))
					(Put (DefinedPredicateNode "Show random expression")
						(ConceptNode "frustrated")))
				(SequentialOr
					(Not (DefinedPredicate "Time to make gesture"))
					(SequentialAnd
						(Put (DefinedPredicate "Show random gesture")
							(ConceptNode "chat-negative-shake"))
						(Put (DefinedPredicate "Show random gesture")
							(ConceptNode "chat-neg-think"))
						(Evaluation (DefinedPredicate "Blink rate")
							(ListLink
								(DefinedSchema "blink chat slow mean")
								(DefinedSchema "blink chat slow var")))
				))
			))))
(DefineLink
	(DefinedPredicate "Speech ended")
	(SequentialAnd
		(DefinedPredicate "Explore Saccade")
		(Evaluation (DefinedPredicate "Blink rate")
			(ListLink
				(DefinedSchema "blink normal mean")
				(DefinedSchema "blink normal var")))
		(True (Put (State chat-state (Variable "$x")) chat-idle))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Finished talking")))
	))
(DefineLink
	(DefinedPredicate "Listening ended")
	(SequentialAnd
		(DefinedPredicate "Explore Saccade")
		(Evaluation (DefinedPredicate "Blink rate")
			(ListLink
				(DefinedSchema "blink normal mean")
				(DefinedSchema "blink normal var")))
		(True (Put (State chat-state (Variable "$x")) chat-idle))
		(Evaluation (GroundedPredicate "scm: print-msg")
			(ListLink (Node "--- Finished talking")))
	))
(DefineLink
	(DefinedPredicate "Listening ongoing")
	(SequentialAnd
		(SequentialOr
			(Not (DefinedPredicate "Time to change expression"))
			(Put (DefinedPredicateNode "Show random expression")
				(ConceptNode "neutral-listen")))
		(SequentialOr
			(Not (DefinedPredicate "Time to make gesture"))
			(SequentialAnd
				(Put (DefinedPredicate "Show random gesture")
					(ConceptNode "chat-positive-nod"))
				(Put (DefinedPredicate "Show random gesture")
					(ConceptNode "chat-pos-think"))
		))
		(TrueLink)
	))
(DefineLink
	(DefinedPredicate "Keep alive")
	(SequentialAnd
		(SequentialOr
			(Not (DefinedPredicate "Time to change expression"))
			(Put (DefinedPredicateNode "Show random expression")
				(ConceptNode "neutral-listen")))
		(SequentialOr
			(Not (DefinedPredicate "Time to make gesture"))
			(SequentialAnd
				(Put (DefinedPredicate "Show random gesture")
					(ConceptNode "chat-positive-nod"))
				(Put (DefinedPredicate "Show random gesture")
					(ConceptNode "chat-pos-think"))
		))
		(TrueLink)
	))
(DefineLink
	(DefinedPredicate "Say whoa!")
		(Put (DefinedPredicate "Say")
			(Node "whoa!")))
(DefineLink
	(DefinedPredicate "React to Sound")
	(SequentialOr
		(SequentialAnd
			(DefinedPredicate "very low sound?")
			(Put (DefinedPredicateNode "Show random expression")
				(ConceptNode "sound-happy"))
			(Evaluation (GroundedPredicate "scm: print-msg")
				(ListLink (Node "--- low sound"))))
		(SequentialAnd
			(DefinedPredicate "normal conversation?")
			(Put (DefinedPredicateNode "Show random expression")
				(ConceptNode "sound-amused"))
			(Evaluation (GroundedPredicate "scm: print-msg")
				(ListLink (Node "--- normal sound"))))
		(SequentialAnd
			(DefinedPredicate "Heard very loud sound?")
			(Put (DefinedPredicateNode "Show random expression")
				(ConceptNode "sound-afraid"))
			(Evaluation (GroundedPredicate "scm: print-msg")
				(ListLink (Node "--- very high sound"))))
))
(DefineLink
	(DefinedPredicate "Curious")
		(Put (DefinedPredicate "Show random gesture")
			(ConceptNode "salient-curious")))
(DefineLink
	(DefinedPredicate "Salient:Curious")
	(SequentialAnd
		(DefinedPredicate "look at salient point")
		))
(DefineLink
	(DefinedPredicate "Bright:happy")
	(Put (DefinedPredicateNode "Show random expression")
		(ConceptNode "luminance-happy")))