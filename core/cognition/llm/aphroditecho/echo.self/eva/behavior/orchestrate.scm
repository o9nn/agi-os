(DefineLink
	(DefinedPredicate "Show facial expression")
	(LambdaLink
		(VariableList
			(Variable "$expr")
			(Variable "$duration")
			(Variable "$intensity"))
		(SequentialAndLink
			(TrueLink (DefinedSchema "set expression timestamp"))
			(TrueLink (State face-expression-state (Variable "$expr")))
			(EvaluationLink (DefinedPredicate "Do show facial expression")
				(ListLink
					(Variable "$expr")
					(Variable "$duration")
					(Variable "$intensity")))
		)))
(DefineLink
	(DefinedPredicate "Show gesture")
	(LambdaLink
		(VariableList
			(Variable "$gest")
			(Variable "$insensity")
			(Variable "$repeat")
			(Variable "$speed"))
		(SequentialAndLink
			(True (DefinedSchema "set gesture timestamp"))
			(EvaluationLink (DefinedPredicate "Do show gesture")
				(ListLink
					(Variable "$gest")
					(Variable "$insensity")
					(Variable "$repeat")
					(Variable "$speed")))
		)))
(DefineLink
	(DefinedPredicate "Look at point")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(SequentialAndLink
			(EvaluationLink (DefinedPredicate "Do look at point")
				(ListLink (Variable "$x") (Variable "$y") (Variable "$z")))
		)))
(DefineLink
	(DefinedPredicate "Gaze at point")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(SequentialAndLink
			(EvaluationLink (DefinedPredicate "Do gaze at point")
				(ListLink (Variable "$x") (Variable "$y") (Variable "$z")))
		)))
(DefineLink
	(DefinedPredicate "Look command")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(SequentialAndLink
			(DefinedPredicate "break eye contact")
			(EvaluationLink (DefinedPredicate "Gaze at point")
				(ListLink (Variable "$x") (Variable "$y") (Variable "$z")))
			(EvaluationLink (DefinedPredicate "Look at point")
				(ListLink (Variable "$x") (Variable "$y") (Variable "$z")))
		)))
(DefineLink
	(DefinedPredicate "Gaze command")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(SequentialAndLink
			(DefinedPredicate "break eye contact")
			(EvaluationLink (DefinedPredicate "Gaze at point")
				(ListLink (Variable "$x") (Variable "$y") (Variable "$z")))
		)))
(DefineLink
	(DefinedPredicate "Look-at-thing cmd")
	(LambdaLink
		(Variable "$object-id")
		(SequentialOr
			(SequentialAnd
				(Equal (Variable "$object-id") (Concept "salient-point"))
				(DefinedPredicate "look at salient point"))
			(Evaluation
				(DefinedPredicate "Set interaction target")
				(ListLink (Variable "$object-id")))
		)))
(define salient-loc  (AnchorNode "Salient location"))
(DefineLink
	(DefinedPredicate "look at salient point")
	(SequentialAnd
		(True (Put
			(Evaluation (DefinedPredicate "Look at point")
				(List (Variable "$x") (Variable "$y") (Variable "$z")))
			(Get (State salient-loc
				(List (Variable "$x") (Variable "$y") (Variable "$z"))))
		))
		(True (Put
			(Evaluation (DefinedPredicate "Gaze at point")
				(List (Variable "$x") (Variable "$y") (Variable "$z")))
			(Get (State salient-loc
				(List (Variable "$x") (Variable "$y") (Variable "$z"))))
		))
	))
(DefineLink
	(DefinedPredicate "Request Set Soma State")
	(LambdaLink
		(VariableList
			(Variable "$requestor")
			(Variable "$state"))
		(True (State soma-state (Variable "$state")))
	))
(DefineLink
	(DefinedPredicate "Request Set Face Expression")
	(LambdaLink
		(VariableList
			(Variable "$requestor")
			(Variable "$state"))
		(True)
	))
(DefineLink
	(DefinedPredicate "Go to sleep")
	(SequentialAnd
		(Put (DefinedPredicate "Request Set Face Expression")
			(ListLink bhv-source (ConceptNode "sleepy")))
		(Evaluation (DefinedPredicate "Request Set Soma State")
			(ListLink bhv-source soma-sleeping))
		(Evaluation (GroundedPredicate "scm: print-msg-time")
			(ListLink (Node "--- Go to sleep.")
				(Minus (TimeLink) (DefinedSchema "get bored timestamp"))))
		(True (DefinedSchema "set sleep timestamp"))
		(Put (DefinedPredicate "Publish behavior")
			(Concept "Falling asleep"))
		(Put (DefinedPredicate "Show random gesture")
			(Concept "sleepy"))
		(Evaluation (DefinedPredicate "Do go to sleep") (ListLink))
	))
(DefineLink
	(DefinedPredicate "Wake up")
	(SequentialAnd
		(Evaluation (DefinedPredicate "Request Set Soma State")
			(ListLink bhv-source soma-awake))
		(Put (DefinedPredicate "Request Set Face Expression")
			(ListLink bhv-source (ConceptNode "wake-up")))
		(Evaluation (GroundedPredicate "scm: print-msg-time")
			(ListLink (Node "--- Wake up!")
				(Minus (TimeLink) (DefinedSchema "get sleep timestamp"))))
		(Put (DefinedPredicate "Publish behavior")
			(Concept "Waking up"))
		(True (DefinedSchema "set bored timestamp"))
		(True (DefinedPredicate "Heard Something?"))
		(True (DefinedSchema "set heard-something timestamp"))
		(Evaluation (DefinedPredicate "Do wake up") (ListLink))
		(Put (DefinedPredicate "Show random expression")
			(Concept "wake-up"))
		(Put (DefinedPredicate "Show random gesture")
			(Concept "wake-up"))
	))
*unspecified*