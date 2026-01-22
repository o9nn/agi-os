(use-modules (opencog logger))
(define (delete-definition STR)
	(define dfn
		(cog-get-link 'DefineLink 'DefinedPredicateNode
			(DefinedPredicate STR)))
	(if (not (null? dfn)) (cog-extract! (car dfn)) #f))
(define-public (prt-pred-defn PRED . REST)
   (cog-logger-info "Called (DefinedPredicate \"~a\") with args ~a\n"
		(cog-name PRED) REST)
   (stv 1 1))
(define-public (prt-face-expr PRED NAME TIME TENS)
	(format #t "Robot displays facial expression \"~a\" at strength ~a for ~a seconds\n"
		(cog-name NAME)
		(cog-name TIME)
		(cog-name TENS))
	(prt-pred-defn PRED NAME TIME TENS)
)
(define-public (prt-face-gest PRED NAME TENS RPT SPD)
	(format #t "Robot performs facial gesture \"~a\" at strength ~a speed ~a\n"
		(cog-name NAME)
		(cog-name TENS)
		(cog-name SPD))
	(prt-pred-defn PRED NAME TENS RPT SPD)
)
(define-public (prt-gaze-dir PRED X Y Z)
	(format #t "Robot looks at point (~a ~a ~a)\n"
		(cog-name X)
		(cog-name Y)
		(cog-name Z))
	(prt-pred-defn PRED X Y Z)
)
(define-public (prt-turn-dir PRED X Y Z)
	(format #t "Robot turns head towards (~a ~a ~a)\n"
		(cog-name X)
		(cog-name Y)
		(cog-name Z))
	(prt-pred-defn PRED X Y Z)
)
(define (dfn-pred PRED)
	(DefineLink
		PRED
		(EvaluationLink
			(GroundedPredicate "scm: prt-pred-defn")
			(ListLink PRED))))
(delete-definition "Do show facial expression")
(DefineLink
	(DefinedPredicate "Do show facial expression")
	(LambdaLink
		(VariableList
			(Variable "$expr")
			(Variable "$duration")
			(Variable "$intensity"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-face-expr")
			(ListLink
				(DefinedPredicate "Do show facial expression")
				(Variable "$expr")
				(Variable "$duration")
				(Variable "$intensity"))
		)))
(delete-definition "Do show gesture")
(DefineLink
	(DefinedPredicate "Do show gesture")
	(LambdaLink
		(VariableList
			(Variable "$gest")
			(Variable "$insensity")
			(Variable "$repeat")
			(Variable "$speed"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-face-gest")
			(ListLink
				(DefinedPredicate "Do show gesture")
				(Variable "$gest")
				(Variable "$insensity")
				(Variable "$repeat")
				(Variable "$speed"))
		)))
(delete-definition "Conversational Saccade")
(delete-definition "Listening Saccade")
(delete-definition "Explore Saccade")
(dfn-pred (DefinedPredicate "Conversational Saccade"))
(dfn-pred (DefinedPredicate "Listening Saccade"))
(dfn-pred (DefinedPredicate "Explore Saccade"))
(delete-definition "Blink rate")
(DefineLink
	(DefinedPredicate "Blink rate")
	(LambdaLink
		(VariableList (Variable "$mean") (Variable "$var"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-pred-defn")
			(ListLink
				(DefinedPredicate "Blink rate")
				(Variable "$mean") (Variable "$var"))
		)))
(delete-definition "Do look at point")
(DefineLink
	(DefinedPredicate "Do look at point")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-turn-dir")
			(ListLink
				(DefinedPredicate "Do look at point")
				(Variable "$x") (Variable "$y") (Variable "$z"))
		)))
(delete-definition "Do gaze at point")
(DefineLink
	(DefinedPredicate "Do gaze at point")
	(LambdaLink
		(VariableList (Variable "$x") (Variable "$y") (Variable "$z"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-gaze-dir")
			(ListLink
				(DefinedPredicate "Do gaze at point")
				(Variable "$x") (Variable "$y") (Variable "$z"))
		)))
(delete-definition "Publish behavior")
(DefineLink
	(DefinedPredicate "Publish behavior")
	(LambdaLink
		(VariableList (Variable "$bhv"))
		(EvaluationLink
			(GroundedPredicate "scm: prt-pred-defn")
			(ListLink
				(DefinedPredicate "Publish behavior")
				(Variable "$bhv"))
		)))
(delete-definition "Do go to sleep")
(DefineLink
	(DefinedPredicate "Do go to sleep")
	(EvaluationLink
		(GroundedPredicate "scm: prt-pred-defn")
		(ListLink
			(DefinedPredicate "Do go to sleep"))
	))
(delete-definition "Do wake up")
(DefineLink
	(DefinedPredicate "Do wake up")
	(EvaluationLink
		(GroundedPredicate "scm: prt-pred-defn")
		(ListLink
			(DefinedPredicate "Do wake up"))
	))
(define-public (prt-say-text SENT)
   (cog-logger-info "Saying this: ~a\n" SENT)
   (stv 1 1))
(delete-definition "Say")
(DefineLink
	(DefinedPredicate "Say")
	(LambdaLink (Variable "sentence")
		(Evaluation
			(GroundedPredicate "scm: prt-say-text")
			(List (Variable "sentence")))
	))
(delete-definition "ROS is running?")
(DefineLink
	(DefinedPredicate "ROS is running?") (True))
*unspecified*