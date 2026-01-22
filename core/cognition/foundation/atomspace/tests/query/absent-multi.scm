(use-modules (opencog))
(use-modules (opencog exec))
(define mulder
	(EvaluationLink
		(PredicateNode "Agent Mulder")
		(ListLink (VariableNode "$x"))))
(define scully
	(EvaluationLink
		(PredicateNode "Agent Scully")
		(ListLink (VariableNode "$y"))))
(define call-mulder
	(PutLink
		(EvaluationLink
			(PredicateNode "Agent Mulder")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Exploring Area 51"))))
(define call-scully
	(PutLink
		(EvaluationLink
			(PredicateNode "Agent Scully")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Late night in the morgue"))))
(define discredit-mulder
	(BindLink mulder (DeleteLink mulder)))
(define discredit-scully
	(BindLink scully (DeleteLink scully)))
(define ufo-state (AnchorNode "UFO"))
(define ufo-denied (ConceptNode "Government denies knowledge"))
(define ufo-exists (ConceptNode "Located at Area 51"))
(define ufo-proven (ConceptNode "Undeniable evidence for UFO's"))
(define (get-denied) ufo-denied)
(define (get-exists) ufo-exists)
(define (get-proven) ufo-proven)
(StateLink ufo-state ufo-exists)
(define is-visible
	(BindLink
		(AndLink (AbsentLink mulder) (AbsentLink scully))
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-exists)
	)
)
(define is-invisible
	(BindLink
		(ChoiceLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-denied)
	)
)
(define is-proven
	(BindLink
		(AndLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-proven)
	)
)
(define (show-ufo-state)
   (car (cog-chase-link 'StateLink 'ConceptNode ufo-state)))