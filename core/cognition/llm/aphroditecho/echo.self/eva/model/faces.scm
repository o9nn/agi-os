(use-modules (opencog))
(use-modules (opencog exec))
(define room-state (AnchorNode "Room State"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))
(StateLink room-state room-empty)
(DefineLink
	(DefinedPredicateNode "Check if room non-empty")
	(SatisfactionLink
			(TypedVariable
				(VariableNode "$face-id")
				(TypeNode "NumberNode"))
		(SequentialAndLink
			(PresentLink (EvaluationLink (PredicateNode "visible face")
					(ListLink (VariableNode "$face-id"))))
			(TrueLink (PutLink
					(StateLink room-state (VariableNode "$x"))
					room-nonempty)))))
(DefineLink
	(DefinedPredicateNode "Check if room empty")
	(SatisfactionLink
		(TypedVariable
			(VariableNode "$face-id")
			(TypeNode "NumberNode"))
		(SequentialAndLink
			(AbsentLink (EvaluationLink (PredicateNode "visible face")
						(ListLink (VariableNode "$face-id"))))
			(TrueLink (PutLink
					(StateLink room-state (VariableNode "$x"))
					room-empty)))))
(DefineLink
	(DefinedPredicateNode "Update room state")
	(SatisfactionLink
		(SequentialOrLink
			(DefinedPredicateNode "Check if room non-empty")
			(DefinedPredicateNode "Check if room empty"))))
(define-public (make-new-face id)
"
 make-new-face ID
 Debug utility - Quick hack to fill the room.
 Call this function to trick opencog into thinking there is a new
 visible face.  There will not be any corresponding 3D coords, so
 the ROS tf2 will not be able to make the robot turn to look...
"
	(EvaluationLink (PredicateNode "visible face")
		(ListLink (NumberNode id))))
(define-public (make-recognized-face face-id recog-id)
"
  make-recognized-face FACE-ID RECOG-ID
  FACE-ID is a number that represents the face ID and RECOG-ID is
  a string that represents the recognition ID for the visible face.
  Currently, RECOG-ID is usually just a string name of the person.
  This function returns the atomese representation of the recognized
  face.
"
	(EvaluationLink
		(PredicateNode "name")
		(ListLink
			(ConceptNode (number->string face-id))
			(ConceptNode recog-id)))
)
(define-public (ack-face face-id)
"
  ackno-face FACE-ID
  Returns the atom that represents an acknowledged face with FACE-ID.
"
	(Evaluation
		(Predicate "acked face")
		(ListLink (Number face-id)))
)
(define-public (remove-face id)
"
 remove-face ID
 Quick hack to remove face ID from the room
"
	(cog-extract! (EvaluationLink (PredicateNode "visible face")
		(ListLink (ConceptNode id)))))
(define (show-visible-faces)
	(define visible-face (PredicateNode "visible face"))
	(filter (lambda(y) (equal? (cog-type y) 'NumberNode))
	(map (lambda (x) (car (cog-outgoing-set x)))
	(cog-chase-link 'EvaluationLink 'ListLink visible-face))))
(define-public (show-acked-faces)
	(define acked-face (PredicateNode "acked face"))
	(filter (lambda(y) (equal? (cog-type y) 'NumberNode))
	(map (lambda (x) (car (cog-outgoing-set x)))
	(cog-chase-link 'EvaluationLink 'ListLink acked-face))))
(define (show-recognized-faces)
"
 Show face-id recognized-face name pairs in atomese
"
	(cog-outgoing-set (cog-execute! (DefinedSchema "Get recognized faces")))
)
(define (show-room-state)
	(car (cog-chase-link 'StateLink 'ConceptNode room-state)))
(define (show-eye-contact-state)
	(define e-c-state (Anchor "Eye Contact State"))
	(car (cog-chase-link 'StateLink 'NumberNode e-c-state)))
(define (show-interaction-target)
	(gar (cog-execute! (DefinedSchema "Current interaction target"))))
(define (undefine def)
	(cog-extract! (car (cog-incoming-set def))))
#|
(cog-evaluate! (DefinedPredicateNode "Update room state"))
(show-room-state)
(cog-incoming-set (PredicateNode "visible face"))
|#