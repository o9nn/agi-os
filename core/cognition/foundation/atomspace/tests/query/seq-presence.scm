(use-modules (opencog) (opencog exec))
(define room-state (AnchorNode "Room State"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))
(ListLink room-state room-empty)
(define (tv-print-atom atom)
(format #t "Hello, I got this atom: ~a\n" atom) (stv 1 1))
(define empty-sequence
(SatisfactionLink
(SequentialAndLink
(PresentLink (ListLink room-state (VariableNode "$x")))
(EqualLink (VariableNode "$x") room-empty)
(EvaluationLink
(GroundedPredicateNode "scm: tv-print-atom")
(ListLink (VariableNode "$x")))
)))