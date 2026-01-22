(use-modules (opencog) (opencog exec))
(define room-state (Anchor "Room State"))
(define room-empty (Concept "room empty"))
(define room-nonempty (Concept "room nonempty"))
(List room-state room-empty)
(define (tv-print-msg)
(display "Hello, I've been triggered!\n") (stv 1 1))
(define (tv-print-atom atom)
(format #t "Hello, I got this atom: ~a\n" atom) (stv 1 1))
(define (atom-print-atom atom)
(format #t "Hello, Executing with atom: ~a\n" atom) atom)
(define empty-sequence
(Satisfaction
(SequentialAnd
(Present (List room-state (Variable "$x")))
(Equal (Variable "$x") room-empty)
(Evaluation
(GroundedPredicate "scm: tv-print-atom")
(List (Variable "$x")))
)))
(cog-evaluate! empty-sequence)
(define get-empty-seq
(Satisfaction
(SequentialAnd
(Equal
(Set room-empty)
(Get (List room-state (Variable "$x"))))
(Evaluation
(GroundedPredicate "scm: tv-print-msg")
(List))
)))
(cog-evaluate! get-empty-seq)
(define bind-empty
(Bind
(And
(List room-state (Variable "$x"))
(Equal (Variable "$x") room-empty)
)
(ExecutionOutput
(GroundedSchema "scm: atom-print-atom")
(List (Variable "$x")))
))
(cog-execute! bind-empty)
(define put-empty-atom
(Put
(ExecutionOutput
(GroundedSchema "scm: atom-print-atom")
(List (Variable "$x")))
(Get
(And
(List room-state (Variable "$y"))
(Equal (Variable "$y") room-empty)))
))
(cog-execute! put-empty-atom)