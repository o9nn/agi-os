(use-modules (opencog) (opencog exec))
(cog-set-value!
(Concept "Aybe Sea") (Predicate "key")
(QueueValue
(Concept "E") (Concept "A") (Concept "D")
(Concept "G") (Concept "B") (Concept "E")))
(cog-execute! (StreamValueOf (Concept "Aybe Sea") (Predicate "key")))
(define generator
(let ((str "a"))
(lambda () (set! str (string-concatenate (list str "b")))
(Concept str))))
(define (create) (generator))
(define (prt atom)
(format #t "Got this atom: ~A\n" atom)
(stv 1 1))
(cog-set-value!  (Concept "abc") (Predicate "key") (QueueValue))
(define mt
(ThreadJoin
(SequentialAnd
(True (SetValue (Concept "abc") (Predicate "key")
(ExecutionOutput (GroundedSchema "scm: create") (List))))
(True (Sleep (Number 1)))
(True (SetValue (Concept "abc") (Predicate "key")
(ExecutionOutput (GroundedSchema "scm: create") (List))))
(True (Sleep (Number 1)))
(True (SetValue (Concept "abc") (Predicate "key")
(ExecutionOutput (GroundedSchema "scm: create") (List))))
(True (Sleep (Number 1)))
(True (SetValue (Concept "abc") (Predicate "key")
(ExecutionOutput (GroundedSchema "scm: create") (List))))
)
(SequentialAnd
(Evaluation (GroundedPredicate "scm: prt")
(List (StreamValueOf (Concept "abc") (Predicate "key"))))
)))