(DefineLink
(DefinedPredicate "main loop")
(SatisfactionLink
(SequentialAnd
(SequentialOr
(DefinedPredicate "Skip Interaction?")
(SequentialAnd
(DefinedPredicate "Did Someone New Speak?")
(DefinedPredicate "Request interaction with person who spoke"))
(SequentialAnd
(DefinedPredicate "Someone requests interaction?")
(DefinedPredicate "Interaction requested action"))
(SequentialAnd
(DefinedPredicate "Did someone arrive?")
(DefinedPredicate "New arrival sequence"))
(SequentialAnd
(DefinedPredicate "Did someone leave?")
(DefinedPredicate "Someone left action"))
(SequentialAnd
(DefinedPredicate "Someone visible?")
(DefinedPredicate "Interact with people"))
(DefinedPredicate "Nothing is happening")
(True))
(SequentialOr
(SequentialAnd
(DefinedPredicate "chatbot started talking?")
(DefinedPredicate "Speech started"))
(SequentialAnd
(DefinedPredicate "chatbot is talking?")
(DefinedPredicate "Speech ongoing"))
(SequentialAnd
(DefinedPredicate "chatbot stopped talking?")
(DefinedPredicate "Speech ended"))
(SequentialAnd
(DefinedPredicate "chatbot started listening?")
(DefinedPredicate "Listening started"))
(SequentialAnd
(DefinedPredicate "chatbot is listening?")
(DefinedPredicate "Listening ongoing"))
(SequentialAnd
(DefinedPredicate "chatbot stopped listening?")
(DefinedPredicate "Listening ended"))
(SequentialAnd
(DefinedPredicate "Skip Interaction?")
(DefinedPredicate "Keep alive"))
(True)
)
(DefinedPredicate "Continue running loop?")
(DefinedPredicate "ROS is running?")
(DefinedPredicate "main loop")
)))
(define do-run-loop #t)
(define-public (behavior-tree-run)
"
behavior-tree-run
Run the Eva behavior tree main loop (in a new thread),
Call (behavior-tree-halt) to exit the loop.
"
(set! do-run-loop #t)
(call-with-new-thread
(lambda () (cog-evaluate! (DefinedPredicateNode "main loop")))))
(define-public (behavior-tree-halt)
"
behavior-tree-halt
Tell the Eva behavior tree main loop thread to exit.
"
(set! do-run-loop #f))
(define-public (behavior-tree-running?)
"
behavior-tree-running?
Return #t if the behavior tree is running, else return false.
"
do-run-loop)
(define-public (behavior-tree-loop-count)
"
behavior-tree-loop-count
Return the loop-count of the behavior tree.
"
loop-count)
(define loop-count 0)
(define-public (continue-running-loop)
(set! loop-count (+ loop-count 1))
(usleep 101000)
(if do-run-loop (stv 1 1) (stv 0 1)))
(DefineLink
(DefinedPredicate "Continue running loop?")
(Evaluation
(GroundedPredicate "scm:continue-running-loop") (ListLink)))