(use-modules (opencog) (opencog exec))
(define green-light  (Concept "green light"))
(define red-light  (Concept "red light"))
(define num-green 0)
(define num-red 0)
(define (stop-go atom)
(format #t "Got called with this: ~A\n" (cog-name atom))
(cond
((equal? atom green-light) (begin (set! num-green (+ 1 num-green)) (stv 1 1)))
((equal? atom red-light) (begin (set! num-red (+ 1 num-red)) (stv 0 1)))
(else (throw 'not-a-stoplight "stop-go" "you're busted"))
)
)
(define off-road
(Satisfaction
(VariableList)
(SequentialAnd
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List (Concept "corn field"))))))
(define traffic-lights
(Satisfaction
(VariableList)
(SequentialAnd
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List green-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List green-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List red-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List (Concept "traffic ticket"))))))
(define (start-again)
(cog-evaluate! traffic-lights)
(format #t "Have seen ~A green lights and ~A  red lights\n"
num-green num-red))
(define hot-rodding
(Satisfaction
(VariableList)
(SequentialOr
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List red-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List red-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List red-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List green-light))
(Evaluation
(GroundedPredicateNode "scm: stop-go")
(List (Concept ".... And they're off!"))))))
(define (drag-race)
(cog-evaluate! hot-rodding)
(simple-format #t "Waited on ~A red lights\n" num-red))