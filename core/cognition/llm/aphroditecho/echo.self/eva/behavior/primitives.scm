(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp))
(use-modules (opencog logger))
(use-modules (opencog eva-model))
(define eva-logger (eva-get-logger))
(define-public (print-msg node)
(cog-logger-info eva-logger "~a\n" (cog-name node))
(stv 1 1))
(define-public (print-msg-face node)
(cog-logger-info eva-logger "~a with face id: ~a"
(cog-name node)
(cog-name (cog-outgoing-atom (cog-execute!
(DefinedSchemaNode "Current interaction target")) 0)))
(stv 1 1))
(define-public (print-msg-time node time)
(cog-logger-info eva-logger "~a Elapsed: ~a seconds"
(cog-name node)
(cog-name time))
(stv 1 1))
(define (change-template pred-name ts-name min-name max-name)
(define get-ts (string-append "get " ts-name " timestamp"))
(define prev-ts (string-append "previous-" ts-name "-call"))
(define delta-ts (string-append "delta-" ts-name "-time"))
(DefineLink
(DefinedPredicate pred-name)
(SequentialOr
(GreaterThan
(Minus (TimeLink) (DefinedSchema get-ts))
(Get
(TypedVariable (Variable "$max") (Type "NumberNode"))
(State (Schema max-name) (Variable "$max")))
)
(SequentialAnd
(True (Put (State (Schema delta-ts) (Variable "$x"))
(Minus (TimeLink)
(Get
(TypedVariable
(Variable "$p") (Type "NumberNode"))
(State (Schema prev-ts) (Variable "$p"))))))
(True (Put (State (Schema prev-ts) (Variable "$x")) (TimeLink)))
(GreaterThan
(Minus (TimeLink) (DefinedSchema get-ts))
(Get
(TypedVariable (Variable "$min") (Type "NumberNode"))
(State (Schema min-name) (Variable "$min")))
)
(GreaterThan
(Get
(TypedVariable (Variable "$delta") (Type "NumberNode"))
(State
(Schema delta-ts) (Variable "$delta")))
(RandomNumber
(Number 0)
(Minus
(Get
(TypedVariable
(Variable "$max") (Type "NumberNode"))
(State (Schema max-name) (Variable "$max")))
(Get
(TypedVariable
(Variable "$min") (Type "NumberNode"))
(State (Schema min-name) (Variable "$min")))))
)
)
)))
(change-template "Time to change interaction" "interaction"
"time_to_change_face_target_min" "time_to_change_face_target_max")
(change-template "Time to wake up" "sleep"
"time_sleeping_min" "time_sleeping_max")
(change-template "Bored too long" "bored"
"time_boredom_min" "time_boredom_max")
(change-template "Silent too long" "heard-something"
"silence_min" "silence_max")
(change-template "Time to change expression" "expression"
"time_since_last_expr_min" "time_since_last_expr_max")
(change-template "Time to make gesture" "gesture"
"time_since_last_gesture_min" "time_since_last_gesture_max")
(change-template "Time to change gaze" "attn-search"
"time_search_attn_min" "time_search_attn_max")
(change-template "Time to reset glance" "glance"
"time_search_glance_min" "time_search_glance_max")
(define-public (run-behavior-tree-gc)
(define (free-stuff)
(sleep 1)
(cog-map-type (lambda (a) (cog-extract! a) #f) 'SetLink)
(cog-map-type (lambda (a) (cog-extract! a) #f) 'ListLink)
(cog-map-type (lambda (a) (cog-extract! a) #f) 'NumberNode)
(cog-map-type (lambda (a) (cog-extract! a) #f) 'ConceptNode)
(free-stuff)
)
(call-with-new-thread free-stuff)
)