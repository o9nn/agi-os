(use-modules (opencog) (opencog exec))
(define my-trans (Concept "My Chain's Transition Rule"))
(define my-state (Anchor "My Chain's Current State"))
(define my-nexts (Anchor "My Chain's Next State"))
(define (reset-state)
(List (stv 1 1) my-state (Concept "initial state"))
(List (stv 0 1) my-state (Concept "green"))
(List (stv 0 1) my-state (Concept "yellow"))
(List (stv 0 1) my-state (Concept "red"))
)
(reset-state)
(ContextLink (stv 0.9 1)
(Concept "initial state")
(List my-trans (Concept "green")))
(ContextLink (stv 0.1 1)
(Concept "initial state")
(List my-trans (Concept "yellow")))
(ContextLink (stv 0.0 1)
(Concept "initial state")
(List my-trans (Concept "initial state")))
(ContextLink (stv 0.9 1)
(Concept "green")
(List my-trans (Concept "yellow")))
(ContextLink (stv 0.1 1)
(Concept "green")
(List my-trans (Concept "red")))
(ContextLink (stv 0.9 1)
(Concept "yellow")
(List my-trans (Concept "red")))
(ContextLink (stv 0.1 1)
(Concept "yellow")
(List my-trans (Concept "green")))
(ContextLink (stv 0.9 1)
(Concept "red")
(List my-trans (Concept "green")))
(ContextLink (stv 0.1 1)
(Concept "red")
(List my-trans (Concept "red")))
(define (create-chain-stepper chain-name chain-next chain-state)
(define curr-state
(List
chain-state
(Variable "$curr-state")
)
)
(define state-trans
(ContextLink
(Variable "$curr-state")
(List
chain-name
(Variable "$next-state")
)
)
)
(define next-state
(List
chain-next
(Variable "$next-state")
)
)
(Bind
(VariableList
(Variable "$curr-state")
(Variable "$next-state")
)
(And
curr-state
state-trans
)
(ExecutionOutput
(GroundedSchema "scm: accum-probability")
(List
next-state
state-trans
curr-state
)
)
)
)
(define (get-prob atom) (cog-mean atom))
(define (is-default-tv? atom)
(not (< 0.5 (cog-confidence atom))))
(define (set-prob atom value)
(cog-set-tv! atom (cog-new-stv value 1.0)))
(define (accum-prob atom value)
(if (is-default-tv? atom)
(set-prob atom value)
(set-prob atom (+ (get-prob atom) value))))
(define (accum-probability PB PBA PA)
(accum-prob PB (* (get-prob PBA) (get-prob PA)))
PB
)
(define (create-chain-deleter chain-state)
(Bind
(Variable "$state")
(List chain-state (Variable "$state"))
(Delete
(List chain-state (Variable "$state"))
)
)
)
(define (copy-tv b a)
(begin (cog-set-tv! b (cog-tv a)) b))
(define (create-chain-copier chain-to chain-from)
(Bind
(Variable "$state")
(List
chain-from
(Variable "$state")
)
(ExecutionOutput
(GroundedSchema "scm:copy-tv")
(List
(List chain-to (Variable "$state"))
(List chain-from (Variable "$state"))
)
)
)
)
(define (create-chain-move chain-to chain-from)
(Bind
(TypedVariable
(Variable "$state")
(Type "Concept")
)
(List
chain-from
(Variable "$state")
)
(List
(ExecutionOutput
(GroundedSchema "scm:copy-tv")
(List
(List chain-to (Variable "$state"))
(List chain-from (Variable "$state"))
)
)
(Delete
(List chain-from (Variable "$state"))
)
)
)
)
(define (show-state state-vect)
(define (get-tv atom) (cog-mean (List state-vect atom)))
(format #t "State vector for \"~A\"\n" (cog-name state-vect))
(format #t "Initial state: ~A\n" (get-tv (Concept "initial state")))
(format #t "Green state: ~A\n" (get-tv (Concept "green")))
(format #t "Yellow state: ~A\n" (get-tv (Concept "yellow")))
(format #t "Red state: ~A\n" (get-tv (Concept "red")))
*unspecified*
)
(define (take-a-step)
(define my-stepper (create-chain-stepper my-trans my-nexts my-state))
(define my-delter (create-chain-deleter my-state))
(define my-mover (create-chain-move my-state my-nexts))
(cog-execute! my-stepper)
(cog-execute! my-delter)
(cog-execute! my-mover)
(show-state my-state)
)