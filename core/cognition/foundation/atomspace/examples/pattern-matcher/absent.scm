(use-modules (opencog) (opencog exec))
(define query
(Evaluation
(Predicate "visibility")
(List (Variable "$x"))))
(define golem
(Put query (Concept "item 42")))
(define destroy
(Bind query (DeleteLink query))
)
(define create
(Bind (Absent query) golem)
)
(define room-state (Anchor "Room State"))
(define room-empty (Concept "room empty"))
(define room-nonempty (Concept "room nonempty"))
(State room-state room-empty)
(define is-visible
(Bind
query
(Put (State room-state (Variable "$x")) room-nonempty)
)
)
(define is-invisible
(Bind
(Absent query)
(Put (State room-state (Variable "$x")) room-empty)
)
)
(define (show-room-state)
(car (cog-chase-link 'StateLink 'ConceptNode room-state)))
(show-room-state)
(cog-execute! create)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)
(cog-execute! destroy)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)
(cog-execute! create)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)
(cog-execute! destroy)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)