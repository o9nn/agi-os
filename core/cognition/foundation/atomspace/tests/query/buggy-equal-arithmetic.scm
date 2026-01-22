(use-modules (opencog) (opencog exec))
(Number 1)
(Number 2)
(Number 3)
(Number 4)
(Number 5)
(Number 6)
(define arithmetic-search
(GetLink
(TypedVariable (Variable "$X1") (Type 'NumberNode))
(Equal (Plus (Variable "$X1") (Number 5)) (Number 11))))