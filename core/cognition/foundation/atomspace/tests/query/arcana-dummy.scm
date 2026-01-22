(use-modules (opencog))
(use-modules (opencog exec))
(Number 42)
(Number 3003)
(define dummy
(GetLink
(TypedVariable (Variable "$x") (Type "NumberNode"))
(GreaterThan (Number 88) (Variable "$x"))))