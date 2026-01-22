(use-modules (opencog) (opencog exec))
(PlusLink (Number 1) (Number 2) (Number 10))
(define quote-glob
(Query
(TypedVariable (Glob "$op") (Type 'NumberNode))
(Plus (Glob "$op") (Number 10))
(Quote (Plus (Unquote (Glob "$op")) (Number 10)))))
(define expect
(PlusLink (Number 1) (Number 2) (Number 10)))