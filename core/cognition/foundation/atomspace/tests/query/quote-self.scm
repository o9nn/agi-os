(use-modules (opencog) (opencog exec))
(define self-ground
(Query
(Plus (Glob "$op") (Number 10))
(Quote (Plus (Unquote (Glob "$op")) (Number 10)))))
(define expect
(Plus (Unquote (Glob "$op")) (Number 10)))