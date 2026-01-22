(use-modules (opencog) (opencog exec))
(PlusLink (Number "1") (Number "2") (Number "3") (Number "4"))
(define glob-for-plus
(BindLink
(PlusLink
(Number "1") (Glob "$star") (Number "4"))
(PlusLink
(Number "1") (Glob "$star") (Number "4") (Number "5"))))
(PlusLink (Number 3) (Number 10))
(define glob-for-ten
(BindLink
(Plus (Glob "$op") (Number 10))
(Times (Plus (Glob "$op") (Number 10)) (Number 30))))