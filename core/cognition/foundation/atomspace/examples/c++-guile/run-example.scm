(add-to-load-path "../examples/c++-guile")
%load-path
(use-modules (opencog))
(use-modules (opencog example))
(hey-print (Concept "a"))
(hey-printmore (Concept "a"))
(define b (Concept "bbb" (SimpleTruthValue 0.6 0.8)))
(hey-printmore b)