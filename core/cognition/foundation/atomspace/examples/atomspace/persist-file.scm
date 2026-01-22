(use-modules (ice-9 readline))
(activate-readline)
(use-modules (opencog))
(Concept "asdf" (stv 0.42 0.24))
(List (Concept "cat") (Concept "dog"))
(cog-prt-atomspace)
(export-all-atoms "/tmp/x.scm")
(clear)
(cog-prt-atomspace)
(load "/tmp/x.scm")
(cog-prt-atomspace)
(primitive-load "/tmp/x.scm")
(use-modules (opencog persist-file))
(load-file "/tmp/x.scm")
,d export-all-atoms
,describe export-atoms
,des prt-atom-list