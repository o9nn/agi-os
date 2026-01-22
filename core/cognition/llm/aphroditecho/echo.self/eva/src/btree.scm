(use-modules (opencog))
(load "time-map.scm")
(use-modules (opencog cogserver))
(start-cogserver "../scripts/opencog.conf")
(use-modules (opencog eva-behavior))
(add-to-load-path "../src")
(load-from-path "cfg-sophia.scm")
(load-from-path "old-tree.scm")
(define (run) (behavior-tree-run))
(define (halt) (behavior-tree-halt))
(define-public (dispatch-text TXT-ATOM)
"
dispatch-text TXT-ATOM
Pass the TXT-ATOM that STT heard into the OpenCog chatbot.
"
(stv 1 1)
)
(run-behavior-tree-gc)
(TrueLink)
(all-threads)
(run)