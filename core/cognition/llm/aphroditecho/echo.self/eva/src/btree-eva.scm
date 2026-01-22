(use-modules (opencog))
(use-modules (opencog cogserver))
(start-cogserver "../scripts/opencog.conf")
(use-modules (opencog eva-behavior))
(use-modules (opencog openpsi))
(use-modules (opencog movement))
(start-ros-movement-node)
(add-to-load-path "../src")
(load-from-path "cfg-eva.scm")
(load-from-path "psi-behavior.scm")
(define (run) (psi-run-per-demand))
(define (halt) (psi-halt))
(use-modules (opencog nlp))
(use-modules (opencog nlp chatbot-eva))
(use-modules (opencog nlp relex2logic))
(define-public (dispatch-text TXT-ATOM)
"
  dispatch-text TXT-ATOM
  Pass the TXT-ATOM that STT heard into the OpenCog chatbot.
"
   (call-with-new-thread
      (lambda () (grounded-talk "luser" (cog-name TXT-ATOM)))
   )
   (stv 1 1)
)
(run-behavior-tree-gc)
*unspecified*
(all-threads)
(run)