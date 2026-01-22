(use-modules (opencog))
(use-modules (opencog logger))
(use-modules (opencog eva-model))
(use-modules (opencog cogserver))
(start-cogserver)
(use-modules (opencog openpsi))
(use-modules (opencog eva-behavior))
(load-sophia-config)
(use-modules (opencog movement))
(start-ros-movement-node)
(define (run) (psi-run-per-demand))
(define (halt) (psi-halt))
(use-modules (opencog nlp))
(use-modules (opencog nlp chatbot))
(use-modules (opencog nlp relex2logic))
(define-public (dispatch-text TXT-ATOM)
"
  dispatch-text TXT-ATOM
  Pass the TXT-ATOM that STT heard into the OpenCog chatbot.
"
   (call-with-new-thread
      (lambda () (chat (cog-name TXT-ATOM)))
   )
   (stv 1 1)
)
(define (configure-loggers LOG-LEVEL)
"
  configure-loggers LOG-LEVEL
  Set the loggers to the same level and separate their logs between runs.
  For each call of this function, the logs are created in
  /tmp/<current-filename>/<module-name>-Year-Month-Day-Hour-Minute-Second.log
"
  (define log-dir (format #f "/tmp/~a" (basename (current-filename))))
  (define z-time (strftime "%F-%H-%M-%S" (localtime (current-time))))
  (define (configure-logger logger name)
    (cog-logger-set-level! logger LOG-LEVEL)
    (cog-logger-set-stdout! logger #f)
    (cog-logger-set-filename! logger
      (format #f "~a/~a-~a.log" log-dir name z-time)))
  (if (not (file-exists? log-dir)) (mkdir log-dir))
  (configure-logger (psi-get-logger) "openpsi")
  (configure-logger (eva-get-logger) "eva")
  (let ((oc-log-file (format #f "~a/opencog-~a.log" log-dir z-time)))
    (cog-logger-set-level! LOG-LEVEL)
    (cog-logger-set-filename! oc-log-file)
    (cog-logger-set-stdout! #f)
  )
)
(configure-loggers "debug")
*unspecified*