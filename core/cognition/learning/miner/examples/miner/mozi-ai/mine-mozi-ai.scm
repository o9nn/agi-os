(define kb-filename "mozi-ai-sample.scm")
(define min-freq 0.01)
(define max-iter 100)
(define max-cnjs 3)
(define max-vars 2)
(define rand-seed 0)
(use-modules (srfi srfi-1))
(use-modules (opencog randgen))
(use-modules (opencog logger))
(use-modules (opencog ure))
(use-modules (opencog miner))
(use-modules (opencog bioscience))
(load "mozi-ai-utils.scm")
(cog-randgen-set-seed! rand-seed)
(define log-filename (string-append
"opencog-"
(rm-extension kb-filename "scm")
"-s" (number->string rand-seed)
"-f" (number->string min-freq)
"-i" (number->string max-iter)
"-c" (number->string max-cnjs)
"-v" (number->string max-vars)
".log"))
(cog-logger-set-level! "debug")
(cog-logger-set-filename! log-filename)
(ure-logger-set-level! "debug")
(ure-logger-set-filename! log-filename)
(define (run-mozi-ai-miner)
(let* (
(db-lst (load-preprocess (string-append "kbs/" kb-filename)))
(msg (cog-logger-fine "db-lst size = ~a" (length db-lst)))
(db-cpt (fill-db-cpt (Concept kb-filename) db-lst))
(msg (cog-logger-info "Run pattern miner over ~a" kb-filename))
(msg (cog-logger-info (string-append "With parameters:\n"
"random-seed = ~a\n"
"min-frequency = ~a\n"
"max-iterations = ~a\n"
"max-conjuncts = ~a\n"
"max-variables = ~a")
rand-seed min-freq max-iter max-cnjs max-vars))
(results (cog-mine db-cpt
#:minfreq min-freq
#:maximum-iterations max-iter
#:conjunction-expansion #t
#:max-conjuncts max-cnjs
#:max-variables max-vars
#:surprisingness 'nisurp))
(pp-results (postprocess results max-cnjs))
(msg (cog-logger-info "Results from mining ~a:\nsize = ~a\n~a"
kb-filename (length pp-results) pp-results)))
pp-results))
(define results (run-mozi-ai-miner))