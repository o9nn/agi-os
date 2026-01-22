(define-module (opencog bioscience))
(use-modules (opencog))
(use-modules (opencog bio-config))
(load-extension
	(string-append opencog-ext-path-bio "libbioscience-types")
	"bioscience_types_init")
(load-from-path "opencog/bioscience/types/bioscience_types.scm")