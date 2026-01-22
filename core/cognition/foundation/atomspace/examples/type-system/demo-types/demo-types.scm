(define-module (opencog demo-types))
(use-modules (opencog))
(use-modules (opencog chemodemo-config))
(load-extension
	(string-append opencog-ext-path-chemodemo "libchem-demo-types")
	"chem_types_init")
(include-from-path "opencog/demo-types/chem_types.scm")