(define-module (opencog unify))
(use-modules (opencog))
(use-modules (opencog unify-config))
(load-extension
   (string-append opencog-ext-path-unify "libunify-types")
   "unify_types_init")
(load-extension
   (string-append opencog-ext-path-unify-atoms "libunify-atoms")
   "opencog_unify_atoms_init")
(load-from-path "opencog/unify/types/unify_types.scm")