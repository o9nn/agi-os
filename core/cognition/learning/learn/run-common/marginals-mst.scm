#! /usr/bin/env -S guile
!#
(load "cogserver.scm")
(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
(define btr (batch-transpose psa))
(display "Fetch all disjuncts. This may take well over half-an-hour!\n")
(psa 'fetch-pairs)
(btr 'clobber)
(btr 'mmt-marginals)
(print-matrix-summary-report psa)
(barrier storage-node)