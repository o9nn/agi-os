#! /usr/bin/env -S guile
!#
(load "cogserver.scm")
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(display "Fetch all sections. This may take a long while!\n")
(csc 'fetch-pairs)
(display "Create cross-sections. This may take a long while!\n")
(csc 'explode-sections)
(define btr (batch-transpose csc))
(btr 'clobber)
(btr 'mmt-marginals)
(print-matrix-summary-report csc)
(barrier storage-node)