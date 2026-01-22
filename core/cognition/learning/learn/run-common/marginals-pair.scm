#! /usr/bin/env -S guile -l ./marginals-pair.scm --
!#
(load "cogserver.scm")
(define ala (make-any-link-api))
(define aca (add-count-api ala))
(define asa (add-pair-stars aca))
(display "Start loading sparse matrix pairs\n")
(asa 'fetch-pairs)
(display "Finished loading sparse matrix pairs\n")
(cog-report-counts)
(batch-all-pair-mi asa)
(print-matrix-summary-report asa)
(barrier storage-node)