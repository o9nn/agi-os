#! /usr/bin/env -S guile
!#
(load "cogserver.scm")
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))
(batch-pairs asa)
(barrier storage-node)