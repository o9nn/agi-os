#! /usr/bin/env -S guile
!#
(load "cogserver.scm")
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))
(define trm (add-trimmer asa))
(batch-pairs asa)
(barrier storage-node)
(trm 'subtotal-trim asa 10 10 4)
(set! ala (make-any-link-api))
(set! asa (add-pair-stars ala))
(batch-all-pair-mi asa)
(barrier storage-node)