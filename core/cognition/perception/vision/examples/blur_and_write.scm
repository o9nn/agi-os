#!/usr/bin/env guile
!#
(use-modules (opencog) (opencog exec) (opencog vision))
(define img-node (Image "example_image.png"))
(define img-collection (Concept "Image Collection"))
(define key-1 (Concept "image#1"))
(define img-blur (ImageBlur img-node (Number "10")))
(cog-set-value! img-collection key-1 (cog-execute! img-blur))
(define img-write (ImageWrite (ValueOf img-collection key-1) (Concept "example_output.png")))
(cog-execute! img-write)