xxxxxxxx
under construction
(define-public (compute-goe-similarity LLOBJ)
"
  compute-goe-similarity LLOBJ -- provide methods for working with
  Gaussian Orthogonal vectors.
  LLOBJ
"
	(define sob (add-pair-stars LLOBJ))
	(define (add-mi-sim LLOBJ IDX)
		(define (get-ref PR IDX)
			(define flov (LLOBJ 'get-count PR))
			(if flov (cog-value-ref flov IDX) -inf.0))
		(lambda (message . args)
			(case message
				((get-count)  (get-ref (car args) IDX))
				(else      (apply LLOBJ (cons message args))))
		))
	(define ami (add-mi-sim sob 0))
	(define goe (add-gaussian-ortho-api ami))
	(goe 'mean-rms)
	(define gos (add-similarity-api ami #f "goe"))
	(define goec (add-similarity-compute goe))
	(define (do-compute A B)
		(define simc (goec 'left-cosine A B))
		(format #t "cos=~7F for (\"~A\", \"~A\")\n"
			simc (cog-name A) (cog-name B))
		(store-atom
			(gos 'set-pair-similarity
				(gos 'make-pair A B)
				(FloatValue simc))))
	(define (dot-prod A B)
		(define have-it (gos 'pair-count A B))
		(if (not have-it) (do-compute A B)))
	(define (redo-mi-sims WRDLIST)
	(define touched-words (recompute-marginals LLOBJ (cons wclass in-grp)))
	(format #t "------ Recomputed MMT marginals in ~A secs\n" (e))
	(recomp-all-sim SIM-API compute-sim touched-words)
		(for-each (lambda (WC) (simmer wclass WC))
			(LLOBJ 'get-clusters))
)
(define allwo (rank-words pcs))
(loop-upper-diagonal dot-prod allwo 0 250)
#! ========
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))
(smi 'fetch-pairs)
==== !#