(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
(define pta (add-transpose-api psa))
(define pco (add-pair-cosine-compute pta))
(define bco
(batch-similarity pta #f "pseudo-cset Cosine-*" 0.0
(lambda (wa wb) (pco 'right-cosine wa wb))))
(define minus-inf (- 0 (inf)))
(define pcam (make-pseudo-cset-api))
(define psam (add-pair-stars pcam))
(define ptam (add-transpose-api psam))
(define pmi (add-symmetric-mi-compute psam))
(define bmi
(batch-similarity ptam #f "pseudo-cset MI-*" minus-inf
(lambda (wa wb) (pmi 'mmt-fmi wa wb))))
(map (lambda (n) (bmi 'batch-compute n)) (iota 10 40 25))
(define cra (make-shape-vec-api))
(define crs (add-pair-stars cra))
(define crt (add-transpose-api crs))
(define cco (add-pair-cosine-compute crt))
(define bcr
(batch-similarity crt #f "Cross Cosine-*" 0.0
(lambda (wa wb) (cco 'right-cosine wa wb))))
(define cram (make-shape-vec-api))
(define crsm (add-pair-stars cram))
(define crtm (add-transpose-api crsm))
(define cmi (add-symmetric-mi-compute crsm))
(define mib
(batch-similarity crtm #f "Shape MI-*" minus-inf
(lambda (wa wb) (cmi 'mmt-fmi wa wb))))
(map (lambda (n) (mib 'batch-compute n)) (iota 4 500 2))
(cog-count-atoms 'SimilarityLink)
(define (store-sims)
(cog-map-type (lambda (atm) (store-atom atm) #f) 'SimilarityLink))
(define (store-regularly)
(sleep 1200)
(sleep 1200)
(sleep 1200)
(store-sims)
(format #t "Done storing ~A ~A\n"
(cog-count-atoms 'SimilarityLink)
(strftime "%c" (localtime (current-time))))
(load-atoms-of-type 'SimilarityLink)
(store-regularly))