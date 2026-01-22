(define (substitute term substitution-pairs)
    (define num-pairs)
    (define subst-map)
    (set! num-pairs (length substitution-pairs))
    (set! subst-map (make-hash-table num-pairs))
    (for-each (lambda(map pair) (hash-set! map (car pair) (cdr pair)))
              (make-list num-pairs subst-map) substitution-pairs
    )
    (substitute-with-map term subst-map)
)
(define (substitute-with-map term subst-map)
    (define outgoing #f)
    (cond
          ((hash-ref subst-map term) (begin
                (set! outgoing (hash-ref subst-map term))
          ))
          ((> (length (cog-outgoing-set term)) 0) (begin
             (let ((subterms (map substitute-with-map (cog-outgoing-set term)
                        (make-list (length (cog-outgoing-set term)) subst-map))))
                    (if (eq? (cog-type term) 'VariableList)
                        (begin
                            (set! subterms (filter (lambda(x) (eq? (cog-type x) 'VariableNode))
                                            subterms))
                        ))
                    (if (and (eq? (cog-type term) 'VariableList)
                            (null-list? subterms))
                        (set! outgoing #nil)
                        (set! outgoing (cog-new-link (cog-type term) subterms)))
             )
          ))
          ((= (length (cog-outgoing-set term)) 0) (begin
                (set! outgoing term)
           ))
    )
    outgoing
)