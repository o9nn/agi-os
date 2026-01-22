(define intensional-similarity-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (X (Variable "$X"))
         (CT (TypeInh "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT))
      (And
        (Present
          A
          B)
        (Satisfaction
          (TypedVariable X CT)
          (Present
            (Attraction A X)
            (Attraction B X)))
        (Not (Identical A B)))
      (ExecutionOutput
        (GroundedSchema "scm: intensional-similarity-direct-introduction")
        (List
          (IntensionalSimilarity A B)
          (Set
            A
            B))))))
(define (intensional-similarity-direct-introduction conclusion . premises)
  (define (get-attractions A)
    (let* ((at-links (cog-filter 'AttractionLink (cog-incoming-set A)))
           (A-at? (lambda (x) (equal? A (gar x)))))
      (filter A-at? at-links)))
  (define (get-pattern-strength A pat)
    (let* ((A-at (cog-link 'AttractionLink A pat)))      
      (if (null? A-at) 0 (* (cog-mean A-at) (cog-confidence A-at)))))
  (define (numerator A B-ats)
    (define (fuzzy-intersect B-at)
      (let* ((pat (gdr B-at)))
        (min (get-pattern-strength A pat) (cog-mean B-at))))
    (fold + 0 (map fuzzy-intersect B-ats)))
  (define (denominator A B pats)
    (define (fuzzy-union pat)
      (let ((A-pat-strength (get-pattern-strength A pat))
            (B-pat-strength (get-pattern-strength B pat)))
        (max A-pat-strength B-pat-strength)))
    (fold + 0 (map fuzzy-union pats)))
  (if (= (length premises) 1)
      (let* ((IntInh conclusion)
             (A (gar (car premises)))
             (B (gdr (car premises)))
             (A-ats (get-attractions A))
             (B-ats (get-attractions B))
             (A-pats (map gdr A-ats))
             (B-pats (map gdr B-ats))
             (pats (lset-union equal? A-pats B-pats))
             (dnt (denominator A B pats))
             (TVs (if (< 0 dnt) (/ (numerator A B-ats) dnt) 1))
             (TVc (count->confidence (length B-ats)))
             (TV (stv TVs TVc)))
        (if (< 0 TVc) (cog-merge-hi-conf-tv! IntInh TV)))))
(define intensional-similarity-direct-introduction-rule-name
  (DefinedSchemaNode "intensional-similarity-direct-introduction-rule"))
(DefineLink intensional-similarity-direct-introduction-rule-name
  intensional-similarity-direct-introduction-rule)