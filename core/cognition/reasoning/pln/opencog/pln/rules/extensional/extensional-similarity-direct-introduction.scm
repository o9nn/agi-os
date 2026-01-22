(load "extensional-utils.scm")
(define extensional-similarity-direct-introduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (CT (Type "ConceptNode")))
    (Bind
      (VariableSet
        (TypedVariable A CT)
        (TypedVariable B CT))
      (Present
        A
        B)
      (ExecutionOutput
        (GroundedSchema "scm: extensional-similarity-direct-introduction")
        (List
          (ExtensionalSimilarity A B)
          A
          B)))))
(define (get-membership-degree A C)
  (let* ((mbr-lnk (cog-link 'MemberLink A C)))
    (if (null? mbr-lnk)
	0
	(cog-mean mbr-lnk))))
(define (ext-sim-numerator A B-mbr-lnks)
  (define (fuzzy-intersect B-mbr-lnk)
    (let* ((mbr (gar B-mbr-lnk)))
      (min (get-membership-degree mbr A) (cog-mean B-mbr-lnk))))
  (fold + 0 (map fuzzy-intersect B-mbr-lnks)))
(define (ext-sim-denominator A B mbrs)
  (define (fuzzy-union mbr)
    (let ((A-mbr-degree (get-membership-degree mbr A))
	  (B-mbr-degree (get-membership-degree mbr B)))
      (max A-mbr-degree B-mbr-degree)))
  (fold + 0 (map fuzzy-union mbrs)))
(define (ext-sim-evidence->tv A B)
  (cog-logger-debug "(ext-sim-evidence->tv A=~a B=~a)" A B)
  (let* ((A-mbr-lnks (get-member-links-of A))
	 (B-mbr-lnks (get-member-links-of B))
	 (A-mbrs (map gar A-mbr-lnks))
	 (B-mbrs (map gar B-mbr-lnks))
	 (mbrs (lset-union equal? A-mbrs B-mbrs))
	 (dnt (ext-sim-denominator A B mbrs))
	 (tv-strength (if (< 0 dnt) (/ (ext-sim-numerator A B-mbr-lnks) dnt) 1))
	 (tv-confidence (count->confidence (length B-mbrs)))
	 (tv (stv tv-strength tv-confidence)))
    tv))
(define (extensional-similarity-direct-introduction conclusion . premises)
  (if (= (length premises) 2)
      (let* ((Sim conclusion)
             (A (car premises))
             (B (cadr premises))
             (tv (ext-sim-evidence->tv A B)))
        (cog-merge-hi-conf-tv! Sim tv))))
(define extensional-similarity-direct-introduction-rule-name
  (DefinedSchemaNode "extensional-similarity-direct-introduction-rule"))
(DefineLink extensional-similarity-direct-introduction-rule-name
  extensional-similarity-direct-introduction-rule)