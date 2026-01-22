(define intensional-inheritance-direct-introduction-rule
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
(GroundedSchema "scm: intensional-inheritance-direct-introduction")
(List
(IntensionalInheritance A B)
A
B)))))
(define (intensional-inheritance-direct-introduction conclusion . premises)
(define (get-attractions A)
(let* ((at-links (cog-filter 'AttractionLink (cog-incoming-set A)))
(A-at? (lambda (x) (equal? A (gar x)))))
(filter A-at? at-links)))
(define (numerator A B-ats)
(define (fuzzy-intersect B-at)
(let* ((pat (gdr B-at))
(A-at (cog-link 'AttractionLink A pat)))
(if (null? A-at)
0
(min (cog-mean A-at) (cog-mean B-at)))))
(fold + 0 (map fuzzy-intersect B-ats)))
(define (denominator B-ats)
(fold + 0 (map cog-mean B-ats)))
(if (= (length premises) 2)
(let* ((IntInh conclusion)
(A (car premises))
(B (cadr premises))
(B-ats (get-attractions B))
(dnt (denominator B-ats))
(TVs (if (< 0 dnt) (/ (numerator A B-ats) dnt) 1))
(TVc (count->confidence (length B-ats)))
(TV (stv TVs TVc)))
(if (< 0 TVc) (cog-merge-hi-conf-tv! IntInh TV)))))
(define intensional-inheritance-direct-introduction-rule-name
(DefinedSchemaNode "intensional-inheritance-direct-introduction-rule"))
(DefineLink intensional-inheritance-direct-introduction-rule-name
intensional-inheritance-direct-introduction-rule)