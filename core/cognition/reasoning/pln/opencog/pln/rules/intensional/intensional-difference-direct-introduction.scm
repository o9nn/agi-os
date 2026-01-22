(define intensional-difference-direct-introduction-rule
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
(GroundedSchema "scm: intensional-difference-direct-introduction")
(List
(IntensionalDifference A B)
A
B)))))
(define (intensional-difference-direct-introduction conclusion . premises)
(define (get-attractions A)
(let* ((at-links (cog-filter 'AttractionLink (cog-incoming-set A)))
(A-at? (lambda (x) (equal? A (gar x)))))
(filter A-at? at-links)))
(define (get-pattern-strength A pat)
(let* ((A-at (cog-link 'AttractionLink A pat)))
(if (null? A-at) 0 (* (cog-mean A-at) (cog-confidence A-at)))))
(define (numerator A-ats B)
(define (fuzzy-difference A-at)
(let* ((pat (gdr A-at)))
(min (cog-mean A-at) (- 1 (get-pattern-strength B pat)))))
(fold + 0 (map fuzzy-difference A-ats)))
(if (= (length premises) 2)
(let* ((IntInh conclusion)
(A (car premises))
(B (cadr premises))
(A-ats (get-attractions A))
(B-ats (get-attractions B))
(A-pats (map gdr A-ats))
(all-cpts (cog-get-atoms 'ConceptNode #t))
(usize (length all-cpts))
(dnt usize)
(TVs (if (< 0 dnt) (/ (numerator A-ats B) dnt) 1))
(TVc (count->confidence dnt))
(TV (stv TVs TVc)))
(if (< 0 TVc) (cog-merge-hi-conf-tv! IntInh TV)))))
(define intensional-difference-direct-introduction-rule-name
(DefinedSchemaNode "intensional-difference-direct-introduction-rule"))
(DefineLink intensional-difference-direct-introduction-rule-name
intensional-difference-direct-introduction-rule)