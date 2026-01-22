(define (display-var name value)
    (newline)(display name)(display ":\n")(display value))
(define (display-atom label atom)
    (newline)(display label)(display ": \n")(display atom))
(define (display-label label value)
    (newline)(display label)(display ": ")(display value)(newline))
(define (incoming atom)
    (cog-incoming-set atom))
(define (make-overexpression-predicate gene)
    (ExecutionOutputLink
        (GroundedSchemaNode "scm: make-overexpression-predicate")
        (ListLink
            gene) (stv .2 .7))
)
#!
       the inference you're building. The curried form has the advantage that it
       hides away the universal quantification, cause then you can use the
       higher order version of the ImplicationLink (i.e. over predicates rather
       than TVs).
!#
(define (make-contains-significant-variant-predicate gene)
    (ExecutionOutputLink
        (GroundedSchemaNode "scm: make-contains-significant-variant-predicate")
        (ListLink
            gene) (stv .2 .7))
)
(use-modules (srfi srfi-1))
(define (common-GO-categories A B)
    (let*
        ((superA (cog-get-supersets A))
         (superB (cog-get-supersets B))
         (superA-length (length superA))
         (superB-length (length superB))
         (superUnion-length (length (lset-union equal? superA superB)))
         (superIntersection (lset-intersection equal? superA superB))
         (superIntersection-length (length superIntersection))
        )
        (if (> superIntersection-length 5)
            (begin
                (display "\nCommon Categories:\n")(display A)(display B)(newline)
                (for-each (lambda (x)
                                (begin
                                    (display (cog-name x))(display " ")
                                    (display (get-GO-name x))(newline)))
                           superIntersection)))
        ))
(define (get-GO-name A)
    (define name-node
        (cog-bind
            (BindLink
                (VariableList
                    (VariableNode "$GO_name"))
                (EvaluationLink
                         (PredicateNode "GO_name")
                         (ListLink
                                 A
                                 (VariableNode "$GO_name")
                         )
                )
                (VariableNode "$GO_name"))))
    (if (> (length (cog-outgoing-set name-node)) 0)
        (cog-name (gar name-node))
        ""))
(define (lifespan-observation-increased-members)
    (cog-outgoing-set
        (cog-bind
            (BindLink
                (MemberLink
                    (VariableNode "$A")
                    (ConceptNode "Lifespan_Observations_Increased_GeneSet"))
                (VariableNode "$A")))))
(define (common-with-lifespan-observation-genes A)
    (for-each common-GO-categories los (make-list (length los) A)))