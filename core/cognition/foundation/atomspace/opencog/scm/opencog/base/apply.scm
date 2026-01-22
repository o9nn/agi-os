(define-public (get-variables forAll) (cog-outgoing-set (gar forAll)))
(define-public (get-body forAll) (gadr forAll))
(define-public (get-bindings forAll arguments)
(map (lambda (v a) (cons v a)) (get-variables forAll) arguments))
(define-public (substitute-var bindings body)
(let ((body_type (cog-type body)))
(cond
((cog-subtype? 'VariableList body_type) (VariableList))
((cog-subtype? 'Link body_type)
(apply cog-new-link
(cons body_type
(map (lambda (child) (substitute-var bindings child))
(cog-outgoing-set body)))))
((cog-subtype? 'VariableNode body_type) (cdr (assoc body bindings)))
(else body))))
(define-public (universal-instantiate forAll arguments)
(substitute-var (get-bindings forAll arguments) (get-body forAll)))