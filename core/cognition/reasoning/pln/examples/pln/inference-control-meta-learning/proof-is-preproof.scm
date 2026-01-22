(define proof-is-preproof
  (Implication (stv 1 1)
    (Predicate "URE:BC:proof-of")
    (Predicate "URE:BC:preproof-of")))
(add-to-load-path "../../../opencog/pln/")
(load-from-path "meta-rules/predicate/conditional-total-instantiation.scm")
(define proof-is-preproof-rule
  (car (apply-rule conditional-total-instantiation-implication-meta-rule
                   proof-is-preproof)))