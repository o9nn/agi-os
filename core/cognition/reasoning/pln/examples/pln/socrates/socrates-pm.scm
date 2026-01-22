(use-modules (opencog))
(use-modules (opencog atom-types))
(use-modules (opencog exec))
(use-modules (opencog query))
(use-modules (opencog nlp))
(use-modules (opencog nlp chatbot))
(use-modules (opencog nlp relex2logic))
(use-modules (opencog pln))
(define (get-r2l sent-node)
  (interp-get-r2l-outputs (car (sent-get-interp sent-node))))
(define (mock-pln-input sentence)
  (get-r2l (car (nlp-parse sentence))))
(load "john-is-a-man-r2l-output.scm")
(load "men-breathe-air-r2l-output.scm")
(add-to-load-path "../../../opencog/pln/rules")
(load-from-path (pln-rule-type->filename "term/deduction"))
(load-from-path (pln-rule-type->filename "wip/abduction"))
(define sog-hack-decomposition-rule
  (Bind
    (VariableList
      (TypedVariable
        (Variable "$W")
        (Type "WordInstanceNode"))
      (TypedVariable
        (Variable "$P")
        (Type "PredicateNode"))
      (TypedVariable
        (Variable "$A")
        (Type "ConceptNode"))
      (TypedVariable
        (Variable "$B")
        (Type "ConceptNode"))
      (TypedVariable
        (Variable "$A-subset")
        (Type "ConceptNode")))
    (And
      (Reference
        (Variable "$P")
        (Variable "$W"))
      (Inheritance
        (Variable "$A-subset")
        (Variable "$A"))
      (Evaluation
        (Variable "$P")
        (List
          (Variable "$A")
          (Variable "$B"))))
    (Evaluation (stv 1 1)
      (Variable "$P")
      (List
        (Variable "$A-subset")
        (Variable "$B")))))
(cog-execute! abduction-inheritance-rule)
(cog-execute! deduction-inheritance-rule)
(cog-execute! sog-hack-decomposition-rule)