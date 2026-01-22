(use-modules (opencog logger))
(define (gen-present-deduction-rule link-type var-type)
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (C (Variable "$C"))
         (AB (link-type A B))
         (BC (link-type B C))
         (AC (link-type A C)))
    (Bind
      (VariableList
        (TypedVariable A var-type)
        (TypedVariable B var-type)
        (TypedVariable C var-type))
      (And
        (Present
          AB
          BC)
        (Not (Identical A C)))
      AC)))
(define inheritance-present-deduction-rule
  (gen-present-deduction-rule InheritanceLink (TypeNode "ConceptNode")))
(define inheritance-present-deduction-rule-name
  (DefinedSchemaNode "inheritance-present-deduction-rule"))
(DefineLink inheritance-present-deduction-rule-name
  inheritance-present-deduction-rule)