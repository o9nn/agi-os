(define and-bit-prior
(ImplicationScope (stv 0.0001 0.001)
(VariableList
(TypedVariable
(Variable "$A")
(Type "DontExecLink"))
(Variable "$T"))
(And
(Evaluation
(Predicate "URE:BC:and-BIT")
(Variable "$A"))
(Evaluation
(Predicate "URE:BC:target")
(Variable "$T")))
(preproof-of
(List
(Variable "$A")
(Variable "$T")))))
(add-to-load-path "../../../opencog/pln/")
(define rule-filenames
(list "meta-rules/predicate/conditional-total-instantiation.scm"
"rules/propositional/fuzzy-conjunction-introduction.scm"
)
)
(for-each load-from-path rule-filenames)
(define and-bit-prior-rule
(car (apply-rule conditional-total-instantiation-implication-scope-meta-rule
and-bit-prior)))
(define and-bit-prior-rule-name (DefinedSchemaNode "and-bit-prior-rule"))
(Define and-bit-prior-rule-name and-bit-prior-rule)
(define abp-rbs (ConceptNode "and-bit-prior-rule-base"))
(InheritanceLink
abp-rbs
(ConceptNode "URE")
)
(define (abp-bc . args)
(apply cog-bc (cons abp-rbs args)))
(define rules
(list
and-bit-prior-rule-name
fuzzy-conjunction-introduction-2ary-rule-name
)
)
(ure-add-rules abp-rbs rules)
(ure-set-num-parameter abp-rbs "URE:maximum-iterations" 6)