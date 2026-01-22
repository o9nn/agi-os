(define preproof-expander-is-preproof
(ImplicationScope (stv 1 1)
(VariableList
(TypedVariable
(Variable "$A")
(Type "DontExecLink"))
(TypedVariable
(Variable "$B")
(Type "DontExecLink"))
(TypedVariable
(Variable "$R")
(Type "DontExecLink"))
(Variable "$L")
(Variable "$T"))
(And
(expand
(List
(Variable "$A")
(Variable "$L")
(Variable "$R"))
(Variable "$B"))
(preproof-of
(List
(Variable "$B")
(Variable "$T"))))
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
(define preproof-expander-is-preproof-rule
(car (apply-rule conditional-total-instantiation-implication-scope-meta-rule
preproof-expander-is-preproof)))
(define preproof-expander-is-preproof-rule-name
(DefinedSchemaNode "preproof-expander-is-preproof-rule"))
(DefineLink preproof-expander-is-preproof-rule-name
preproof-expander-is-preproof-rule)
(define pep-rbs (ConceptNode "preproof-expander-is-preproof-rule-base"))
(define (pep-bc . args)
(apply cog-bc (cons pep-rbs args)))
(define rules
(list
preproof-expander-is-preproof-rule-name
fuzzy-conjunction-introduction-2ary-rule-name
)
)
(ure-add-rules pep-rbs rules)
(ure-set-num-parameter pep-rbs "URE:maximum-iterations"
(ceiling (exact->inexact (+ (/ (+ piter 1) 2)
(* piter piter)))))
(ure-set-num-parameter pep-rbs "URE:complexity-penalty" -2)
(define pep-rule
(BindLink
(VariableList
(TypedVariableLink
(VariableNode "$A")
(TypeNode "DontExecLink")
)
(VariableNode "$T")
(TypedVariableLink
(VariableNode "$B-cc00185")
(TypeNode "DontExecLink")
)
(TypedVariableLink
(VariableNode "$R-7b43bfa2")
(TypeNode "DontExecLink")
)
(VariableNode "$L-359df4eb")
)
(AndLink
(preproof-of
(ListLink
(VariableNode "$B-cc00185")
(VariableNode "$T")
)
)
(expand
(ListLink
(VariableNode "$A")
(VariableNode "$L-359df4eb")
(VariableNode "$R-7b43bfa2")
)
(VariableNode "$B-cc00185")
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: conditional-total-instantiation-scope-formula")
(ListLink
(preproof-of
(ListLink
(VariableNode "$A")
(VariableNode "$T")
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: fuzzy-conjunction-introduction-formula")
(ListLink
(AndLink
(preproof-of
(ListLink
(VariableNode "$B-cc00185")
(VariableNode "$T")
)
)
(expand
(ListLink
(VariableNode "$A")
(VariableNode "$L-359df4eb")
(VariableNode "$R-7b43bfa2")
)
(VariableNode "$B-cc00185")
)
)
(SetLink
(preproof-of
(ListLink
(VariableNode "$B-cc00185")
(VariableNode "$T")
)
)
(expand
(ListLink
(VariableNode "$A")
(VariableNode "$L-359df4eb")
(VariableNode "$R-7b43bfa2")
)
(VariableNode "$B-cc00185")
)
)
)
)
(ImplicationScopeLink (stv 1.000000 1.000000)
(VariableList
(TypedVariableLink
(VariableNode "$A")
(TypeNode "DontExecLink")
)
(TypedVariableLink
(VariableNode "$B")
(TypeNode "DontExecLink")
)
(TypedVariableLink
(VariableNode "$R")
(TypeNode "DontExecLink")
)
(VariableNode "$L")
(VariableNode "$T")
)
(AndLink
(expand
(ListLink
(VariableNode "$A")
(VariableNode "$L")
(VariableNode "$R")
)
(VariableNode "$B")
)
(preproof-of
(ListLink
(VariableNode "$B")
(VariableNode "$T")
)
)
)
(preproof-of
(ListLink
(VariableNode "$A")
(VariableNode "$T")
)
)
)
)
)
)
)