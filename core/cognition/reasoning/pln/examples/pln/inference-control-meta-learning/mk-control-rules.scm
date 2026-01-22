(define control-as (cog-new-atomspace))
(define (mk-control-rules)
(clear)
(cp-as history-as (cog-atomspace))
(icl-logger-debug "Evaluate antecedents")
(evaluate-antecedents)
(icl-logger-debug "Mine control rules")
(mine-all-control-rules)
(icl-logger-fine "mk-control-rules (cog-atomspace) [after mining control rules]:")
(icl-logger-fine-atomspace (cog-atomspace))
(ure-logger-flush)
(icl-logger-debug "Evaluate control rules")
(let* ((results (evaluate-control-rules)))
(icl-cp control-as (cog-outgoing-set results)))
(remove-dangling-atoms control-as)
(icl-logger-debug "Control AtomSpace:")
(icl-logger-debug-atomspace control-as)
)
(define (ground-control-rules)
(load "pln-rb.scm")
(icl-logger-debug "Ground context free control rules")
(ground-context-free-rules)
(icl-logger-debug "Ground a-pattern control rules")
(ground-a-pattern-rules)
)
(define (ground-context-free-rules)
(let* ((vardecl (TypedVariable
(Variable "$Rule")
(Type "DefinedSchemaNode")))
(impl-vardecl (VariableList
(Variable "$T")
(dontexec-typed (Variable "$A"))
(Variable "$L")
(dontexec-typed (Variable "$B"))))
(impl-antecedent (And
(preproof-of
(List
(Variable "$A")
(Variable "$T")))
(expand
(List
(Variable "$A")
(Variable "$L")
(DontExec (Variable "$Rule")))
(Variable "$B"))))
(impl-consequent (preproof-of
(List
(Variable "$B")
(Variable "$T"))))
(target (ImplicationScope
impl-vardecl
impl-antecedent
impl-consequent))
(rules-to-targets (Bind
vardecl
(Member
(Variable "$Rule")
pln-rbs)
target))
(results (cog-execute! rules-to-targets)))
(extract-hypergraph rules-to-targets)
results))
(define (ground-a-pattern-rules)
(let* ((vardecl (TypedVariable
(Variable "$Rule")
(Type "DefinedSchemaNode")))
(impl-vardecl (VariableList
(Variable "$T")
(dontexec-typed (Variable "$A"))
(Variable "$X")
(dontexec-typed (Variable "$B"))))
(impl-antecedent (And
(preproof-of
(List
(Variable "$A")
(Variable "$T")))
(expand
(List
(Variable "$A")
(Inheritance
(Concept "a")
(Variable "$X"))
(DontExec (Variable "$Rule")))
(Variable "$B"))))
(impl-consequent (preproof-of
(List
(Variable "$B")
(Variable "$T"))))
(target (ImplicationScope
impl-vardecl
impl-antecedent
impl-consequent))
(rules-to-targets (Bind
vardecl
(Member
(Variable "$Rule")
pln-rbs)
target))
(results (cog-execute! rules-to-targets)))
(extract-hypergraph rules-to-targets)
results))
(define (evaluate-antecedents)
(load "icr-rb.scm")
(let* ((impl-antecedent (And
(preproof-of
(List
(DontExec (Variable "$A"))
(Variable "$T")))
(expand
(List
(DontExec (Variable "$A"))
(Variable "$L")
(DontExec (Variable "$Rule")))
(DontExec (Variable "$B")))))
(results (pp-icr-bc impl-antecedent)))
(icl-logger-fine "evaluate-antecedents results = ~a" results)
(extract-hypergraph impl-antecedent)
(cog-extract results)
(remove-dangling-atoms (cog-atomspace))
)
)
(define (evaluate-control-rules)
(load "icr-rb.scm")
(icl-logger-fine "evaluate-control-rules (cog-atomspace):")
(icl-logger-fine-atomspace (cog-atomspace))
(let* ((control-rules-vardecl (VariableList
(TypedVariable
(Variable "$impl-vardecl")
(Type "VariableList"))
(TypedVariable
(Variable "$preproof-A-args")
(Type "ListLink"))
(TypedVariable
(Variable "$expand-inputs")
(Type "ListLink"))
(Variable "$expand-output")
(TypedVariable
(Variable "$preproof-B-args")
(Type "ListLink"))))
(control-rules-target (Quote
(ImplicationScope
(Unquote
(Variable "$impl-vardecl"))
(And
(preproof-of
(Unquote (Variable "$preproof-A-args")))
(expand
(Unquote (Variable "$expand-inputs"))
(Unquote (Variable "$expand-output"))))
(preproof-of
(Unquote (Variable "$preproof-B-args")))))))
(icl-logger-fine "evaluate-control-rules control-rules-vardecl = ~a"
control-rules-vardecl)
(icl-logger-fine "evaluate-control-rules control-rules-target = ~a"
control-rules-target)
(icr-bc control-rules-target #:vardecl control-rules-vardecl)))