"
Helper functions for rule application
(get-set-members set)
(cog-get-supersets A)
(cog-apply-rule rule atoms #:optional no-focus-set)
(define (create-not-gene-set A)
(cog-define-name rule-symbol-name)
(define (pln-attraction-rule-no-variables subsetAB subsetNotAB)
"
(use-modules (opencog))
(use-modules (opencog query))
(use-modules (opencog rule-engine))
(define (cog-name-rule rule-symbol-name)
(define name-append (string-append rule-symbol-name "-name"))
(eval `(define ,(string->symbol name-append) ,(DefinedSchema rule-symbol-name))
(interaction-environment))
(DefineLink
(DefinedSchema rule-symbol-name)
(eval-string rule-symbol-name)))
(define* (cog-apply-rule rule atoms #:optional no-focus-set)
#!
Do a one-step inference via the URE using a particular rule on specified atoms
rule - String representation of the rule symbol name used in the rule
definition file, e.g., "pln-rule-deduction". Aternatively, can also pass
the rule name Node defined in the rule file, e.g., (Node "pln-rule-deduction")
atoms - The source atom(s) used by the forward chainer for applying the rule.
Can be a scheme list of Atoms, a SetLink containing Atoms, or an
individual Atom. By default, these atoms will also serve as the focus set
of the chainer, unless the optional no-focus-set parameter is set to #t.
no-focus-set (optional) - Boolean that defaults to #f. When #t, no focus set
is specified, and the whole atomspace is searched for additional premises
to satisfy the rule. When #f (default) the atoms in the 'atoms' parameter
are used as the focus set.
Requires that the applicable scheme rule file (i.e., from
opencog/reasoning/pln/rules) has been loaded.
Example usage:
(define ab (InheritanceLink (ConceptNode "a") (ConceptNode "b")))
(define ab (InheritanceLink (ConceptNode "b") (ConceptNode "c")))
(cog-apply-rule "pln-rule-deduction" (list ab bc))
!#
(define temp-rbs)
(define URE-inheritance)
(define rules)
(define focus-set)
(for-each cog-delete (cog-get-root (ConceptNode "temp-pln")))
(for-each cog-delete (cog-incoming-set (ConceptNode "temp-pln")))
(set! temp-rbs (ConceptNode "temp-pln"))
(set! URE-inheritance
(InheritanceLink
temp-rbs
(ConceptNode "URE")
)
)
(if (not (or (string? rule)
(and (cog-atom? rule) (equal? (cog-type rule) 'DefinedSchemaNode))))
(begin
(display "\n    Usage: cog-apply-rule \"quoted-rule-name\" ")
(display "(list atom1 atom2 ...)\n\n")
(exit)
)
(begin
(if (string? rule)
(set! rule (DefinedSchemaNode rule)))
(set! rules (list (list rule 1)))
(ure-add-rules temp-rbs rules)
(ure-set-num-parameter temp-rbs "URE:maximum-iterations" 1)
(ure-set-fuzzy-bool-parameter temp-rbs "URE:attention-allocation" 0)
(if (list? atoms)
(set! atoms (SetLink atoms)))
(if (equal? (cog-type atoms) 'ListLink)
(set! atoms (SetLink (cog-outgoing-set atoms))))
(if (not (equal? (cog-type atoms) 'SetLink))
(set! atoms (SetLink atoms)))
(if no-focus-set
(set! focus-set (SetLink))
(set! focus-set atoms))
(cog-fc atoms temp-rbs focus-set)
)
)
)
#
(define (cog-apply-rule-test)
(define atoms (list
(InheritanceLink (ConceptNode "A" (stv .3 1))
(ConceptNode "B" (stv .3 1)))
(InheritanceLink (ConceptNode "B") (ConceptNode "C" (stv .3 1)))))
(load-from-path "rules/deduction.scm")
(load-from-path "av-tv.scm")
(display "Doing deduction rule\n")
(display (cog-apply-rule "deduction-rule" atoms))
(load-from-path "rules/induction-rule.scm")
(display "Doing irrelevant rule\n")
(display (cog-apply-rule "induction-inheritance-rule" atoms))
)
(define (get-set-members set)
"
Return members of set that are defined through MemberLink relationships with
set, or if set is a SetLink return its outgoing set.
"
(if (equal? (cog-type set) 'SetLink)
(cog-outgoing-set set)
(cog-outgoing-set
(cog-bind
(BindLink
(VariableList
(VariableNode "$x"))
(MemberLink
(VariableNode "$x")
set)
(VariableNode "$x"))))))
(define (cog-get-supersets A)
"
Return the atoms that A is a (direct) subset of through a SubsetLink
relationship. Also checks for cases where singleton {A} is a subset.
"
(let ((result
(cog-outgoing-set
(cog-bind
(BindLink
(VariableList
(VariableNode "$B")
)
(ChoiceLink
(SubsetLink
A
(VariableNode "$B")
)
(SubsetLink
(SetLink
A
)
(VariableNode "$B")
)
)
(VariableNode "$B")
)
)
)
)) result )
)
(define (create-not-gene-set A)
(cog-new-link 'SetLink (lset-difference equal? (cog-get-atoms 'GeneNode)
(get-set-members A))))
(define (pln-attraction-rule-no-variables subsetAB subsetNotAB)
(define attractionAB)
(set! attractionAB (AttractionLink
(list-ref (cog-outgoing-set subsetAB) 0)
(list-ref (cog-outgoing-set subsetAB) 1)))
(attraction-formula attractionAB subsetAB subsetNotAB))