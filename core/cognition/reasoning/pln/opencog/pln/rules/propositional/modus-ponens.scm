(load "formulas.scm")
(define (gen-modus-ponens-rule link-type)
(let* ((A (Variable "$A"))
(B (Variable "$B"))
(AB (link-type
A
B)))
(Bind
(VariableList
A
B)
(And
(Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
A)
(Evaluation
(GroundedPredicate "scm: gt-zero-confidence")
AB)
(Present
AB
A))
(ExecutionOutputLink
(GroundedSchemaNode "scm: modus-ponens")
(ListLink
B
AB
A)))))
(define (modus-ponens B AB A)
(let
((sA (cog-mean A))
(cA (cog-confidence A))
(sAB (cog-mean AB))
(cAB (cog-confidence AB))
(snotAB 0.2)
(cnotAB 1))
(cog-set-tv!
B
(stv
(precise-modus-ponens-strength-formula sA sAB snotAB)
(min (min cAB cnotAB) cA)))))
(define modus-ponens-inheritance-rule
(gen-modus-ponens-rule InheritanceLink))
(define modus-ponens-implication-rule
(gen-modus-ponens-rule ImplicationLink))
(define modus-ponens-subset-rule
(gen-modus-ponens-rule SubsetLink))
(define modus-ponens-inheritance-rule-name
(DefinedSchemaNode "modus-ponens-inheritance-rule"))
(DefineLink modus-ponens-inheritance-rule-name
modus-ponens-inheritance-rule)
(define modus-ponens-implication-rule-name
(DefinedSchemaNode "modus-ponens-implication-rule"))
(DefineLink modus-ponens-implication-rule-name
modus-ponens-implication-rule)
(define modus-ponens-subset-rule-name
(DefinedSchemaNode "modus-ponens-subset-rule"))
(DefineLink modus-ponens-subset-rule-name
modus-ponens-subset-rule)
(define inheritance-modus-ponens-rule
(gen-modus-ponens-rule InheritanceLink))
(define implication-modus-ponens-rule
(gen-modus-ponens-rule ImplicationLink))
(define subset-modus-ponens-rule
(gen-modus-ponens-rule SubsetLink))
(define inheritance-modus-ponens-rule-name
(DefinedSchemaNode "inheritance-modus-ponens-rule"))
(DefineLink inheritance-modus-ponens-rule-name
inheritance-modus-ponens-rule)
(define implication-modus-ponens-rule-name
(DefinedSchemaNode "implication-modus-ponens-rule"))
(DefineLink implication-modus-ponens-rule-name
implication-modus-ponens-rule)
(define subset-modus-ponens-rule-name
(DefinedSchemaNode "subset-modus-ponens-rule"))
(DefineLink subset-modus-ponens-rule-name
subset-modus-ponens-rule)