(define pln-rule-equivalence-subset-substitution
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")
)
(AndLink
(EquivalenceLink
(VariableNode "$A")
(VariableNode "$B")
)
(SubsetLink
(VariableNode "$B")
(VariableNode "$C")
)
)
(ExecutionOutputLink
(GroundedSchemaNode "scm: pln-formula-equivalence-subset-substitution")
(ListLink
(EquivalenceLink
(VariableNode "$A")
(VariableNode "$B")
)
(SubsetLink
(VariableNode "$B")
(VariableNode "$C")
)
(SubsetLink
(VariableNode "$A")
(VariableNode "$C")
)
)
)
)
)
(define (pln-formula-equivalence-subset-substitution AB BC AC)
(display "formula-equiv-subs-subst-rule\n")
(display-atom "AB" AB)
(display-atom "BC" BC)
(display-atom "AC" AC)
(cog-set-tv! AC
(pln-formula-equivalence-subset-substitution-side-effect-free AB BC)
)
)
(define (pln-formula-equivalence-subset-substitution-side-effect-free AB BC)
(display "pln-formula-equivalance-subset-substitution-side-effect-free\n")
(let
((sAB (cog-stv-strength AB))
(cAB (cog-stv-confidence AB))
(sBC (cog-stv-strength BC))
(cBC (cog-stv-confidence BC)))
(display "sAB: ")(display sAB)(newline)
(display "cAB: ")(display cAB)(newline)
(stv (* sAB sBC) (* cAB cBC))
)
)
(cog-name-rule "pln-rule-equivalance-subset-substitution")