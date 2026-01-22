(load "formulas.scm")
(define and-to-subset-3-rule
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C"))
(AndLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C"))
(AndLink
(VariableNode "$A")
(VariableNode "$B")))
(ExecutionOutputLink
(GroundedSchemaNode "scm: and-to-subsetn-formula")
(ListLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C"))
(AndLink
(VariableNode "$A")
(VariableNode "$B"))
(SubsetLink
(AndLink
(VariableNode "$A")
(VariableNode "$B"))
(VariableNode "$C"))))))
(define and-to-subset-4-rule
(BindLink
(VariableList
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")
(VariableNode "$D"))
(AndLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")
(VariableNode "$D"))
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")))
(ExecutionOutputLink
(GroundedSchemaNode "scm: and-to-subsetn-formula")
(ListLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C")
(VariableNode "$D"))
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C"))
(SubsetLink
(AndLink
(VariableNode "$A")
(VariableNode "$B")
(VariableNode "$C"))
(VariableNode "$D"))))))
(define (and-to-subsetn-formula ABCD ABC sABCD)
(cog-set-tv!
sABCD
(stv
(/ (cog-mean ABCD) (cog-mean ABC))
(min (cog-confidence ABCD) (cog-confidence ABC)))))