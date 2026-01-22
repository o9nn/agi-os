(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "python-guile-shared-atomspace-test")
(test-begin tname)
(python-eval "
from opencog.atomspace import AtomSpace, types
from opencog.type_constructors import get_default_atomspace, TruthValue
# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b):
atomspace = get_default_atomspace()
TV = TruthValue(0.2, 0.69)
atomspace.add_node(types.ConceptNode, 'Apple', TV)
atomspace.add_link(types.InheritanceLink, [atom_a, atom_b])
return TruthValue(0.42, 0.24)
")
(cog-evaluate!
(Evaluation
(GroundedPredicate "py:foo")
(List (Concept "fruit") (Concept "banana"))))
(test-assert "Apple atom was created"
(not (eq? #f (cog-node 'ConceptNode "Apple"))))
(test-assert "TV on Apple is wrong"
(< (abs (- 0.2 (cog-mean (Concept "Apple")))) 0.00001))
(test-end tname)
(opencog-test-end)