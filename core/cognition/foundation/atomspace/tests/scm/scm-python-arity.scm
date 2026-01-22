(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))
(use-modules (opencog test-runner))
(opencog-test-runner)
(define tname "python-arity-test")
(test-begin tname)
(python-eval "
from opencog.atomspace import AtomSpace, types
from opencog.type_constructors import TruthValue
# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b) :
atomspace = atom_a.atomspace
TV = TruthValue(0.2, 0.69)
atomspace.add_node(types.ConceptNode, 'Apple', TV)
atomspace.add_link(types.InheritanceLink, [atom_a, atom_b])
return TruthValue(0.42, 0.24)
")
(define returned-tv
(cog-evaluate!
(Evaluation
(GroundedPredicate "py:foo")
(List (Concept "fruit") (Concept "banana")))))
(test-assert "Apple atom was created"
(not (eq? '() (cog-node 'ConceptNode "Apple"))))
(test-assert "TV on Apple is wrong"
(< (abs (- 0.2 (cog-mean (Concept "Apple")))) 0.00001))
(test-assert "returned TV is wrong"
(< (abs (- 0.42 (cog-tv-mean returned-tv))) 0.00001))
(define (catch-wrong-args thunk)
(catch #t
thunk
(lambda (key . parameters)
(format (current-error-port)
"Expected to catch this Python exception: '~a: ~a\n"
key parameters)
"woo-hooo!!")))
(test-assert "Threw exception even when given the right number of arguments"
(equal? (SimpleTruthValue 0.42 0.24)
(catch-wrong-args (lambda ()
(cog-evaluate!
(Evaluation
(GroundedPredicate "py:foo")
(List (Concept "fruit") (Concept "banana"))))))))
(test-assert "Failed to throw when given too few arguments"
(string=? "woo-hooo!!"
(catch-wrong-args (lambda ()
(cog-evaluate!
(Evaluation
(GroundedPredicate "py:foo")
(List (Concept "fruit"))))))))
(test-assert "Failed to throw when given too many arguments"
(string=? "woo-hooo!!"
(catch-wrong-args (lambda ()
(cog-evaluate!
(Evaluation
(GroundedPredicate "py:foo")
(List
(Concept "fruit-stuff")
(Concept "banana")
(Concept "orange"))))))))
(test-end tname)
(opencog-test-end)