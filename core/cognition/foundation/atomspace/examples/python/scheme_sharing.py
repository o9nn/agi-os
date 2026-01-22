from opencog.atomspace import *
from opencog.type_constructors import *
from opencog.scheme_wrapper import *
asp = AtomSpace()
set_default_atomspace(asp)
ConceptNode('this is a test')
scheme_eval(asp, '(cog-prt-atomspace)')
scheme_eval(asp, '(format #t "Yes this is really scheme: ~A\n" (+ 2 2))')
foo_atom = scheme_eval_h(asp, '(Concept "foo")')
print('In python it looks like this:', foo_atom)
print('The AtomSpace contains:', list(asp))