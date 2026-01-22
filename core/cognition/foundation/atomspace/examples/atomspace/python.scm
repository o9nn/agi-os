(use-modules (opencog) (opencog python))
(python-eval "print ('hello! ' + str(2+2))")
(python-eval "exec(open('my_py_func.py').read())")
(python-eval "
from opencog.atomspace import AtomSpace, createTruthValue
from opencog.atomspace import types
def foo(atspace):
    TV = createTruthValue(0.42, 0.69)
    atspace.add_node(types.ConceptNode, 'Apple', TV)
")
(python-call-with-as "foo" (cog-atomspace))
(cog-node 'ConceptNode "Apple")
(python-eval "
from opencog.scheme_wrapper import scheme_eval_as
from opencog.atomspace import createTruthValue
from opencog.atomspace import types
# Get the atomspace...
asp = scheme_eval_as('(cog-atomspace)')
TV = createTruthValue(0.444, 0.777)
# Do something with it ...
asp.add_node(types.ConceptNode, 'Banana', TV)
")
(cog-node 'ConceptNode "Banana")