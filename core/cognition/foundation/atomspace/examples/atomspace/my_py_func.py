from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
asp = AtomSpace()
def my_py_func(atoma, atomb):
    print('Python received two arguments:\n' + str(atoma) + str(atomb))
    av = float(atoma.name)
    bv = float(atomb.name)
    cv = av + bv
    print(f'The sum is {str(cv)}')
    return asp.add_node(types.ConceptNode, str(cv))
def my_py_predicate(atoma, atomb):
    print('Python predicate received two arguments:\n' + str(atoma) + str(atomb))
    av = float(atoma.name)
    bv = float(atomb.name)
    return TruthValue(1.0 / av, 1.0 / bv)