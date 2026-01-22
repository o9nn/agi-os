from opencog.atomspace import AtomSpace, Atom
from opencog.type_constructors import FloatValue, StringValue
from opencog.atomspace import types
a = AtomSpace()
concept_type = types.ConceptNode
A = a.add_node(concept_type, 'Apple')
B = a.add_node(concept_type, 'Berry')
C = a.add_node(concept_type, 'Comestible')
weights = FloatValue([1, 0.8, 42, 3.14159])
key = a.add_node(types.PredicateNode, 'my favorite keyname')
A.set_value(key, weights)
B.set_value(key, FloatValue([0.5, 0.333, 66, 2.71828]))
C.set_value(key, StringValue(['just', 'some', 'words']))
A.get_value(key)
B.get_value(key)
C.get_value(key)
print('Ello, there!', list(C.get_value(key)))
inh_type = types.InheritanceLink
a.add_link(inh_type, [A, B])
a.add_link(inh_type, [B, C])
a.add_link(inh_type, [A, C])
V = a.add_node(types.VariableNode, 'x')
M = a.add_link(types.MeetLink, [V])
print('The atomspace contains:\n\n', M.execute())