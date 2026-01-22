from opencog.atomspace import *
from opencog.type_constructors import *
set_default_atomspace(AtomSpace())
set_is_unordered = is_a(get_type('SetLink'), get_type('UnorderedLink'))
print('Is a set unordered?', set_is_unordered)
A = ConceptNode('A')
print('Is A a concept?', is_a(A.type, get_type('ConceptNode')))
print('Is A a predicate?', is_a(A.type, get_type('PredicateNode')))
print(get_type_name(3) + '\n')
myatom = ConceptNode('foo')
print(get_type_name(myatom.type) + '\n')
for key, value in sorted(types.__dict__.items()):
    if '__' not in key:
        print(key, value)