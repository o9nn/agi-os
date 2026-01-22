from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.storage import *
from opencog.storage_rocks import *
space = AtomSpace()
set_default_atomspace(space)
e = EdgeLink(PredicateNode('URL'), ListLink(ItemNode('file:///Home Computer/folders/My photo album'), ItemNode('Fantastic Sunset on Sunday.jpg')))
print("Here's your data:", e)
storage = RocksStorageNode('rocks:///tmp/foo')
cog_open(storage)
store_atom(e)
cog_close(storage)
print('Closed the connection to storage')
def prt_atomspace_contents(asp):
    print('AtomSpace contains a total of ' + str(len(space)) + ' Atoms')
    if 0 < len(asp):
        print('These are:')
    count = 0
    for atom in asp:
        count += 1
        print('Atom ' + str(count) + '.... ' + str(atom))
print('The AtomSpace before clearing:')
prt_atomspace_contents(space)
print('\nWill now clear the AtomSpace.')
space.clear()
print('The AtomSpace size after clearing: ', len(space))
prt_atomspace_contents(space)
storage = RocksStorageNode('rocks:///tmp/foo')
cog_open(storage)
print('Restore one atom: the file, whose name we magically know already.')
fetch_atom(PredicateNode('URL'))
prt_atomspace_contents(space)
print('Restore all Edges in storage.')
fetch_incoming_set(PredicateNode('URL'))
cog_close(storage)
print('After restoring, the AtomSpace size is ' + str(len(space)))
prt_atomspace_contents(space)
print('Good bye!')