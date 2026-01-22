from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.chempydemo import *
spa = AtomSpace()
set_default_atomspace(spa)
print(f'Hello! The AtomSpace is {str(spa)}')
Mg('foo')
x = Mg('foo')
print(f'The Magnesium atom is {str(x)}')
ch = SB(C('some carbon atom'), H('just a proton, ok?'))
print(f'The CH bond is {str(ch)}')
methane = Molecule(SB(C('1'), H('1')), SB(C('1'), H('2')), SB(C('1'), H('3')), SB(C('1'), H('4')))
print(f'Methane is a molecule: {str(methane)}')
c14 = Carbon14Node('found in wood')
print(f'The carbon-14 atom is {str(c14)}')
decay_products = c14.execute()
print(f'The carbon-14 decay products are {str(decay_products)}')