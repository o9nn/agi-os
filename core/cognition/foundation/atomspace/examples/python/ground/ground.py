import sys
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
import mymodule as mm
a = AtomSpace()
set_default_atomspace(a)
def local_func(x1, x2, x3):
    print('Entering local function with\n', x1, x2, x3)
    return x3
class LocalClass:
    @staticmethod
    def static_check(x1):
        print('Entering static method with\n', x1)
        return x1
    def forward(self, x1, x2):
        print('Entering LocalClass with\n', x1, x2)
        return x1
nn = LocalClass()
exlof = ExecutionOutput(GroundedSchema('py:local_func'), ListLink(Concept('aa'), Concept('bb'), Concept('cc')))
assert exlof.execute() == Concept('cc'), 'Failed while calling local function'
exloc = ExecutionOutput(GroundedSchema('py:nn.forward'), List(Concept('aa'), Concept('bb')))
assert exloc.execute() == Concept('aa'), 'Failed while calling local object method'
exst = ExecutionOutput(GroundedSchema('py:LocalClass.static_check'), ListLink(Concept('aa')))
assert exst.execute() == Concept('aa'), 'Failed while calling static class method'
exexf = ExecutionOutput(GroundedSchema('py:mm.mod_func'), ListLink(Concept('aa'), Concept('bb'), Concept('cc')))
assert exexf.execute() == Concept('aa'), 'Failed while calling external function'
exext = ExecutionOutputLink(GroundedSchemaNode('py:mm.nn.submodule.forward'), ListLink(ConceptNode('aa'), ConceptNode('bb')))
assert exext.execute() == Concept('bb'), 'Failed while calling external object method'
'\n# also, here is an example of how to make a generic wrapper to call object methods\nclass LocalClass:\n    def forward(self, listArgs):\n        print("entering forward with args:")\n        print(listArgs)\n        return listArgs.out[0]\n\ndef callObjMethod(conceptObject, conceptFunction, listArgs):\n    o = getattr(sys.modules[__name__], conceptObject.name)\n    return getattr(o, conceptFunction.name)(listArgs)\n\nex = ExecutionOutput(\n   GroundedSchemaNode("py:callObjMethod"),\n   List(Concept("nn"),\n            List(Concept("bb"))))\n'