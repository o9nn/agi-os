from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
from opencog.scheme_wrapper import scheme_eval, scheme_eval_h
a = AtomSpace()
set_default_atomspace(a)
scheme_eval(a, '(load "crisp-deduction.scm")')
fc_result = scheme_eval_h(a, '(crisp-deduction-fc AB)')
print('fc_result =', fc_result)