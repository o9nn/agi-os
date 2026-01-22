__author__ = 'eddie'
import time
from opencog.scheme_wrapper import load_scm
def atoms_to_scheme_file(atoms, filename):
    f = open(filename, 'wb')
    for atom in atoms:
        f.write(str(atom) + '\n')
    f.close()
def load_scheme_files(atomspace, files, label=''):
    print('Loading scheme files - {}'.format(label))
    start = time.clock()
    for file in files:
        print('  Loading scheme file: ' + file)
        if not load_scm(atomspace, file):
            print('***  Error loading scheme file: ' + file + '  ***')
    scheme_load_time = int(time.clock() - start)
    print('Scheme file loading completed in {} seconds'.format(scheme_load_time))
    return scheme_load_time