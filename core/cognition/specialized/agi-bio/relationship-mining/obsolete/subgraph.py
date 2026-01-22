import opencog.cogserver as cogserver
from opencog.atomspace import AtomSpace, types
import opencog.scheme_wrapper as scheme
from opencog.scheme_wrapper import *
from utilities import atoms_to_scheme_file
import os
import random
__author__ = 'eddie'
EXCLUDED_NODES_FOR_EXPANSION = ('biological_process', 'molecular_function', 'cellular_component', 'GO_term', 'GO_namespace', 'GO_synonym_EXACT', 'GO_synonym_BROAD', 'GO_synonym_RELATED', 'GO_synonym_NARROW', 'GO_name', 'GO_alt_id', 'RO_part_of')
DEFAULT_SUBGRAPH_SIZE = 100000
SMALL_RUN = False
V = VERBOSE = False
class SubgraphMiner:
    def __init__(self, atomspace=None):
        if not atomspace:
            atomspace = AtomSpace()
        self.a = self.atomspace = atomspace
        scheme.__init__(self.atomspace)
    def create_connected_subgraph(self, size=DEFAULT_SUBGRAPH_SIZE):
        print('Creating connected subgraph of {} atoms'.format(size))
        a = self.atomspace
        print('total atomspace size = {0}'.format(len(a)))
        genes = a.get_atoms_by_type(types.GeneNode)
        random_gene = genes[random.randrange(len(genes))]
        self.subgraph = subgraph = set()
        subgraph.add(random_gene)
        unprocessed = set([random_gene])
        while len(subgraph) < size and len(unprocessed) > 0:
            if V:
                print('unprocessed = {0}     subgraph = {1} '.format(len(unprocessed), len(subgraph)))
            atom = unprocessed.pop()
            if V:
                print('popped atom {0}'.format(atom))
            if atom.name in EXCLUDED_NODES_FOR_EXPANSION:
                continue
            inbound = a.get_incoming(atom.h)
            outbound = a.get_outgoing(atom.h)
            new = set(inbound).difference(subgraph)
            new.update(set(outbound).difference(subgraph))
            if len(subgraph) + len(new) > size:
                new = random.sample(new, size - len(subgraph))
            subgraph.update(new)
            unprocessed.update(new)
        print('final subgraph = {0} atoms'.format(len(subgraph)))
        filename = 'SUBGRAPH_{}.scm'.format(size)
        atoms_to_scheme_file(subgraph, filename)
        print('Generated results file {}'.format(filename))