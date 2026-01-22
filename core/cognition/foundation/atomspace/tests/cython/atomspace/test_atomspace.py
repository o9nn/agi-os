from unittest import TestCase
import opencog.atomspace
from opencog.atomspace import Atom
from opencog.atomspace import types, is_a, get_type, get_type_name, create_child_atomspace
from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog, tmp_atomspace
from time import sleep
class AtomSpaceTest(TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_bare(self):
        self.space
        self.space.type
        self.space.long_string()
        self.space.short_string()
        self.space.is_atom()
        self.space.is_node()
        self.space.is_link()
        list(self.space)
        str(self.space)
        len(self.space)
    def test_add_node(self):
        self.space.add_node(types.Node, 'node')
        self.assertRaises(TypeError, self.space.add_node, types.Node, 'test', 0, True)
        self.assertRaises(TypeError, self.space.add_node, 'ConceptNode', 'test', TruthValue(0.5, 0.8))
        a1 = Node('test')
        self.assertTrue(a1)
        a2 = Node('test')
        self.assertEqual(a1, a2)
        caught = False
        try:
            self.space.add_node(types.Link, 'test')
        except RuntimeError:
            caught = True
        self.assertEqual(caught, True)
        a3 = Node('test_w_tv').truth_value(0.5, 0.8)
        self.assertEqual(self.space.size(), 3)
        a4 = Node('test_w_tv_alt', tv=TruthValue(0.5, 0.8))
        self.assertEqual(self.space.size(), 4)
    def test_add_link(self):
        n1 = Node('test1')
        n2 = Node('test2')
        l1 = Link(n1, n2)
        self.assertTrue(l1 is not None)
        l2 = Link(n1, n2)
        self.assertTrue(l2 is not None)
        self.assertTrue(l2 == l1)
        n3 = Node('test3')
        l3 = Link(n1, n3).truth_value(0.5, 0.8)
        self.assertTrue(l3 is not None)
        n4 = Node('test4')
        l4 = Link(n1, n4, tv=TruthValue(0.5, 0.8))
        self.assertTrue(l4 is not None)
        caught = False
        try:
            l1 = self.space.add_link(types.Node, [n1, n3])
        except RuntimeError:
            caught = True
        self.assertEqual(caught, True)
    def test_is_valid(self):
        a1 = Node('test1')
        self.assertTrue(self.space.is_valid(a1))
        self.assertRaises(TypeError, self.space.is_valid, 'test')
    def test_truth_value(self):
        tv = TruthValue(0.5, 0.8)
        self.assertEqual(tv.mean, 0.5)
        self.assertAlmostEqual(tv.confidence, 0.8, places=4)
        self.assertEqual(str(tv), '(stv 0.5 0.8)')
        tv2 = TruthValue(0.5, 0.8)
        tv3 = TruthValue(0.6, 0.8)
        self.assertTrue(tv == tv2)
        self.assertFalse(tv == tv3)
        atom = Node('atom with tv')
        default_tv = atom.tv
        atom.truth_value(0.75, 0.9)
        new_tv = atom.tv
        self.assertFalse(new_tv == default_tv)
        self.assertEqual(new_tv.mean, 0.75)
        self.assertAlmostEqual(new_tv.confidence, 0.9, places=4)
    def test_get_by_type(self):
        a1 = Node('test1')
        a2 = ConceptNode('test2')
        a3 = PredicateNode('test3')
        result = self.space.get_atoms_by_type(types.Node)
        self.assertTrue(a1 in result)
        self.assertTrue(a2 in result)
        self.assertTrue(a3 in result)
        l1 = InheritanceLink(a1, a2)
        result = self.space.get_atoms_by_type(types.Link)
        self.assertTrue(l1 in result)
        result = self.space.get_atoms_by_type(types.Node, subtype=False)
        self.assertTrue(a1 in result)
        self.assertTrue(a2 not in result)
        self.assertTrue(a3 not in result)
        result = self.space.get_atoms_by_type(types.AnchorNode, subtype=False)
        self.assertEqual(len(result), 0)
    def test_incoming_by_type(self):
        a1 = Node('test1')
        a2 = ConceptNode('test2')
        a3 = PredicateNode('test3')
        result = a1.incoming_by_type(types.Node)
        self.assertTrue(a1 not in result)
        l1 = InheritanceLink(a1, a2)
        result = a1.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 in result)
        result = a2.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 in result)
        result = a3.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 not in result)
    def test_remove(self):
        a1 = Node('test1')
        a2 = ConceptNode('test2')
        a3 = PredicateNode('test3')
        self.assertTrue(a1 in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)
        self.space.remove(a1)
        self.assertTrue(a1 not in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)
        l = SimilarityLink(a2, a3)
        self.space.remove(a2, True)
        self.assertTrue(a2 not in self.space)
        self.assertTrue(l not in self.space)
    def test_clear(self):
        a1 = Node('test1')
        a2 = ConceptNode('test2')
        a3 = PredicateNode('test3')
        self.space.clear()
        self.assertEqual(self.space.size(), 0)
        self.assertEqual(len(self.space), 0)
    def test_container_methods(self):
        self.assertEqual(len(self.space), 0)
        a1 = Node('test1')
        a2 = ConceptNode('test2')
        a3 = PredicateNode('test3')
        self.assertTrue(a1 in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)
        self.assertEqual(len(self.space), 3)
    def test_context_mgr_tmp(self):
        a = ConceptNode('a')
        with tmp_atomspace() as tmp_as:
            b = ConceptNode('b')
            self.assertTrue(a in self.space)
            self.assertFalse(b in self.space)
        c = ConceptNode('c')
        self.assertTrue(c in self.space)
class AtomTest(TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_create_child_atomspace(self):
        a = opencog.atomspace.AtomSpace()
        b = opencog.atomspace.create_child_atomspace(a)
        del a
    def test_creation(self):
        a = Node('test1')
        self.assertEqual(a.name, 'test1')
        self.assertEqual(a.tv, TruthValue(1.0, 0.0))
    def test_w_truthvalue(self):
        tv = TruthValue(0.5, 100)
        a = Node('test2', tv)
        self.assertEqual(a.tv, tv)
        a.tv = TruthValue(0.1, 10)
        self.assertEqual(a.tv, TruthValue(0.1, 10))
    def test_out(self):
        a1 = Node('test2')
        self.assertEqual(a1.out, [])
        tv = TruthValue(0.5, 100)
        a2 = Node('test3', tv)
        l = Link(a1, a2)
        self.assertEqual(l.out, [a1, a2])
        self.assertRaises(AttributeError, setattr, l, 'out', [a1])
    def test_arity(self):
        a1 = Node('test2')
        self.assertEqual(a1.arity, 0)
        tv = TruthValue(0.5, 100)
        a2 = Node('test3', tv)
        l = Link(a1, a2)
        self.assertEqual(l.arity, 2)
        self.assertRaises(AttributeError, setattr, l, 'arity', 4)
    def test_type(self):
        a = Node('test2')
        a2 = Node('test3')
        l = Link(a, a2)
        self.assertRaises(AttributeError, setattr, l, 'type', 5)
        self.assertRaises(AttributeError, setattr, a, 'type', 5)
        self.assertEqual(l.type_name, 'Link')
        self.assertEqual(a.type_name, 'Node')
    def test_create_child_atomspace(self):
        test = ConceptNode('test')
        b = create_child_atomspace(self.space)
        test2 = b.add_node(types.ConceptNode, 'test2')
        self.assertTrue(test in b.get_atoms_by_type(types.ConceptNode))
        self.assertTrue(test2 in b.get_atoms_by_type(types.ConceptNode))
        self.assertTrue(test2 not in self.space.get_atoms_by_type(types.ConceptNode))
    def test_strings(self):
        tv = TruthValue(0.5, 0.8)
        a1 = Node('test1', tv)
        a2 = Node('test2')
        a2.tv = TruthValue(0.1, 0.3)
        l = Link(a1, a2)
        space_uuid = 0
        a1_expected = '(Node "test1") ; [{0}]\n'.format(space_uuid)
        a1_expected_long = '(Node "test1" (stv 0.500000 0.800000)) ; [{0}]\n'.format(space_uuid)
        a2_expected = '(Node "test2") ; [{0}]\n'.format(space_uuid)
        a2_expected_long = '(Node "test2" (stv 0.100000 0.300000)) ; [{0}]\n'.format(space_uuid)
        l_expected = '(Link\n  {0}  {1}) ; [{2}]\n'.format(a1_expected, a2_expected, space_uuid)
        l_expected_long = '(Link\n  {0}  {1}) ; [{2}]\n'.format(a1_expected_long, a2_expected_long, space_uuid)
class TypeTest(TestCase):
    def test_is_a(self):
        self.assertTrue(is_a(types.ConceptNode, types.Node))
        self.assertTrue(is_a(types.ConceptNode, types.Atom))
        self.assertTrue(is_a(types.ListLink, types.Link))
        self.assertTrue(is_a(types.ListLink, types.Atom))
        self.assertFalse(is_a(types.Link, types.Node))
    def test_get_type(self):
        self.assertEqual(get_type('ConceptNode'), types.ConceptNode)
        self.assertEqual(get_type(''), types.NO_TYPE)
        self.assertRaises(TypeError, get_type, 1)
    def test_get_type_name(self):
        self.assertEqual(get_type_name(types.Node), 'Node')
        self.assertEqual(get_type_name(2231), '')
        self.assertEqual(get_type_name(types.NO_TYPE), '*** Bottom Type! ***')