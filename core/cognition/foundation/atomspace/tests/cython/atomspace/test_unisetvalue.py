import unittest
from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog
class UnisetValueTest(unittest.TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_create_empty(self):
        value = UnisetValue()
        self.assertTrue(value is not None)
        self.assertEqual(0, len(value))
    def test_create_single_value(self):
        value = UnisetValue(StringValue('foo'))
        self.assertTrue(value is not None)
        self.assertEqual(1, len(value))
    def test_create_list_value(self):
        value = UnisetValue([FloatValue(42), StringValue('foo')])
        self.assertTrue(value is not None)
        self.assertEqual(2, len(value))
    def test_open_close(self):
        value = UnisetValue()
        value.open()
        self.assertFalse(value.is_closed())
        value.close()
        self.assertTrue(value.is_closed())
    def test_add_remove(self):
        value = UnisetValue()
        value.open()
        value.add(FloatValue(1.0))
        value.add(StringValue('hello'))
        value.add(FloatValue(2.0))
        self.assertEqual(3, len(value))
        removed = []
        for _ in range(3):
            removed.append(value.remove())
        self.assertEqual(3, len(removed))
        self.assertEqual(0, len(value))
        removed_types = sorted([v.type_name for v in removed])
        expected_types = sorted(['FloatValue', 'StringValue', 'FloatValue'])
        self.assertEqual(expected_types, removed_types)
        value.close()
    def test_add_duplicates(self):
        value = UnisetValue()
        value.open()
        val1 = FloatValue(42.0)
        value.add(val1)
        value.add(val1)
        value.add(val1)
        self.assertEqual(1, len(value))
        value.add(FloatValue(43.0))
        value.add(StringValue('test'))
        self.assertEqual(3, len(value))
        value.close()
    def test_pop_method(self):
        value = UnisetValue()
        value.open()
        value.add(StringValue('test'))
        popped = value.pop()
        self.assertEqual(StringValue('test'), popped)
        self.assertEqual(0, len(value))
        value.close()
    def test_clear(self):
        value = UnisetValue([FloatValue(1), StringValue('a'), FloatValue(2)])
        self.assertEqual(3, len(value))
        value.clear()
        self.assertEqual(0, len(value))
    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = UnisetValue([StringValue('a'), FloatValue(42)])
        atom.set_value(key, value)
        retrieved = atom.get_value(key)
        self.assertTrue(retrieved.is_a(types.UnisetValue))
        self.assertEqual(2, len(retrieved))
    def test_get_list_of_items_from_value(self):
        value = UnisetValue([FloatValue(42), StringValue('foo')])
        items = value.to_list()
        self.assertEqual(2, len(items))
        item_types = sorted([v.type_name for v in items])
        self.assertEqual(['FloatValue', 'StringValue'], item_types)
    def test_set_with_atoms(self):
        value = UnisetValue()
        value.open()
        a1 = ConceptNode('concept1')
        a2 = ConceptNode('concept2')
        a3 = PredicateNode('predicate1')
        value.add(a1)
        value.add(a2)
        value.add(a3)
        self.assertEqual(3, len(value))
        value.add(a1)
        self.assertEqual(3, len(value))
        value.close()
    def test_pop_empty_set(self):
        value = UnisetValue()
        value.open()
        value.close()
        with self.assertRaises(RuntimeError) as cm:
            value.pop()
        self.assertIn('Cannot remove from closed empty set', str(cm.exception))
    def test_pop_until_empty_then_close(self):
        value = UnisetValue()
        value.open()
        value.add(FloatValue(42))
        popped = value.pop()
        self.assertEqual(FloatValue(42), popped)
        self.assertEqual(0, len(value))
        value.close()
        with self.assertRaises(RuntimeError):
            value.pop()
    def test_str(self):
        value = UnisetValue([FloatValue(42), StringValue('foo')])
        str_repr = str(value)
        self.assertIn('UnisetValue', str_repr)
    def test_is_a(self):
        value = UnisetValue([FloatValue(42), StringValue('foo')])
        self.assertEqual(types.UnisetValue, value.type)
        self.assertEqual('UnisetValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
        self.assertTrue(value.is_a(types.UnisetValue))
    def test_create_with_duplicates_in_list(self):
        val1 = FloatValue(42.0)
        val2 = StringValue('test')
        value = UnisetValue([val1, val2, val1, val2, val1])
        self.assertEqual(2, len(value))
        items = value.to_list()
        item_types = sorted([v.type_name for v in items])
        self.assertEqual(['FloatValue', 'StringValue'], item_types)
if __name__ == '__main__':
    unittest.main()