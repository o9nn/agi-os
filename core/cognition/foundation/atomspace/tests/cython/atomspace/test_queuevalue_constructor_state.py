import unittest
from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog
class QueueValueConstructorStateTest(unittest.TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_empty_queue_is_open(self):
        queue = QueueValue()
        self.assertFalse(queue.is_closed())
        self.assertEqual(0, len(queue))
        queue.push(FloatValue(42.0))
        queue.append(StringValue('test'))
        self.assertEqual(2, len(queue))
    def test_queue_with_list_is_closed(self):
        values = [FloatValue(1.0), StringValue('test'), FloatValue(2.0)]
        queue = QueueValue(values)
        self.assertTrue(queue.is_closed())
        self.assertEqual(3, len(queue))
        contents = queue.to_list()
        self.assertEqual(values, contents)
    def test_queue_with_single_value_is_closed(self):
        value = StringValue('single')
        queue = QueueValue(value)
        self.assertTrue(queue.is_closed())
        self.assertEqual(1, len(queue))
        self.assertEqual([value], queue.to_list())
    def test_reopen_behavior(self):
        queue = QueueValue([FloatValue(1.0), FloatValue(2.0)])
        self.assertTrue(queue.is_closed())
        self.assertEqual(2, len(queue))
        queue.open()
        self.assertFalse(queue.is_closed())
        self.assertEqual(0, len(queue))
    def test_append_requires_open_queue(self):
        queue1 = QueueValue()
        queue1.append(FloatValue(1.0))
        self.assertEqual(1, len(queue1))
        queue2 = QueueValue([FloatValue(1.0)])
        self.assertTrue(queue2.is_closed())
    def test_close_then_read(self):
        queue = QueueValue()
        queue.push(FloatValue(1.0))
        queue.push(FloatValue(2.0))
        queue.push(FloatValue(3.0))
        queue.close()
        self.assertEqual(3, len(queue))
        values = queue.to_list()
        self.assertEqual([FloatValue(1.0), FloatValue(2.0), FloatValue(3.0)], values)