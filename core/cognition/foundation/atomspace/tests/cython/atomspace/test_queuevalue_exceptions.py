import unittest
from opencog.atomspace import AtomSpace
from opencog.utilities import set_default_atomspace
from opencog.type_constructors import *
class TestQueueValueExceptions(unittest.TestCase):
    def setUp(self):
        self.atomspace = AtomSpace()
        set_default_atomspace(self.atomspace)
    def tearDown(self):
        self.atomspace = None
    def test_push_to_closed_queue_raises_exception(self):
        q = QueueValue()
        q.close()
        with self.assertRaises(RuntimeError) as context:
            q.push(FloatValue(3.14))
        self.assertEqual(str(context.exception), 'std::exception')
    def test_append_to_closed_queue_raises_exception(self):
        q = QueueValue()
        q.close()
        with self.assertRaises(RuntimeError) as context:
            q.append(StringValue('test'))
        self.assertEqual(str(context.exception), 'std::exception')
    def test_push_to_open_queue_succeeds(self):
        q = QueueValue()
        q.open()
        try:
            q.push(FloatValue(1.0))
            q.push(StringValue('hello'))
            q.push(FloatValue(2.0))
        except Exception as e:
            self.fail(f'Pushing to open queue raised unexpected exception: {e}')
        q.close()
        self.assertEqual(len(q), 3)
    def test_queue_with_initial_values_push_behavior(self):
        q = QueueValue([FloatValue(1.0), StringValue('initial')])
        with self.assertRaises(RuntimeError) as context:
            q.push(FloatValue(2.0))
        self.assertEqual(str(context.exception), 'std::exception')
        q.open()
        q.push(FloatValue(3.0))
        q.close()
        self.assertEqual(len(q), 3)
    def test_pop_from_closed_empty_queue(self):
        q = QueueValue()
        q.open()
        q.close()
        with self.assertRaises(RuntimeError) as context:
            q.pop()
        self.assertIn('Cannot pop from closed empty queue', str(context.exception))
if __name__ == '__main__':
    unittest.main()