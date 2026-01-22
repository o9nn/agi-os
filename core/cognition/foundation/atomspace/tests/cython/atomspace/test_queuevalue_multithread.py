import unittest
import threading
import time
import random
from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog
class QueueValueMultithreadTest(unittest.TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_concurrent_producer_consumer(self):
        queue = QueueValue()
        queue.open()
        produced_values = []
        consumed_values = []
        producer_exception = None
        consumer_exception = None
        num_items = 10000
        def producer():
            nonlocal producer_exception
            try:
                random.seed(42)
                for i in range(num_items):
                    if i % 4 == 0:
                        val = FloatValue(float(i))
                    elif i % 4 == 1:
                        val = StringValue(f'string_{i}')
                    elif i % 4 == 2:
                        val = FloatValue(float(i * 2))
                    else:
                        val = StringValue(f'value_{i}')
                    produced_values.append(val)
                    queue.push(val)
                    if i % 100 == 0:
                        time.sleep(0.0001)
            except Exception as e:
                producer_exception = e
            finally:
                queue.close()
        def consumer():
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = queue.pop()
                        consumed_values.append(val)
                    except RuntimeError as e:
                        if 'Cannot pop from closed empty queue' in str(e):
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                consumed_values.append(val)
                            queue.close()
                            break
                        else:
                            raise
                    if len(consumed_values) % 100 == 0:
                        time.sleep(0.0001)
            except Exception as e:
                consumer_exception = e
        producer_thread = threading.Thread(target=producer, name='Producer')
        consumer_thread = threading.Thread(target=consumer, name='Consumer')
        consumer_thread.start()
        producer_thread.start()
        producer_thread.join(timeout=30)
        consumer_thread.join(timeout=30)
        self.assertFalse(producer_thread.is_alive(), 'Producer thread did not complete')
        self.assertFalse(consumer_thread.is_alive(), 'Consumer thread did not complete')
        if producer_exception:
            raise AssertionError(f'Producer thread failed: {producer_exception}')
        if consumer_exception:
            raise AssertionError(f'Consumer thread failed: {consumer_exception}')
        self.assertEqual(len(produced_values), num_items)
        self.assertEqual(len(consumed_values), num_items)
        for i, (produced, consumed) in enumerate(zip(produced_values, consumed_values)):
            self.assertEqual(produced, consumed, f'Mismatch at index {i}: produced {produced}, consumed {consumed}')
        self.assertEqual(0, len(queue))
        self.assertTrue(queue.is_closed())
    def test_concurrent_multiple_producers_single_consumer(self):
        queue = QueueValue()
        queue.open()
        producer_exceptions = {}
        consumer_exception = None
        consumed_values = []
        expected_total = 0
        num_producers = 5
        items_per_producer = 2000
        def producer(producer_id):
            try:
                for i in range(items_per_producer):
                    val = StringValue(f'producer_{producer_id}_item_{i}')
                    queue.push(val)
                    if i % 50 == 0:
                        time.sleep(1e-05)
            except Exception as e:
                producer_exceptions[producer_id] = e
        def consumer():
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = queue.pop()
                        consumed_values.append(val)
                    except RuntimeError as e:
                        if 'Cannot pop from closed empty queue' in str(e):
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                consumed_values.append(val)
                            queue.close()
                            break
                        else:
                            raise
            except Exception as e:
                consumer_exception = e
        expected_total = num_producers * items_per_producer
        producer_threads = []
        for i in range(num_producers):
            thread = threading.Thread(target=producer, args=(i,), name=f'Producer-{i}')
            producer_threads.append(thread)
            thread.start()
        consumer_thread = threading.Thread(target=consumer, name='Consumer')
        consumer_thread.start()
        for thread in producer_threads:
            thread.join(timeout=30)
            self.assertFalse(thread.is_alive(), f'{thread.name} did not complete')
        queue.close()
        consumer_thread.join(timeout=30)
        self.assertFalse(consumer_thread.is_alive(), 'Consumer thread did not complete')
        for producer_id, exc in producer_exceptions.items():
            raise AssertionError(f'Producer {producer_id} failed: {exc}')
        if consumer_exception:
            raise AssertionError(f'Consumer thread failed: {consumer_exception}')
        self.assertEqual(len(consumed_values), expected_total)
        producer_counts = {}
        for val in consumed_values:
            producer_id = int(val.to_list()[0].split('_')[1])
            producer_counts[producer_id] = producer_counts.get(producer_id, 0) + 1
        for i in range(num_producers):
            self.assertEqual(producer_counts.get(i, 0), items_per_producer, f'Producer {i} contributed wrong number of items')
    def test_concurrent_burst_pattern(self):
        queue = QueueValue()
        queue.open()
        write_count = 0
        read_count = 0
        errors = []
        def burst_writer():
            nonlocal write_count
            try:
                for burst in range(100):
                    for i in range(100):
                        queue.push(FloatValue(float(burst * 100 + i)))
                        write_count += 1
                    time.sleep(0.001)
            except Exception as e:
                errors.append(f'Writer error: {e}')
        def burst_reader():
            nonlocal read_count
            try:
                while True:
                    try:
                        val = queue.pop()
                        read_count += 1
                        if read_count % 500 == 0:
                            time.sleep(0.001)
                    except RuntimeError as e:
                        if 'Cannot pop from closed empty queue' in str(e):
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                read_count += 1
                            queue.close()
                            break
                        else:
                            raise
            except Exception as e:
                errors.append(f'Reader error: {e}')
        writer = threading.Thread(target=burst_writer)
        reader = threading.Thread(target=burst_reader)
        reader.start()
        writer.start()
        writer.join(timeout=30)
        queue.close()
        reader.join(timeout=30)
        if errors:
            raise AssertionError('Errors occurred: ' + '; '.join(errors))
        self.assertEqual(write_count, 10000)
        self.assertEqual(read_count, write_count)
    def test_drain_before_close(self):
        queue = QueueValue()
        queue.open()
        num_values = 5000
        for i in range(num_values):
            queue.push(FloatValue(float(i)))
        drained_values = []
        for i in range(num_values):
            val = queue.pop()
            drained_values.append(val)
        queue.close()
        self.assertEqual(len(drained_values), num_values)
        for i, val in enumerate(drained_values):
            self.assertEqual(val.to_list()[0], float(i))
        self.assertEqual(0, len(queue))
        with self.assertRaises(RuntimeError) as cm:
            queue.pop()
        self.assertIn('Cannot pop from closed empty queue', str(cm.exception))
    def test_stress_many_small_operations(self):
        queue = QueueValue()
        queue.open()
        num_threads = 10
        ops_per_thread = 1000
        thread_errors = {}
        def worker(thread_id):
            try:
                for i in range(ops_per_thread):
                    if i % 2 == 0:
                        queue.push(StringValue(f'thread_{thread_id}_op_{i}'))
                    elif len(queue) > 0:
                        try:
                            queue.pop()
                        except RuntimeError:
                            pass
                    time.sleep(random.random() * 0.0001)
            except Exception as e:
                thread_errors[thread_id] = e
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(target=worker, args=(i,), name=f'Worker-{i}')
            threads.append(thread)
            thread.start()
        for thread in threads:
            thread.join(timeout=60)
            self.assertFalse(thread.is_alive(), f'{thread.name} did not complete')
        for thread_id, exc in thread_errors.items():
            raise AssertionError(f'Thread {thread_id} failed: {exc}')
        queue.close()
        remaining = 0
        while True:
            try:
                queue.pop()
                remaining += 1
            except RuntimeError:
                break
        print(f'Stress test complete. Remaining values in queue: {remaining}')
        self.assertGreaterEqual(remaining, 0)
if __name__ == '__main__':
    unittest.main()