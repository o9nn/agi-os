import unittest
import threading
import time
import random
from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog
class UnisetValueMultithreadTest(unittest.TestCase):
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)
    def tearDown(self):
        finalize_opencog()
        del self.space
    def test_concurrent_producer_consumer(self):
        uniset = UnisetValue()
        uniset.open()
        produced_values = set()
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
                    produced_values.add(str(val))
                    uniset.add(val)
                    if i % 100 == 0:
                        time.sleep(0.0001)
            except Exception as e:
                producer_exception = e
            finally:
                uniset.close()
        def consumer():
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = uniset.pop()
                        consumed_values.append(str(val))
                    except RuntimeError as e:
                        if 'Cannot remove from closed empty set' in str(e):
                            uniset.open()
                            remaining = len(uniset)
                            for _ in range(remaining):
                                val = uniset.pop()
                                consumed_values.append(str(val))
                            uniset.close()
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
        consumed_set = set(consumed_values)
        self.assertEqual(len(produced_values), len(consumed_set))
        self.assertEqual(produced_values, consumed_set)
        self.assertEqual(0, len(uniset))
        self.assertTrue(uniset.is_closed())
    def test_concurrent_multiple_producers_single_consumer(self):
        uniset = UnisetValue()
        uniset.open()
        producer_exceptions = {}
        consumer_exception = None
        consumed_values = []
        produced_values = set()
        produced_lock = threading.Lock()
        num_producers = 5
        items_per_producer = 2000
        def producer(producer_id):
            try:
                for i in range(items_per_producer):
                    val = StringValue(f'producer_{producer_id}_item_{i}')
                    uniset.add(val)
                    with produced_lock:
                        produced_values.add(str(val))
                    if i % 50 == 0:
                        time.sleep(1e-05)
            except Exception as e:
                producer_exceptions[producer_id] = e
        def consumer():
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = uniset.pop()
                        consumed_values.append(str(val))
                    except RuntimeError as e:
                        if 'Cannot remove from closed empty set' in str(e):
                            uniset.open()
                            remaining = len(uniset)
                            for _ in range(remaining):
                                val = uniset.pop()
                                consumed_values.append(str(val))
                            uniset.close()
                            break
                        else:
                            raise
            except Exception as e:
                consumer_exception = e
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
        uniset.close()
        consumer_thread.join(timeout=30)
        self.assertFalse(consumer_thread.is_alive(), 'Consumer thread did not complete')
        for producer_id, exc in producer_exceptions.items():
            raise AssertionError(f'Producer {producer_id} failed: {exc}')
        if consumer_exception:
            raise AssertionError(f'Consumer thread failed: {consumer_exception}')
        consumed_set = set(consumed_values)
        self.assertEqual(len(produced_values), len(consumed_set))
        self.assertEqual(produced_values, consumed_set)
    def test_concurrent_add_duplicates(self):
        uniset = UnisetValue()
        uniset.open()
        errors = []
        add_count = 0
        lock = threading.Lock()
        shared_values = [FloatValue(float(i)) for i in range(10)]
        def adder(thread_id):
            nonlocal add_count
            try:
                for i in range(1000):
                    for val in shared_values:
                        uniset.add(val)
                        with lock:
                            add_count += 1
                    if i % 100 == 0:
                        time.sleep(0.0001)
            except Exception as e:
                errors.append(f'Thread {thread_id} error: {e}')
        threads = []
        for i in range(5):
            thread = threading.Thread(target=adder, args=(i,))
            threads.append(thread)
            thread.start()
        for thread in threads:
            thread.join(timeout=30)
        if errors:
            raise AssertionError('Errors occurred: ' + '; '.join(errors))
        self.assertEqual(10, len(uniset))
        values = []
        for _ in range(10):
            values.append(uniset.pop())
        values.sort(key=lambda v: v.to_list()[0])
        for i, val in enumerate(values):
            self.assertEqual(float(i), val.to_list()[0])
        uniset.close()
        self.assertEqual(5 * 1000 * 10, add_count)
        print(f'Added same 10 objects {add_count} times, set size remained 10')
    def test_stress_many_threads(self):
        uniset = UnisetValue()
        uniset.open()
        num_threads = 10
        ops_per_thread = 1000
        thread_errors = {}
        def worker(thread_id):
            try:
                for i in range(ops_per_thread):
                    if i % 2 == 0:
                        uniset.add(StringValue(f'thread_{thread_id}_op_{i}'))
                    elif len(uniset) > 0:
                        try:
                            uniset.pop()
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
        uniset.close()
        remaining = 0
        uniset.open()
        while len(uniset) > 0:
            uniset.pop()
            remaining += 1
        uniset.close()
        print(f'Stress test complete. Remaining values in set: {remaining}')
        self.assertGreaterEqual(remaining, 0)
        self.assertLessEqual(remaining, num_threads * (ops_per_thread // 2))
if __name__ == '__main__':
    unittest.main()