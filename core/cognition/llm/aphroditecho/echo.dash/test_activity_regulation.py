import unittest
import logging
import sys
import time
from unittest.mock import Mock
from pathlib import Path
from enum import Enum
sys.path.insert(0, str(Path(__file__).parent))
try:
    from activity_regulation import ActivityRegulator, ActivityState, TaskPriority, ScheduledTask
    ACTIVITY_REGULATION_AVAILABLE = True
except ImportError as e:
    ACTIVITY_REGULATION_AVAILABLE = False
    print(f'Warning: Could not import activity_regulation: {e}')
class TestActivityRegulation(unittest.TestCase):
    def setUp(self):
        logging.getLogger().setLevel(logging.CRITICAL)
    def tearDown(self):
        pass
    def test_import_activity_regulation(self):
        if not ACTIVITY_REGULATION_AVAILABLE:
            self.skipTest('activity_regulation module not available')
        self.assertTrue(ACTIVITY_REGULATION_AVAILABLE)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_state_enum(self):
        expected_states = ['ACTIVE', 'RESTING', 'DORMANT', 'PROCESSING', 'WAITING']
        for state_name in expected_states:
            if hasattr(ActivityState, state_name):
                state = getattr(ActivityState, state_name)
                self.assertIsInstance(state, ActivityState)
                self.assertEqual(state.name, state_name)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_task_priority_enum(self):
        expected_priorities = ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW', 'BACKGROUND']
        for priority_name in expected_priorities:
            if hasattr(TaskPriority, priority_name):
                priority = getattr(TaskPriority, priority_name)
                self.assertIsInstance(priority, TaskPriority)
                self.assertEqual(priority.name, priority_name)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_task_priority_values(self):
        if hasattr(TaskPriority, 'CRITICAL') and hasattr(TaskPriority, 'HIGH'):
            self.assertLess(TaskPriority.CRITICAL.value, TaskPriority.HIGH.value)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_scheduled_task_creation(self):
        mock_callback = Mock()
        task = ScheduledTask(priority=TaskPriority.HIGH, scheduled_time=time.time(), task_id='test_task', callback=mock_callback)
        self.assertEqual(task.priority, TaskPriority.HIGH)
        self.assertEqual(task.task_id, 'test_task')
        self.assertEqual(task.callback, mock_callback)
        self.assertIsInstance(task.scheduled_time, float)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_scheduled_task_optional_fields(self):
        mock_callback = Mock()
        mock_condition = Mock(return_value=True)
        task = ScheduledTask(priority=TaskPriority.MEDIUM, scheduled_time=time.time(), task_id='optional_test', callback=mock_callback, interval=30.0, condition=mock_condition, cpu_threshold=0.9, memory_threshold=0.8)
        self.assertEqual(task.interval, 30.0)
        self.assertEqual(task.condition, mock_condition)
        self.assertEqual(task.cpu_threshold, 0.9)
        self.assertEqual(task.memory_threshold, 0.8)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_regulator_creation(self):
        regulator = ActivityRegulator()
        self.assertIsNotNone(regulator)
        self.assertTrue(hasattr(regulator, 'logger'))
        self.assertTrue(hasattr(regulator, 'state'))
        self.assertTrue(hasattr(regulator, 'running'))
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_regulator_attributes(self):
        regulator = ActivityRegulator()
        expected_attrs = ['logger', 'state', 'task_queue', 'periodic_tasks', 'event_tasks', 'running']
        for attr in expected_attrs:
            self.assertTrue(hasattr(regulator, attr), f'Missing expected attribute: {attr}')
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_regulator_initial_state(self):
        regulator = ActivityRegulator()
        self.assertEqual(regulator.state, ActivityState.ACTIVE)
        self.assertTrue(regulator.running)
        if hasattr(regulator, 'task_queue'):
            self.assertIsNotNone(regulator.task_queue)
        if hasattr(regulator, 'periodic_tasks'):
            self.assertIsInstance(regulator.periodic_tasks, dict)
        if hasattr(regulator, 'event_tasks'):
            self.assertIsInstance(regulator.event_tasks, dict)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_scheduled_task_ordering(self):
        callback1 = Mock()
        callback2 = Mock()
        task1 = ScheduledTask(priority=TaskPriority.HIGH, scheduled_time=time.time(), task_id='task1', callback=callback1)
        task2 = ScheduledTask(priority=TaskPriority.CRITICAL, scheduled_time=time.time(), task_id='task2', callback=callback2)
        self.assertLess(task2, task1)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_state_string_values(self):
        for state in ActivityState:
            self.assertIsInstance(state.value, str)
            self.assertGreater(len(state.value), 0)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_task_priority_integer_values(self):
        for priority in TaskPriority:
            self.assertIsInstance(priority.value, int)
            self.assertGreaterEqual(priority.value, 0)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_module_imports(self):
        import importlib
        try:
            importlib.reload(activity_regulation)
        except ImportError as e:
            if 'No module named' in str(e):
                self.skipTest(f'Module dependencies not available: {e}')
            else:
                self.fail(f'Module failed to reload: {e}')
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_scheduled_task_defaults(self):
        mock_callback = Mock()
        task = ScheduledTask(priority=TaskPriority.MEDIUM, scheduled_time=time.time(), task_id='default_test', callback=mock_callback)
        self.assertIsNone(task.interval)
        self.assertIsNone(task.condition)
        self.assertIsNone(task.last_run)
        self.assertEqual(task.cpu_threshold, 0.8)
        self.assertEqual(task.memory_threshold, 0.8)
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_activity_regulator_logger_configuration(self):
        regulator = ActivityRegulator()
        self.assertIsNotNone(regulator.logger)
        self.assertEqual(regulator.logger.name, 'activity_regulation')
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_all_enums_accessible(self):
        import activity_regulation as ar_module
        self.assertTrue(hasattr(ar_module, 'ActivityState'))
        self.assertTrue(hasattr(ar_module, 'TaskPriority'))
        self.assertTrue(issubclass(ar_module.ActivityState, Enum))
        self.assertTrue(issubclass(ar_module.TaskPriority, Enum))
    @unittest.skipIf(not ACTIVITY_REGULATION_AVAILABLE, 'activity_regulation not available')
    def test_scheduled_task_comparison_edge_cases(self):
        callback1 = Mock()
        callback2 = Mock()
        current_time = time.time()
        task1 = ScheduledTask(priority=TaskPriority.HIGH, scheduled_time=current_time, task_id='task1', callback=callback1)
        task2 = ScheduledTask(priority=TaskPriority.HIGH, scheduled_time=current_time + 1, task_id='task2', callback=callback2)
        self.assertLess(task1, task2)
def main():
    unittest.main(verbosity=2)
if __name__ == '__main__':
    main()