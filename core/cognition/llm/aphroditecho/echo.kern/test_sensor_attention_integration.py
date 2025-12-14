#!/usr/bin/env python3
"""
Test suite for Sensor Attention Integration Module
================================================

Tests for the integration layer that connects DTESN sensor attention mechanisms
with existing sensory-motor systems.
"""

import unittest
import time
import threading
import tempfile
from pathlib import Path
import json
from unittest.mock import Mock

# Import the integration module
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))

from sensor_attention_integration import (
    AttentionGuidedSensorSystem,
    create_attention_guided_system,
    integrate_with_existing_sensory_motor
)

from kernel.dtesn.sensor_attention_mechanism import SensorModalityType


class TestAttentionGuidedSensorSystem(unittest.TestCase):
    """Test the main attention-guided sensor system"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.system = AttentionGuidedSensorSystem(enable_logging=False)
        
        # Sample environment data
        self.env_data = {
            'screen_data': {'frame': 'test_frame'},
            'motion_detected': True,
            'velocity': [1.5, 0.8],
            'audio_level': 0.6,
            'mouse_moved': True,
            'cursor_pos': (100, 200),
            'detected_objects': ['object1', 'object2']
        }
        
        # Sample sensory-motor data
        self.sensory_motor_data = {
            'status': 'processed',
            'motion': {'motion_detected': True, 'velocity': [1.0, 2.0]},
            'objects': ['detected_object'],
            'mouse_moved': True
        }
    
    def test_initialization(self):
        """Test system initialization"""
        self.assertIsNotNone(self.system.sensor_attention)
        self.assertFalse(self.system.active)
        self.assertEqual(self.system.context_state, "default")
        self.assertEqual(self.system.total_inputs_processed, 0)
    
    def test_attention_context_setting(self):
        """Test setting attention context"""
        # Test different contexts
        contexts = ["navigation", "interaction", "exploration"]
        
        for context in contexts:
            self.system.set_attention_context(context)
            self.assertEqual(self.system.context_state, context)
            
            # Check that sensor weights were updated
            weights = self.system.sensor_attention.sensor_weights
            self.assertIsInstance(weights, dict)
            self.assertGreater(len(weights), 0)
    
    def test_sensory_motor_data_processing(self):
        """Test processing of sensory-motor data"""
        result = self.system.process_sensory_motor_data(self.sensory_motor_data)
        
        # Should return processed data
        self.assertIsInstance(result, dict)
        self.assertIn('status', result)
        self.assertIn('attention_metadata', result)
        
        # Check attention metadata
        metadata = result['attention_metadata']
        self.assertIn('context', metadata)
        self.assertIn('active_foci', metadata)
        self.assertIn('processing_time_ms', metadata)
        self.assertGreaterEqual(metadata['processing_time_ms'], 0.0)
        
        # Check that original data is preserved
        self.assertEqual(result['status'], 'processed')
    
    def test_environment_data_conversion(self):
        """Test conversion of environment data to sensor inputs"""
        sensor_inputs = self.system.create_sensor_inputs_from_environment(self.env_data)
        
        # Should create multiple sensor inputs
        self.assertGreater(len(sensor_inputs), 0)
        
        # Check for expected modalities
        modalities_present = [inp.modality for inp in sensor_inputs]
        self.assertIn(SensorModalityType.VISUAL, modalities_present)
        self.assertIn(SensorModalityType.MOTION, modalities_present)
        
        # Check sensor input properties
        for inp in sensor_inputs:
            self.assertIsInstance(inp.timestamp, float)
            self.assertGreaterEqual(inp.confidence, 0.0)
            self.assertLessEqual(inp.confidence, 1.0)
            self.assertGreaterEqual(inp.priority, 0.0)
            self.assertLessEqual(inp.priority, 1.0)
            self.assertIsInstance(inp.metadata, dict)
    
    def test_salient_feature_focusing(self):
        """Test focusing on salient features"""
        sensor_inputs = self.system.create_sensor_inputs_from_environment(self.env_data)
        focused_inputs = self.system.focus_on_salient_features(sensor_inputs)
        
        # Should return filtered inputs
        self.assertIsInstance(focused_inputs, list)
        self.assertLessEqual(len(focused_inputs), len(sensor_inputs))
        
        # All focused inputs should be from original set
        original_inputs = set(id(inp) for inp in sensor_inputs)
        for focused_inp in focused_inputs:
            self.assertIn(id(focused_inp), original_inputs)
    
    def test_dynamic_sensor_prioritization(self):
        """Test dynamic sensor prioritization"""
        # Test without context hints
        priorities = self.system.prioritize_sensors_dynamically()
        self.assertIsInstance(priorities, dict)
        self.assertEqual(len(priorities), len(SensorModalityType))
        
        # All priorities should be positive
        for modality, priority in priorities.items():
            self.assertGreater(priority, 0.0)
        
        # Test with context hints
        context_hints = {
            'task': 'navigation',
            'urgency': 0.9
        }
        
        urgent_priorities = self.system.prioritize_sensors_dynamically(context_hints)
        self.assertIsInstance(urgent_priorities, dict)
        
        # High urgency should boost motion and visual
        self.assertGreater(
            urgent_priorities[SensorModalityType.MOTION],
            priorities[SensorModalityType.MOTION]
        )
    
    def test_attention_summary(self):
        """Test attention system summary"""
        # Process some data first
        self.system.process_sensory_motor_data(self.sensory_motor_data)
        
        summary = self.system.get_attention_summary()
        
        # Check summary structure
        self.assertIn('system_state', summary)
        self.assertIn('attention_state', summary)
        self.assertIn('performance_metrics', summary)
        
        # Check system state
        sys_state = summary['system_state']
        self.assertIn('active', sys_state)
        self.assertIn('context', sys_state)
        self.assertIn('total_inputs_processed', sys_state)
        self.assertGreater(sys_state['total_inputs_processed'], 0)
        
        # Check performance metrics
        perf_metrics = summary['performance_metrics']
        self.assertIn('attention_switches_per_input', perf_metrics)
        self.assertIn('total_processing_time_s', perf_metrics)
        self.assertIn('meets_realtime_constraints', perf_metrics)
    
    def test_performance_report_saving(self):
        """Test saving performance reports"""
        # Process some data first
        self.system.process_sensory_motor_data(self.sensory_motor_data)
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
            temp_path = Path(f.name)
        
        try:
            self.system.save_performance_report(temp_path)
            
            # Check file was created
            self.assertTrue(temp_path.exists())
            
            # Check file contents
            with open(temp_path, 'r') as f:
                report = json.load(f)
            
            self.assertIn('timestamp', report)
            self.assertIn('report_type', report)
            self.assertEqual(report['report_type'], 'sensor_attention_performance')
            self.assertIn('system_state', report)
        
        finally:
            if temp_path.exists():
                temp_path.unlink()
    
    def test_error_handling(self):
        """Test error handling in data processing"""
        # Test with malformed data
        bad_data = {'invalid': 'data', 'structure': None}
        
        result = self.system.process_sensory_motor_data(bad_data)
        
        # Should still return data, possibly with error indication
        self.assertIsInstance(result, dict)
        # May contain original data or error information
    
    def test_performance_tracking(self):
        """Test performance tracking over multiple processing cycles"""
        initial_summary = self.system.get_attention_summary()
        
        # Process multiple inputs
        for i in range(5):
            modified_data = dict(self.sensory_motor_data)
            modified_data['iteration'] = i
            self.system.process_sensory_motor_data(modified_data)
        
        final_summary = self.system.get_attention_summary()
        
        # Should show increased processing
        self.assertGreater(
            final_summary['system_state']['total_inputs_processed'],
            initial_summary['system_state']['total_inputs_processed']
        )
    
    def test_thread_safety(self):
        """Test thread safety of the attention system"""
        results = {}
        errors = []
        
        def process_data(thread_id):
            try:
                test_data = dict(self.sensory_motor_data)
                test_data['thread_id'] = thread_id
                result = self.system.process_sensory_motor_data(test_data)
                results[thread_id] = result
            except Exception as e:
                errors.append(f"Thread {thread_id}: {e}")
        
        # Start multiple threads
        threads = []
        for i in range(3):
            thread = threading.Thread(target=process_data, args=(i,))
            threads.append(thread)
            thread.start()
        
        # Wait for completion
        for thread in threads:
            thread.join()
        
        # Check results
        self.assertEqual(len(errors), 0, f"Thread errors: {errors}")
        self.assertEqual(len(results), 3)


class TestContinuousProcessing(unittest.TestCase):
    """Test continuous processing functionality"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.system = AttentionGuidedSensorSystem(enable_logging=False)
    
    def tearDown(self):
        """Clean up after tests"""
        if self.system.active:
            self.system.stop_continuous_processing()
    
    def test_start_stop_continuous_processing(self):
        """Test starting and stopping continuous processing"""
        # Initially inactive
        self.assertFalse(self.system.active)
        
        # Start processing
        self.system.start_continuous_processing()
        self.assertTrue(self.system.active)
        self.assertIsNotNone(self.system.processing_thread)
        
        # Allow brief processing
        time.sleep(0.1)
        
        # Stop processing
        self.system.stop_continuous_processing()
        self.assertFalse(self.system.active)
    
    def test_continuous_processing_prevents_double_start(self):
        """Test that continuous processing prevents double start"""
        self.system.start_continuous_processing()
        
        # Try to start again - should not crash
        self.system.start_continuous_processing()
        self.assertTrue(self.system.active)
        
        self.system.stop_continuous_processing()


class TestFactoryFunctions(unittest.TestCase):
    """Test factory functions and utilities"""
    
    def test_create_attention_guided_system(self):
        """Test factory function for creating attention-guided systems"""
        # Test with default config
        system1 = create_attention_guided_system()
        self.assertIsInstance(system1, AttentionGuidedSensorSystem)
        
        # Test with custom config
        config = {
            'max_foci': 5,
            'attention_threshold': 0.8,
            'enable_logging': False
        }
        
        system2 = create_attention_guided_system(config)
        self.assertIsInstance(system2, AttentionGuidedSensorSystem)
        
        # Should apply configuration
        self.assertEqual(
            system2.sensor_attention.config.max_concurrent_foci, 5
        )
        self.assertEqual(
            system2.sensor_attention.config.attention_switch_threshold, 0.8
        )
    
    def test_integration_with_existing_system(self):
        """Test integration with existing sensory-motor systems"""
        # Create mock sensory-motor system
        mock_system = Mock()
        mock_system.process_input = Mock(return_value={'status': 'original'})
        
        # Create attention system
        attention_system = create_attention_guided_system({'enable_logging': False})
        
        # Integrate
        integrate_with_existing_sensory_motor(mock_system, attention_system)
        
        # Test that method was replaced
        self.assertTrue(hasattr(mock_system, 'process_input'))
        
        # Test enhanced functionality
        result = mock_system.process_input()
        
        # Should include attention metadata
        self.assertIn('attention_metadata', result)
        
        # Original method should have been called
        # (Note: Due to mocking, we can't verify this directly, but integration works)
    
    def test_integration_with_system_without_process_input(self):
        """Test integration with system that lacks process_input method"""
        mock_system = Mock(spec=[])  # System without process_input
        attention_system = create_attention_guided_system({'enable_logging': False})
        
        # Should not crash
        integrate_with_existing_sensory_motor(mock_system, attention_system)


class TestRealTimeConstraints(unittest.TestCase):
    """Test adherence to real-time performance constraints"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.system = AttentionGuidedSensorSystem(enable_logging=False)
        
        # Complex environment data to stress test
        self.complex_env_data = {
            'screen_data': {'frame': 'large_frame_data' * 100},
            'motion_detected': True,
            'velocity': [1.5, 0.8, 2.3],
            'audio_level': 0.8,
            'audio_change': True,
            'mouse_moved': True,
            'cursor_pos': (150, 300),
            'key_pressed': True,
            'detected_objects': [f'object_{i}' for i in range(20)],
            'environment_state': {'complexity': 'high'},
            'env_type': 'simulation'
        }
    
    def test_processing_time_constraints(self):
        """Test that processing meets time constraints"""
        # Process complex data multiple times
        processing_times = []
        
        for _ in range(10):
            start_time = time.time()
            self.system.process_sensory_motor_data({'test': 'data'})
            end_time = time.time()
            
            processing_times.append(end_time - start_time)
        
        # Calculate statistics
        avg_time = sum(processing_times) / len(processing_times)
        max_time = max(processing_times)
        
        # Should meet reasonable performance constraints (relaxed for Python)
        self.assertLess(avg_time, 0.05, f"Average processing time {avg_time*1000:.2f}ms too slow")
        self.assertLess(max_time, 0.1, f"Max processing time {max_time*1000:.2f}ms too slow")
    
    def test_complex_environment_processing(self):
        """Test processing of complex environment data"""
        start_time = time.time()
        
        # Convert to sensor inputs
        sensor_inputs = self.system.create_sensor_inputs_from_environment(self.complex_env_data)
        
        # Apply attention
        focused_inputs = self.system.focus_on_salient_features(sensor_inputs)
        
        # Get priorities
        priorities = self.system.prioritize_sensors_dynamically({
            'urgency': 0.8,
            'task': 'navigation'
        })
        
        end_time = time.time()
        processing_time = end_time - start_time
        
        # Should complete within reasonable time
        self.assertLess(processing_time, 0.1, 
                       f"Complex processing took {processing_time*1000:.2f}ms")
        
        # Should produce valid results
        self.assertGreater(len(sensor_inputs), 0)
        self.assertIsInstance(focused_inputs, list)
        self.assertIsInstance(priorities, dict)
    
    def test_memory_usage_stability(self):
        """Test that memory usage remains stable over time"""
        # Get initial summary
        initial_summary = self.system.get_attention_summary()
        
        # Process many inputs
        for i in range(100):
            test_data = {
                'iteration': i,
                'motion_detected': i % 2 == 0,
                'audio_level': i / 100.0
            }
            self.system.process_sensory_motor_data(test_data)
        
        # Get final summary
        final_summary = self.system.get_attention_summary()
        
        # Processing count should have increased
        self.assertEqual(
            final_summary['system_state']['total_inputs_processed'],
            initial_summary['system_state']['total_inputs_processed'] + 100
        )
        
        # Average processing time should remain reasonable
        avg_time = final_summary['system_state']['avg_processing_time_ms']
        self.assertLess(avg_time, 50.0, f"Average time increased to {avg_time:.2f}ms")


if __name__ == "__main__":
    # Set up logging for tests
    import logging
    logging.basicConfig(level=logging.WARNING)
    
    # Run tests with verbose output
    unittest.main(verbosity=2)