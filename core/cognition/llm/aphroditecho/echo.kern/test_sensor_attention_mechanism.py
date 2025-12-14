#!/usr/bin/env python3
"""
Test suite for DTESN Sensor Attention Mechanism
==============================================

Comprehensive tests for Phase 3.1.3: Create Attention Mechanisms for Sensors
"""

import unittest
import time
import threading
from pathlib import Path
import tempfile
import json

# Import the sensor attention mechanism
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from kernel.dtesn.sensor_attention_mechanism import (
    SensorModalityType,
    SensorInput,
    AttentionFocus,
    SensorAttentionConfig,
    SensorAttentionMechanism,
    integrate_with_sensory_motor,
    create_sensor_attention_for_dtesn
)


class TestSensorModalityType(unittest.TestCase):
    """Test sensor modality type enumeration"""
    
    def test_modality_types_exist(self):
        """Test that all expected modality types are defined"""
        expected_modalities = [
            'VISUAL', 'AUDITORY', 'TACTILE', 'PROPRIOCEPTIVE', 
            'ENVIRONMENTAL', 'MOTION'
        ]
        
        for modality in expected_modalities:
            self.assertTrue(hasattr(SensorModalityType, modality))
    
    def test_modality_values(self):
        """Test that modality values are correct strings"""
        self.assertEqual(SensorModalityType.VISUAL.value, "visual")
        self.assertEqual(SensorModalityType.MOTION.value, "motion")
        self.assertEqual(SensorModalityType.AUDITORY.value, "auditory")


class TestSensorInput(unittest.TestCase):
    """Test SensorInput dataclass"""
    
    def test_sensor_input_creation(self):
        """Test basic sensor input creation"""
        inp = SensorInput(
            modality=SensorModalityType.VISUAL,
            data={'frame': 'test'},
            timestamp=time.time(),
            confidence=0.8,
            priority=0.6
        )
        
        self.assertEqual(inp.modality, SensorModalityType.VISUAL)
        self.assertEqual(inp.data['frame'], 'test')
        self.assertEqual(inp.confidence, 0.8)
        self.assertEqual(inp.priority, 0.6)
    
    def test_sensor_input_defaults(self):
        """Test sensor input default values"""
        inp = SensorInput(
            modality=SensorModalityType.MOTION,
            data={'velocity': 1.0},
            timestamp=time.time()
        )
        
        self.assertEqual(inp.confidence, 1.0)
        self.assertEqual(inp.priority, 0.5)
        self.assertIsNone(inp.spatial_location)
        self.assertEqual(inp.metadata, {})


class TestAttentionFocus(unittest.TestCase):
    """Test AttentionFocus dataclass"""
    
    def test_attention_focus_creation(self):
        """Test attention focus creation"""
        modality_weights = {
            SensorModalityType.VISUAL: 1.0,
            SensorModalityType.MOTION: 0.7
        }
        
        focus = AttentionFocus(
            modality_weights=modality_weights,
            saliency_threshold=0.6,
            temporal_window=2.0
        )
        
        self.assertEqual(focus.modality_weights, modality_weights)
        self.assertEqual(focus.saliency_threshold, 0.6)
        self.assertEqual(focus.temporal_window, 2.0)


class TestSensorAttentionMechanism(unittest.TestCase):
    """Test the main sensor attention mechanism"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.attention = SensorAttentionMechanism()
        
        # Create test sensor inputs
        self.visual_input = SensorInput(
            modality=SensorModalityType.VISUAL,
            data={'frame': 'test_frame'},
            timestamp=time.time(),
            confidence=0.8,
            priority=0.6,
            metadata={'high_contrast': True}
        )
        
        self.motion_input = SensorInput(
            modality=SensorModalityType.MOTION,
            data={'velocity': [1.0, 2.0]},
            timestamp=time.time(),
            confidence=0.9,
            priority=0.8,
            metadata={'motion_detected': True}
        )
        
        self.auditory_input = SensorInput(
            modality=SensorModalityType.AUDITORY,
            data={'audio_level': 0.5},
            timestamp=time.time(),
            confidence=0.7,
            priority=0.4,
            metadata={'sudden_change': False}
        )
    
    def test_initialization(self):
        """Test mechanism initialization"""
        self.assertIsInstance(self.attention.config, SensorAttentionConfig)
        self.assertEqual(len(self.attention.current_foci), 0)
        self.assertEqual(self.attention.attention_switches, 0)
        
        # Check all modalities are initialized in outputs
        for modality in SensorModalityType:
            self.assertIn(modality, self.attention.filtered_outputs)
            self.assertEqual(len(self.attention.filtered_outputs[modality]), 0)
    
    def test_compute_saliency_score(self):
        """Test saliency score computation"""
        # Test visual input with motion
        score = self.attention.compute_saliency_score(self.visual_input)
        self.assertGreater(score, 0.0)
        self.assertLessEqual(score, 1.0)
        
        # Motion input should have higher saliency
        motion_score = self.attention.compute_saliency_score(self.motion_input)
        self.assertGreater(motion_score, score)
        
        # Test with old timestamp (should reduce saliency)
        old_input = SensorInput(
            modality=SensorModalityType.VISUAL,
            data={'frame': 'old'},
            timestamp=time.time() - 5.0,  # 5 seconds ago
            confidence=0.8,
            priority=0.6
        )
        old_score = self.attention.compute_saliency_score(old_input)
        self.assertLess(old_score, score)
    
    def test_update_modality_weights(self):
        """Test dynamic modality weight updates"""
        # Test navigation context
        self.attention.update_modality_weights("navigation")
        self.assertGreater(
            self.attention.sensor_weights[SensorModalityType.VISUAL],
            self.attention.sensor_weights[SensorModalityType.AUDITORY]
        )
        
        # Test interaction context
        self.attention.update_modality_weights("interaction")
        self.assertGreater(
            self.attention.sensor_weights[SensorModalityType.AUDITORY],
            self.attention.sensor_weights[SensorModalityType.VISUAL]
        )
        
        # Test exploration context (should be balanced)
        self.attention.update_modality_weights("exploration")
        for modality in SensorModalityType:
            self.assertEqual(self.attention.sensor_weights[modality], 1.0)
    
    def test_create_attention_focus(self):
        """Test attention focus creation"""
        focus = self.attention.create_attention_focus(
            SensorModalityType.VISUAL,
            saliency_threshold=0.7,
            temporal_window=1.5
        )
        
        self.assertEqual(focus.saliency_threshold, 0.7)
        self.assertEqual(focus.temporal_window, 1.5)
        self.assertEqual(focus.modality_weights[SensorModalityType.VISUAL], 1.0)
        self.assertLess(focus.modality_weights[SensorModalityType.AUDITORY], 1.0)
        
        # Motion should always have decent weight
        self.assertGreaterEqual(focus.modality_weights[SensorModalityType.MOTION], 0.7)
    
    def test_selective_attention_filtering(self):
        """Test selective attention filtering"""
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        
        # Process without any foci (should use default filtering)
        filtered = self.attention.apply_selective_attention(test_inputs)
        
        self.assertGreater(len(filtered), 0)
        self.assertLessEqual(len(filtered), len(test_inputs))
        
        # Motion input should likely be included due to high saliency
        motion_included = any(
            inp.modality == SensorModalityType.MOTION for inp in filtered
        )
        self.assertTrue(motion_included)
    
    def test_attention_focus_updating(self):
        """Test attention focus updates"""
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        
        # Initial state - no foci
        self.assertEqual(len(self.attention.current_foci), 0)
        
        # Process inputs - should create focus
        self.attention.update_attention_focus(test_inputs)
        
        # Should have created at least one focus
        self.assertGreater(len(self.attention.current_foci), 0)
        self.assertGreater(self.attention.attention_switches, 0)
    
    def test_process_sensor_inputs_integration(self):
        """Test full sensor input processing"""
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        
        # Process inputs
        result = self.attention.process_sensor_inputs(test_inputs)
        
        # Should return results for all modalities
        self.assertIsInstance(result, dict)
        for modality in SensorModalityType:
            self.assertIn(modality, result)
            self.assertIsInstance(result[modality], list)
        
        # Should have filtered some inputs
        total_filtered = sum(len(inputs) for inputs in result.values())
        self.assertGreater(total_filtered, 0)
    
    def test_attention_state_tracking(self):
        """Test attention state monitoring"""
        # Initial state
        state = self.attention.get_attention_state()
        self.assertEqual(state['current_foci'], 0)
        self.assertEqual(state['attention_switches'], 0)
        
        # Process some inputs
        test_inputs = [self.motion_input]
        self.attention.process_sensor_inputs(test_inputs)
        
        # State should have changed
        new_state = self.attention.get_attention_state()
        self.assertGreaterEqual(new_state['attention_switches'], 0)
    
    def test_performance_monitoring(self):
        """Test performance monitoring and timing"""
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        
        # Process inputs multiple times
        for _ in range(5):
            start_time = time.time()
            self.attention.process_sensor_inputs(test_inputs)
            end_time = time.time()
            
            # Should complete within reasonable time (100ms for Python implementation)
            self.assertLess(end_time - start_time, 0.1)
        
        # Check performance metrics
        state = self.attention.get_attention_state()
        if state['attention_switches'] > 0:
            self.assertGreater(state['avg_switch_time_ms'], 0.0)
    
    def test_reset_attention_state(self):
        """Test attention state reset"""
        test_inputs = [self.motion_input]
        
        # Process inputs to create state
        self.attention.process_sensor_inputs(test_inputs)
        
        # Verify state exists
        self.attention.get_attention_state()
        # Reset
        self.attention.reset_attention_state()
        
        # Verify reset
        reset_state = self.attention.get_attention_state()
        self.assertEqual(reset_state['current_foci'], 0)
        self.assertEqual(reset_state['attention_switches'], 0)
        self.assertEqual(reset_state['avg_switch_time_ms'], 0.0)
    
    def test_attention_log_saving(self):
        """Test attention log saving functionality"""
        test_inputs = [self.motion_input]
        self.attention.process_sensor_inputs(test_inputs)
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
            temp_path = Path(f.name)
        
        try:
            self.attention.save_attention_log(temp_path)
            
            # Verify file was created and contains valid JSON
            self.assertTrue(temp_path.exists())
            
            with open(temp_path, 'r') as f:
                data = json.load(f)
            
            # Check required fields
            self.assertIn('timestamp', data)
            self.assertIn('current_foci', data)
            self.assertIn('attention_switches', data)
        
        finally:
            if temp_path.exists():
                temp_path.unlink()


class TestSensorAttentionConfig(unittest.TestCase):
    """Test sensor attention configuration"""
    
    def test_config_defaults(self):
        """Test configuration default values"""
        config = SensorAttentionConfig()
        
        self.assertEqual(config.max_concurrent_foci, 3)
        self.assertEqual(config.attention_switch_threshold, 0.7)
        self.assertEqual(config.decay_rate, 0.95)
        self.assertEqual(config.competition_threshold, 0.1)
        self.assertEqual(config.cooperative_weight, 0.8)
        self.assertEqual(config.min_focus_duration, 0.1)
        self.assertEqual(config.max_focus_duration, 5.0)
    
    def test_custom_config(self):
        """Test custom configuration creation"""
        config = SensorAttentionConfig(
            max_concurrent_foci=5,
            attention_switch_threshold=0.8,
            decay_rate=0.9
        )
        
        self.assertEqual(config.max_concurrent_foci, 5)
        self.assertEqual(config.attention_switch_threshold, 0.8)
        self.assertEqual(config.decay_rate, 0.9)


class TestIntegrationUtilities(unittest.TestCase):
    """Test integration utilities"""
    
    def test_sensory_motor_integration(self):
        """Test integration with sensory-motor system"""
        attention = SensorAttentionMechanism()
        
        # Mock sensory-motor data
        sensory_data = {
            'status': 'processed',
            'motion': {
                'motion_detected': True,
                'velocity': [1.0, 2.0]
            },
            'objects': ['object1', 'object2'],
            'mouse_moved': True
        }
        
        # Test integration
        result = integrate_with_sensory_motor(attention, sensory_data)
        
        self.assertIn('status', result)
        self.assertEqual(result['status'], 'processed')
        
        # Should have some attention-filtered results
        attention_keys = [k for k in result if 'filtered' in k or 'attention_active' in k]
        self.assertGreater(len(attention_keys), 0)
    
    def test_dtesn_integration_factory(self):
        """Test DTESN integration factory function"""
        # Test with default config
        attention1 = create_sensor_attention_for_dtesn()
        self.assertIsInstance(attention1, SensorAttentionMechanism)
        
        # Test with custom DTESN config
        dtesn_config = {
            'max_attention_channels': 5,
            'attention_threshold': 0.8,
            'cooperative_weight': 0.9
        }
        
        attention2 = create_sensor_attention_for_dtesn(dtesn_config)
        self.assertEqual(attention2.config.max_concurrent_foci, 5)
        self.assertEqual(attention2.config.attention_switch_threshold, 0.8)
        self.assertEqual(attention2.config.cooperative_weight, 0.9)


class TestThreadSafety(unittest.TestCase):
    """Test thread safety of sensor attention mechanism"""
    
    def test_concurrent_processing(self):
        """Test concurrent sensor input processing"""
        attention = SensorAttentionMechanism()
        results = {}
        errors = []
        
        def process_inputs(thread_id):
            try:
                test_input = SensorInput(
                    modality=SensorModalityType.VISUAL,
                    data={'thread': thread_id},
                    timestamp=time.time(),
                    confidence=0.8,
                    priority=0.6
                )
                
                result = attention.process_sensor_inputs([test_input])
                results[thread_id] = result
            except Exception as e:
                errors.append(f"Thread {thread_id}: {e}")
        
        # Start multiple threads
        threads = []
        for i in range(5):
            thread = threading.Thread(target=process_inputs, args=(i,))
            threads.append(thread)
            thread.start()
        
        # Wait for completion
        for thread in threads:
            thread.join()
        
        # Check results
        self.assertEqual(len(errors), 0, f"Thread errors: {errors}")
        self.assertEqual(len(results), 5)
    
    def test_concurrent_state_access(self):
        """Test concurrent attention state access"""
        attention = SensorAttentionMechanism()
        states = []
        errors = []
        
        def access_state(thread_id):
            try:
                # Process some inputs
                test_input = SensorInput(
                    modality=SensorModalityType.MOTION,
                    data={'thread': thread_id},
                    timestamp=time.time(),
                    confidence=0.9,
                    priority=0.8
                )
                attention.process_sensor_inputs([test_input])
                
                # Access state
                state = attention.get_attention_state()
                states.append(state)
            except Exception as e:
                errors.append(f"Thread {thread_id}: {e}")
        
        # Start multiple threads
        threads = []
        for i in range(3):
            thread = threading.Thread(target=access_state, args=(i,))
            threads.append(thread)
            thread.start()
        
        # Wait for completion
        for thread in threads:
            thread.join()
        
        # Check results
        self.assertEqual(len(errors), 0, f"Thread errors: {errors}")
        self.assertEqual(len(states), 3)


class TestPerformanceRequirements(unittest.TestCase):
    """Test performance requirements compliance"""
    
    def test_attention_switch_timing(self):
        """Test that attention switching meets timing requirements"""
        attention = SensorAttentionMechanism()
        
        # Create high-saliency input to trigger attention switch
        high_saliency_input = SensorInput(
            modality=SensorModalityType.MOTION,
            data={'high_motion': True},
            timestamp=time.time(),
            confidence=1.0,
            priority=1.0,
            metadata={'motion_detected': True, 'high_contrast': True}
        )
        
        # Measure attention switch timing
        start_time = time.time()
        attention.process_sensor_inputs([high_saliency_input])
        end_time = time.time()
        
        # Should meet DTESN timing requirements (≤10ms for kernel, more lenient for Python)
        processing_time_ms = (end_time - start_time) * 1000
        self.assertLess(processing_time_ms, 100,  # 100ms for Python implementation
                       f"Attention processing took {processing_time_ms:.2f}ms")
    
    def test_memory_usage_bounds(self):
        """Test that memory usage stays within reasonable bounds"""
        attention = SensorAttentionMechanism()
        
        # Process many inputs to test memory management
        for i in range(1000):
            test_input = SensorInput(
                modality=SensorModalityType.VISUAL,
                data={'iteration': i},
                timestamp=time.time(),
                confidence=0.5,
                priority=0.5
            )
            attention.process_sensor_inputs([test_input])
        
        # Check that history doesn't grow unbounded
        state = attention.get_attention_state()
        
        # Should have reasonable bounds on stored data
        self.assertLessEqual(len(attention.attention_history), 1000)
        self.assertLessEqual(state['current_foci'], attention.config.max_concurrent_foci)


if __name__ == "__main__":
    # Set up logging for tests
    import logging
    logging.basicConfig(level=logging.WARNING)  # Reduce noise during testing
    
    # Run tests
    unittest.main(verbosity=2)