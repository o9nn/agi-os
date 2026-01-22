import unittest
import time
import threading
from pathlib import Path
import tempfile
import json
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))
from kernel.dtesn.sensor_attention_mechanism import SensorModalityType, SensorInput, AttentionFocus, SensorAttentionConfig, SensorAttentionMechanism, integrate_with_sensory_motor, create_sensor_attention_for_dtesn
class TestSensorModalityType(unittest.TestCase):
    def test_modality_types_exist(self):
        expected_modalities = ['VISUAL', 'AUDITORY', 'TACTILE', 'PROPRIOCEPTIVE', 'ENVIRONMENTAL', 'MOTION']
        for modality in expected_modalities:
            self.assertTrue(hasattr(SensorModalityType, modality))
    def test_modality_values(self):
        self.assertEqual(SensorModalityType.VISUAL.value, 'visual')
        self.assertEqual(SensorModalityType.MOTION.value, 'motion')
        self.assertEqual(SensorModalityType.AUDITORY.value, 'auditory')
class TestSensorInput(unittest.TestCase):
    def test_sensor_input_creation(self):
        inp = SensorInput(modality=SensorModalityType.VISUAL, data={'frame': 'test'}, timestamp=time.time(), confidence=0.8, priority=0.6)
        self.assertEqual(inp.modality, SensorModalityType.VISUAL)
        self.assertEqual(inp.data['frame'], 'test')
        self.assertEqual(inp.confidence, 0.8)
        self.assertEqual(inp.priority, 0.6)
    def test_sensor_input_defaults(self):
        inp = SensorInput(modality=SensorModalityType.MOTION, data={'velocity': 1.0}, timestamp=time.time())
        self.assertEqual(inp.confidence, 1.0)
        self.assertEqual(inp.priority, 0.5)
        self.assertIsNone(inp.spatial_location)
        self.assertEqual(inp.metadata, {})
class TestAttentionFocus(unittest.TestCase):
    def test_attention_focus_creation(self):
        modality_weights = {SensorModalityType.VISUAL: 1.0, SensorModalityType.MOTION: 0.7}
        focus = AttentionFocus(modality_weights=modality_weights, saliency_threshold=0.6, temporal_window=2.0)
        self.assertEqual(focus.modality_weights, modality_weights)
        self.assertEqual(focus.saliency_threshold, 0.6)
        self.assertEqual(focus.temporal_window, 2.0)
class TestSensorAttentionMechanism(unittest.TestCase):
    def setUp(self):
        self.attention = SensorAttentionMechanism()
        self.visual_input = SensorInput(modality=SensorModalityType.VISUAL, data={'frame': 'test_frame'}, timestamp=time.time(), confidence=0.8, priority=0.6, metadata={'high_contrast': True})
        self.motion_input = SensorInput(modality=SensorModalityType.MOTION, data={'velocity': [1.0, 2.0]}, timestamp=time.time(), confidence=0.9, priority=0.8, metadata={'motion_detected': True})
        self.auditory_input = SensorInput(modality=SensorModalityType.AUDITORY, data={'audio_level': 0.5}, timestamp=time.time(), confidence=0.7, priority=0.4, metadata={'sudden_change': False})
    def test_initialization(self):
        self.assertIsInstance(self.attention.config, SensorAttentionConfig)
        self.assertEqual(len(self.attention.current_foci), 0)
        self.assertEqual(self.attention.attention_switches, 0)
        for modality in SensorModalityType:
            self.assertIn(modality, self.attention.filtered_outputs)
            self.assertEqual(len(self.attention.filtered_outputs[modality]), 0)
    def test_compute_saliency_score(self):
        score = self.attention.compute_saliency_score(self.visual_input)
        self.assertGreater(score, 0.0)
        self.assertLessEqual(score, 1.0)
        motion_score = self.attention.compute_saliency_score(self.motion_input)
        self.assertGreater(motion_score, score)
        old_input = SensorInput(modality=SensorModalityType.VISUAL, data={'frame': 'old'}, timestamp=time.time() - 5.0, confidence=0.8, priority=0.6)
        old_score = self.attention.compute_saliency_score(old_input)
        self.assertLess(old_score, score)
    def test_update_modality_weights(self):
        self.attention.update_modality_weights('navigation')
        self.assertGreater(self.attention.sensor_weights[SensorModalityType.VISUAL], self.attention.sensor_weights[SensorModalityType.AUDITORY])
        self.attention.update_modality_weights('interaction')
        self.assertGreater(self.attention.sensor_weights[SensorModalityType.AUDITORY], self.attention.sensor_weights[SensorModalityType.VISUAL])
        self.attention.update_modality_weights('exploration')
        for modality in SensorModalityType:
            self.assertEqual(self.attention.sensor_weights[modality], 1.0)
    def test_create_attention_focus(self):
        focus = self.attention.create_attention_focus(SensorModalityType.VISUAL, saliency_threshold=0.7, temporal_window=1.5)
        self.assertEqual(focus.saliency_threshold, 0.7)
        self.assertEqual(focus.temporal_window, 1.5)
        self.assertEqual(focus.modality_weights[SensorModalityType.VISUAL], 1.0)
        self.assertLess(focus.modality_weights[SensorModalityType.AUDITORY], 1.0)
        self.assertGreaterEqual(focus.modality_weights[SensorModalityType.MOTION], 0.7)
    def test_selective_attention_filtering(self):
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        filtered = self.attention.apply_selective_attention(test_inputs)
        self.assertGreater(len(filtered), 0)
        self.assertLessEqual(len(filtered), len(test_inputs))
        motion_included = any((inp.modality == SensorModalityType.MOTION for inp in filtered))
        self.assertTrue(motion_included)
    def test_attention_focus_updating(self):
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        self.assertEqual(len(self.attention.current_foci), 0)
        self.attention.update_attention_focus(test_inputs)
        self.assertGreater(len(self.attention.current_foci), 0)
        self.assertGreater(self.attention.attention_switches, 0)
    def test_process_sensor_inputs_integration(self):
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        result = self.attention.process_sensor_inputs(test_inputs)
        self.assertIsInstance(result, dict)
        for modality in SensorModalityType:
            self.assertIn(modality, result)
            self.assertIsInstance(result[modality], list)
        total_filtered = sum((len(inputs) for inputs in result.values()))
        self.assertGreater(total_filtered, 0)
    def test_attention_state_tracking(self):
        state = self.attention.get_attention_state()
        self.assertEqual(state['current_foci'], 0)
        self.assertEqual(state['attention_switches'], 0)
        test_inputs = [self.motion_input]
        self.attention.process_sensor_inputs(test_inputs)
        new_state = self.attention.get_attention_state()
        self.assertGreaterEqual(new_state['attention_switches'], 0)
    def test_performance_monitoring(self):
        test_inputs = [self.visual_input, self.motion_input, self.auditory_input]
        for _ in range(5):
            start_time = time.time()
            self.attention.process_sensor_inputs(test_inputs)
            end_time = time.time()
            self.assertLess(end_time - start_time, 0.1)
        state = self.attention.get_attention_state()
        if state['attention_switches'] > 0:
            self.assertGreater(state['avg_switch_time_ms'], 0.0)
    def test_reset_attention_state(self):
        test_inputs = [self.motion_input]
        self.attention.process_sensor_inputs(test_inputs)
        self.attention.get_attention_state()
        self.attention.reset_attention_state()
        reset_state = self.attention.get_attention_state()
        self.assertEqual(reset_state['current_foci'], 0)
        self.assertEqual(reset_state['attention_switches'], 0)
        self.assertEqual(reset_state['avg_switch_time_ms'], 0.0)
    def test_attention_log_saving(self):
        test_inputs = [self.motion_input]
        self.attention.process_sensor_inputs(test_inputs)
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
            temp_path = Path(f.name)
        try:
            self.attention.save_attention_log(temp_path)
            self.assertTrue(temp_path.exists())
            with open(temp_path, 'r') as f:
                data = json.load(f)
            self.assertIn('timestamp', data)
            self.assertIn('current_foci', data)
            self.assertIn('attention_switches', data)
        finally:
            if temp_path.exists():
                temp_path.unlink()
class TestSensorAttentionConfig(unittest.TestCase):
    def test_config_defaults(self):
        config = SensorAttentionConfig()
        self.assertEqual(config.max_concurrent_foci, 3)
        self.assertEqual(config.attention_switch_threshold, 0.7)
        self.assertEqual(config.decay_rate, 0.95)
        self.assertEqual(config.competition_threshold, 0.1)
        self.assertEqual(config.cooperative_weight, 0.8)
        self.assertEqual(config.min_focus_duration, 0.1)
        self.assertEqual(config.max_focus_duration, 5.0)
    def test_custom_config(self):
        config = SensorAttentionConfig(max_concurrent_foci=5, attention_switch_threshold=0.8, decay_rate=0.9)
        self.assertEqual(config.max_concurrent_foci, 5)
        self.assertEqual(config.attention_switch_threshold, 0.8)
        self.assertEqual(config.decay_rate, 0.9)
class TestIntegrationUtilities(unittest.TestCase):
    def test_sensory_motor_integration(self):
        attention = SensorAttentionMechanism()
        sensory_data = {'status': 'processed', 'motion': {'motion_detected': True, 'velocity': [1.0, 2.0]}, 'objects': ['object1', 'object2'], 'mouse_moved': True}
        result = integrate_with_sensory_motor(attention, sensory_data)
        self.assertIn('status', result)
        self.assertEqual(result['status'], 'processed')
        attention_keys = [k for k in result if 'filtered' in k or 'attention_active' in k]
        self.assertGreater(len(attention_keys), 0)
    def test_dtesn_integration_factory(self):
        attention1 = create_sensor_attention_for_dtesn()
        self.assertIsInstance(attention1, SensorAttentionMechanism)
        dtesn_config = {'max_attention_channels': 5, 'attention_threshold': 0.8, 'cooperative_weight': 0.9}
        attention2 = create_sensor_attention_for_dtesn(dtesn_config)
        self.assertEqual(attention2.config.max_concurrent_foci, 5)
        self.assertEqual(attention2.config.attention_switch_threshold, 0.8)
        self.assertEqual(attention2.config.cooperative_weight, 0.9)
class TestThreadSafety(unittest.TestCase):
    def test_concurrent_processing(self):
        attention = SensorAttentionMechanism()
        results = {}
        errors = []
        def process_inputs(thread_id):
            try:
                test_input = SensorInput(modality=SensorModalityType.VISUAL, data={'thread': thread_id}, timestamp=time.time(), confidence=0.8, priority=0.6)
                result = attention.process_sensor_inputs([test_input])
                results[thread_id] = result
            except Exception as e:
                errors.append(f'Thread {thread_id}: {e}')
        threads = []
        for i in range(5):
            thread = threading.Thread(target=process_inputs, args=(i,))
            threads.append(thread)
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(len(errors), 0, f'Thread errors: {errors}')
        self.assertEqual(len(results), 5)
    def test_concurrent_state_access(self):
        attention = SensorAttentionMechanism()
        states = []
        errors = []
        def access_state(thread_id):
            try:
                test_input = SensorInput(modality=SensorModalityType.MOTION, data={'thread': thread_id}, timestamp=time.time(), confidence=0.9, priority=0.8)
                attention.process_sensor_inputs([test_input])
                state = attention.get_attention_state()
                states.append(state)
            except Exception as e:
                errors.append(f'Thread {thread_id}: {e}')
        threads = []
        for i in range(3):
            thread = threading.Thread(target=access_state, args=(i,))
            threads.append(thread)
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(len(errors), 0, f'Thread errors: {errors}')
        self.assertEqual(len(states), 3)
class TestPerformanceRequirements(unittest.TestCase):
    def test_attention_switch_timing(self):
        attention = SensorAttentionMechanism()
        high_saliency_input = SensorInput(modality=SensorModalityType.MOTION, data={'high_motion': True}, timestamp=time.time(), confidence=1.0, priority=1.0, metadata={'motion_detected': True, 'high_contrast': True})
        start_time = time.time()
        attention.process_sensor_inputs([high_saliency_input])
        end_time = time.time()
        processing_time_ms = (end_time - start_time) * 1000
        self.assertLess(processing_time_ms, 100, f'Attention processing took {processing_time_ms:.2f}ms')
    def test_memory_usage_bounds(self):
        attention = SensorAttentionMechanism()
        for i in range(1000):
            test_input = SensorInput(modality=SensorModalityType.VISUAL, data={'iteration': i}, timestamp=time.time(), confidence=0.5, priority=0.5)
            attention.process_sensor_inputs([test_input])
        state = attention.get_attention_state()
        self.assertLessEqual(len(attention.attention_history), 1000)
        self.assertLessEqual(state['current_foci'], attention.config.max_concurrent_foci)
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.WARNING)
    unittest.main(verbosity=2)