#!/usr/bin/env python3
"""
Sensor Attention Integration Module
==================================

Integration layer for connecting DTESN sensor attention mechanisms 
with existing sensory-motor systems and attention allocators.

This module implements Phase 3.1.3 requirements:
- Selective attention for sensory input
- Dynamic sensor prioritization  
- Attention-guided perception
"""

import time
import logging
from typing import Dict, List, Optional
from pathlib import Path
import json
import threading

try:
    from kernel.dtesn.sensor_attention_mechanism import (
        SensorModalityType, SensorInput, SensorAttentionMechanism,
        create_sensor_attention_for_dtesn, integrate_with_sensory_motor
    )
except ImportError:
    # Handle import when running from different directory
    import sys
    import os
    sys.path.insert(0, os.path.dirname(__file__))
    from kernel.dtesn.sensor_attention_mechanism import (
        SensorModalityType, SensorInput, create_sensor_attention_for_dtesn, integrate_with_sensory_motor
    )


class AttentionGuidedSensorSystem:
    """
    Integration system that provides attention-guided perception for agents.
    
    This system bridges the DTESN sensor attention mechanism with existing
    sensory-motor systems to provide focused attention on relevant sensory information.
    """
    
    def __init__(self, 
                 dtesn_config: Optional[Dict] = None,
                 enable_logging: bool = True):
        """
        Initialize the attention-guided sensor system.
        
        Args:
            dtesn_config: Optional DTESN configuration parameters
            enable_logging: Whether to enable detailed logging
        """
        # Initialize sensor attention mechanism
        self.sensor_attention = create_sensor_attention_for_dtesn(dtesn_config)
        
        # System state
        self.active = False
        self.processing_thread = None
        self.stop_event = threading.Event()
        
        # Performance tracking
        self.total_inputs_processed = 0
        self.total_processing_time = 0.0
        self.attention_switches = 0
        
        # Logging
        self.logger = logging.getLogger(__name__)
        if enable_logging:
            self.logger.setLevel(logging.INFO)
        else:
            self.logger.setLevel(logging.WARNING)
        
        # Integration state
        self.last_sensory_motor_data = None
        self.attention_history = []
        self.context_state = "default"
        
        self.logger.info("AttentionGuidedSensorSystem initialized")
    
    def set_attention_context(self, context: str) -> None:
        """
        Set the current attention context to adjust sensor priorities.
        
        Args:
            context: Context string ('navigation', 'interaction', 'exploration', etc.)
        """
        self.context_state = context
        self.sensor_attention.update_modality_weights(context)
        self.logger.info(f"Attention context set to: {context}")
    
    def process_sensory_motor_data(self, sensory_motor_data: Dict) -> Dict:
        """
        Process data from sensory-motor system through attention mechanism.
        
        Args:
            sensory_motor_data: Data from existing sensory-motor system
            
        Returns:
            Processed data with attention filtering applied
        """
        start_time = time.time()
        
        try:
            # Store for analysis
            self.last_sensory_motor_data = sensory_motor_data
            
            # Apply attention-guided processing
            filtered_data = integrate_with_sensory_motor(
                self.sensor_attention, 
                sensory_motor_data
            )
            
            # Add attention metadata
            attention_state = self.sensor_attention.get_attention_state()
            filtered_data['attention_metadata'] = {
                'context': self.context_state,
                'active_foci': attention_state['current_foci'],
                'attention_switches': attention_state['attention_switches'],
                'processing_time_ms': (time.time() - start_time) * 1000
            }
            
            # Update performance metrics
            processing_time = time.time() - start_time
            self.total_inputs_processed += 1
            self.total_processing_time += processing_time
            
            # Track attention performance
            if attention_state['attention_switches'] > self.attention_switches:
                self.attention_switches = attention_state['attention_switches']
                
                # Log significant attention switches
                if processing_time > 0.010:  # >10ms
                    self.logger.warning(f"Attention switch took {processing_time*1000:.2f}ms")
                else:
                    self.logger.debug(f"Attention switch completed in {processing_time*1000:.2f}ms")
            
            return filtered_data
            
        except Exception as e:
            self.logger.error(f"Error processing sensory-motor data: {e}")
            # Return original data on error
            sensory_motor_data['attention_error'] = str(e)
            return sensory_motor_data
    
    def create_sensor_inputs_from_environment(self, env_data: Dict) -> List[SensorInput]:
        """
        Convert environment data to SensorInput objects.
        
        Args:
            env_data: Environment data dictionary
            
        Returns:
            List of SensorInput objects
        """
        sensor_inputs = []
        current_time = time.time()
        
        # Process visual/screen data
        if 'screen_data' in env_data or 'visual_data' in env_data:
            visual_data = env_data.get('screen_data', env_data.get('visual_data'))
            confidence = 0.8 if visual_data else 0.1
            
            sensor_inputs.append(SensorInput(
                modality=SensorModalityType.VISUAL,
                data=visual_data,
                timestamp=current_time,
                confidence=confidence,
                priority=0.7,
                metadata={
                    'source': 'screen_capture',
                    'has_motion': env_data.get('motion_detected', False),
                    'object_count': len(env_data.get('detected_objects', []))
                }
            ))
        
        # Process motion data
        if 'motion_data' in env_data or env_data.get('motion_detected', False):
            motion_data = env_data.get('motion_data', {'detected': True})
            
            sensor_inputs.append(SensorInput(
                modality=SensorModalityType.MOTION,
                data=motion_data,
                timestamp=current_time,
                confidence=0.9 if env_data.get('motion_detected', False) else 0.3,
                priority=0.8,
                metadata={
                    'motion_detected': env_data.get('motion_detected', False),
                    'velocity': env_data.get('velocity', [0.0, 0.0]),
                    'source': 'motion_detector'
                }
            ))
        
        # Process audio data
        if 'audio_data' in env_data or 'audio_level' in env_data:
            audio_data = env_data.get('audio_data', {'level': env_data.get('audio_level', 0.0)})
            
            sensor_inputs.append(SensorInput(
                modality=SensorModalityType.AUDITORY,
                data=audio_data,
                timestamp=current_time,
                confidence=0.7,
                priority=0.5,
                metadata={
                    'audio_level': env_data.get('audio_level', 0.0),
                    'sudden_change': env_data.get('audio_change', False),
                    'source': 'audio_capture'
                }
            ))
        
        # Process proprioceptive data (mouse, keyboard, etc.)
        if any(key in env_data for key in ['mouse_moved', 'key_pressed', 'cursor_pos']):
            proprioceptive_data = {
                'mouse_moved': env_data.get('mouse_moved', False),
                'key_pressed': env_data.get('key_pressed', False),
                'cursor_pos': env_data.get('cursor_pos', (0, 0))
            }
            
            sensor_inputs.append(SensorInput(
                modality=SensorModalityType.PROPRIOCEPTIVE,
                data=proprioceptive_data,
                timestamp=current_time,
                confidence=0.6,
                priority=0.4,
                metadata={
                    'interaction_type': 'user_input',
                    'source': 'input_devices'
                }
            ))
        
        # Process environmental data
        if 'environment_state' in env_data:
            env_state = env_data['environment_state']
            
            sensor_inputs.append(SensorInput(
                modality=SensorModalityType.ENVIRONMENTAL,
                data=env_state,
                timestamp=current_time,
                confidence=0.5,
                priority=0.3,
                metadata={
                    'environment_type': env_data.get('env_type', 'unknown'),
                    'source': 'environment_monitor'
                }
            ))
        
        return sensor_inputs
    
    def focus_on_salient_features(self, sensor_inputs: List[SensorInput]) -> List[SensorInput]:
        """
        Apply attention-guided perception to focus on salient features.
        
        Args:
            sensor_inputs: List of sensor inputs to process
            
        Returns:
            Filtered list of sensor inputs with attention applied
        """
        return self.sensor_attention.apply_selective_attention(sensor_inputs)
    
    def prioritize_sensors_dynamically(self, context_hints: Optional[Dict] = None) -> Dict[SensorModalityType, float]:
        """
        Dynamically prioritize sensors based on context and recent activity.
        
        Args:
            context_hints: Optional hints about current context
            
        Returns:
            Dictionary mapping sensor modalities to priority weights
        """
        # Update context if provided
        if context_hints:
            if 'task' in context_hints:
                self.set_attention_context(context_hints['task'])
            
            if 'urgency' in context_hints:
                # Adjust attention thresholds based on urgency
                urgency = context_hints['urgency']
                if urgency > 0.8:
                    # High urgency - focus on motion and visual
                    self.sensor_attention.sensor_weights[SensorModalityType.MOTION] *= 1.5
                    self.sensor_attention.sensor_weights[SensorModalityType.VISUAL] *= 1.3
                elif urgency < 0.3:
                    # Low urgency - more balanced attention
                    for modality in SensorModalityType:
                        self.sensor_attention.sensor_weights[modality] = 1.0
        
        return dict(self.sensor_attention.sensor_weights)
    
    def get_attention_summary(self) -> Dict:
        """
        Get comprehensive attention system summary.
        
        Returns:
            Dictionary containing attention system state and performance
        """
        base_state = self.sensor_attention.get_attention_state()
        
        return {
            'system_state': {
                'active': self.active,
                'context': self.context_state,
                'total_inputs_processed': self.total_inputs_processed,
                'avg_processing_time_ms': (
                    (self.total_processing_time / max(1, self.total_inputs_processed)) * 1000
                )
            },
            'attention_state': base_state,
            'performance_metrics': {
                'attention_switches_per_input': (
                    self.attention_switches / max(1, self.total_inputs_processed)
                ),
                'total_processing_time_s': self.total_processing_time,
                'meets_realtime_constraints': base_state.get('avg_switch_time_ms', 0) <= 10.0
            }
        }
    
    def save_performance_report(self, filepath: Path) -> None:
        """
        Save comprehensive performance report.
        
        Args:
            filepath: Path to save the performance report
        """
        try:
            report = self.get_attention_summary()
            report['timestamp'] = time.time()
            report['report_type'] = 'sensor_attention_performance'
            
            with open(filepath, 'w') as f:
                json.dump(report, f, indent=2)
            
            self.logger.info(f"Performance report saved to {filepath}")
            
        except Exception as e:
            self.logger.error(f"Failed to save performance report: {e}")
    
    def start_continuous_processing(self) -> None:
        """Start continuous processing thread (for real-time applications)"""
        if self.active:
            self.logger.warning("Continuous processing already active")
            return
        
        self.active = True
        self.stop_event.clear()
        self.processing_thread = threading.Thread(target=self._continuous_processing_loop)
        self.processing_thread.start()
        self.logger.info("Started continuous sensor attention processing")
    
    def stop_continuous_processing(self) -> None:
        """Stop continuous processing thread"""
        if not self.active:
            return
        
        self.active = False
        self.stop_event.set()
        
        if self.processing_thread:
            self.processing_thread.join(timeout=5.0)
            if self.processing_thread.is_alive():
                self.logger.warning("Processing thread did not stop cleanly")
        
        self.logger.info("Stopped continuous sensor attention processing")
    
    def _continuous_processing_loop(self) -> None:
        """Continuous processing loop for real-time applications"""
        self.logger.debug("Continuous processing loop started")
        
        while self.active and not self.stop_event.is_set():
            try:
                # This would integrate with real-time sensor feeds
                # For now, just apply attention decay and maintenance
                self.sensor_attention.apply_attention_decay(self.sensor_attention)
                
                # Sleep briefly to prevent busy-waiting
                time.sleep(0.01)  # 10ms cycle time
                
            except Exception as e:
                self.logger.error(f"Error in continuous processing: {e}")
                time.sleep(0.1)  # Back off on error
        
        self.logger.debug("Continuous processing loop stopped")


def create_attention_guided_system(config: Optional[Dict] = None) -> AttentionGuidedSensorSystem:
    """
    Factory function to create an attention-guided sensor system.
    
    Args:
        config: Optional configuration dictionary
        
    Returns:
        Configured AttentionGuidedSensorSystem instance
    """
    dtesn_config = None
    
    if config:
        dtesn_config = {
            'max_attention_channels': config.get('max_foci', 3),
            'attention_threshold': config.get('attention_threshold', 0.7),
            'cooperative_weight': config.get('cooperative_weight', 0.8)
        }
    
    return AttentionGuidedSensorSystem(
        dtesn_config=dtesn_config,
        enable_logging=config.get('enable_logging', True) if config else True
    )


def integrate_with_existing_sensory_motor(sensory_motor_system, attention_system: AttentionGuidedSensorSystem):
    """
    Integrate with existing sensory-motor system by wrapping its process_input method.
    
    Args:
        sensory_motor_system: Existing sensory-motor system instance
        attention_system: Attention-guided sensor system to integrate
    """
    if hasattr(sensory_motor_system, 'process_input'):
        # Store original method
        original_process_input = sensory_motor_system.process_input
        
        def attention_enhanced_process_input(*args, **kwargs):
            """Enhanced process_input with attention guidance"""
            # Call original method
            result = original_process_input(*args, **kwargs)
            
            # Apply attention processing
            enhanced_result = attention_system.process_sensory_motor_data(result)
            
            return enhanced_result
        
        # Replace method
        sensory_motor_system.process_input = attention_enhanced_process_input
        
        logging.getLogger(__name__).info("Integrated attention guidance with existing sensory-motor system")
    else:
        logging.getLogger(__name__).warning("Sensory-motor system does not have process_input method")


# Example usage and demonstration
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    
    print("DTESN Sensor Attention Integration Demo")
    print("=" * 50)
    
    # Create attention-guided sensor system
    attention_system = create_attention_guided_system({
        'max_foci': 3,
        'attention_threshold': 0.6,
        'enable_logging': True
    })
    
    # Set context
    attention_system.set_attention_context("exploration")
    
    # Simulate environmental data
    env_data = {
        'screen_data': {'frame': 'test_frame'},
        'motion_detected': True,
        'velocity': [1.5, 0.8],
        'audio_level': 0.6,
        'mouse_moved': True,
        'cursor_pos': (100, 200),
        'detected_objects': ['object1', 'object2']
    }
    
    print("\nProcessing environmental data...")
    
    # Create sensor inputs
    sensor_inputs = attention_system.create_sensor_inputs_from_environment(env_data)
    print(f"Created {len(sensor_inputs)} sensor inputs")
    
    # Apply attention-guided perception
    focused_inputs = attention_system.focus_on_salient_features(sensor_inputs)
    print(f"Attention focused on {len(focused_inputs)} inputs")
    
    # Get dynamic sensor priorities
    priorities = attention_system.prioritize_sensors_dynamically({'urgency': 0.7})
    print("\nSensor priorities:")
    for modality, priority in priorities.items():
        print(f"  {modality.value}: {priority:.2f}")
    
    # Test with sensory-motor data format
    sensory_motor_data = {
        'status': 'processed',
        'motion': {'motion_detected': True, 'velocity': [1.0, 2.0]},
        'objects': ['detected_object'],
        'mouse_moved': True
    }
    
    result = attention_system.process_sensory_motor_data(sensory_motor_data)
    print(f"\nProcessed sensory-motor data with {len(result)} fields")
    
    if 'attention_metadata' in result:
        metadata = result['attention_metadata']
        print(f"Attention metadata: {metadata['active_foci']} active foci, "
              f"{metadata['processing_time_ms']:.2f}ms processing time")
    
    # Print performance summary
    summary = attention_system.get_attention_summary()
    print("\nPerformance Summary:")
    print(f"  Inputs processed: {summary['system_state']['total_inputs_processed']}")
    print(f"  Avg processing time: {summary['system_state']['avg_processing_time_ms']:.2f}ms")
    print(f"  Meets real-time constraints: {summary['performance_metrics']['meets_realtime_constraints']}")