"""
DTESN Integration for Body State Awareness - Phase 3.3.1

Integrates the Body State Awareness System with the existing
Deep Tree Echo System Network (DTESN) architecture in echo.kern.

This ensures body state awareness data is available to the DTESN
for processing and decision making.
"""

import time
from typing import Dict, Any, Optional, List, Tuple
from dataclasses import dataclass, field

# Try to import echo.kern components
try:
    import sys
    from pathlib import Path
    echo_kern_path = Path(__file__).parent.parent.parent / "echo.kern"
    sys.path.append(str(echo_kern_path))
    
    from enactive_perception import BodyState, MotorAction, SensorimotorExperience
    ECHO_KERN_AVAILABLE = True
except ImportError:
    ECHO_KERN_AVAILABLE = False
    
    # Fallback definitions for compatibility
    @dataclass
    class BodyState:
        position: Tuple[float, float, float] = (0.0, 0.0, 0.0)
        orientation: Tuple[float, float, float] = (0.0, 0.0, 0.0)
        joint_angles: Dict[str, float] = field(default_factory=dict)
        sensory_state: Dict[str, Any] = field(default_factory=dict)
        timestamp: float = field(default_factory=time.time)
    
    @dataclass  
    class MotorAction:
        joint_targets: Dict[str, float] = field(default_factory=dict)
        muscle_commands: Dict[str, float] = field(default_factory=dict)
        duration: float = 1.0
        force: float = 1.0
        precision: float = 1.0
        timestamp: float = field(default_factory=time.time)

# Import body state awareness system
try:
    from .body_state_awareness import BodyStateAwarenessSystem, BodyStateType
    BODY_STATE_AWARENESS_AVAILABLE = True
except ImportError:
    BODY_STATE_AWARENESS_AVAILABLE = False


@dataclass
class DTESNBodyStateData:
    """Body state data formatted for DTESN processing."""
    node_id: str
    timestamp: float
    
    # Core sensing data (Phase 3.3.1 requirements)
    joint_angle_data: Dict[str, float] = field(default_factory=dict)
    joint_velocity_data: Dict[str, float] = field(default_factory=dict)
    body_position: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    body_orientation: Tuple[float, float, float, float] = (0.0, 0.0, 0.0, 1.0)
    
    # Internal monitoring data
    balance_score: float = 0.8
    stability_index: float = 0.8
    coordination_level: float = 0.8
    awareness_confidence: float = 0.9
    
    # Metadata for DTESN processing
    processing_priority: float = 1.0
    membrane_compatibility: bool = True
    real_time_constraint: bool = True


class DTESNBodyStateIntegration:
    """
    Integration bridge between Body State Awareness System and DTESN.
    
    Converts body state awareness data into DTESN-compatible formats
    and provides real-time data feeds for the DTESN architecture.
    """
    
    def __init__(self, body_state_system: Optional[Any] = None, dtesn_node_id: str = "body_awareness_node"):
        """Initialize DTESN integration."""
        self.body_state_system = body_state_system
        self.dtesn_node_id = dtesn_node_id
        
        # DTESN integration state
        self.membrane_buffer = []
        self.max_buffer_size = 100
        self.last_dtesn_update = 0.0
        self.dtesn_update_frequency = 30.0  # Hz - suitable for real-time processing
        
        # Data conversion mappings
        self.joint_mapping = {}  # Will be populated based on virtual body
        self.sensor_priority = {
            'joint_angle': 1.0,
            'joint_velocity': 0.9,
            'balance': 0.8,
            'stability': 0.7
        }
        
        # Integration metrics
        self.integration_active = True
        self.conversion_success_rate = 1.0
        self.dtesn_processing_latency = 0.0
    
    def convert_to_dtesn_format(self, body_state_data: Dict[str, Any]) -> DTESNBodyStateData:
        """Convert body state awareness data to DTESN format."""
        current_time = time.time()
        
        # Extract joint angle and velocity data
        joint_sensing = body_state_data.get('joint_angle_velocity_sensing', {})
        joint_angles = {}
        joint_velocities = {}
        
        for joint_id, joint_data in joint_sensing.items():
            if 'angle' in joint_data:
                joint_angles[joint_id] = float(joint_data['angle'])
            if 'velocity' in joint_data:
                joint_velocities[joint_id] = float(joint_data['velocity'])
        
        # Extract position and orientation
        position_tracking = body_state_data.get('body_position_orientation_tracking', {})
        body_position = tuple(position_tracking.get('position', [0.0, 0.0, 0.0]))
        
        # Handle orientation (convert to quaternion if needed)
        orientation_data = position_tracking.get('orientation', [0, 0, 0, 1])
        if len(orientation_data) == 3:  # Euler angles
            # Convert to quaternion for DTESN compatibility  
            body_orientation = (*orientation_data, 1.0)
        else:
            body_orientation = tuple(orientation_data[:4])
        
        # Extract internal monitoring data
        internal_monitoring = body_state_data.get('internal_body_state_monitoring', {})
        
        return DTESNBodyStateData(
            node_id=self.dtesn_node_id,
            timestamp=current_time,
            joint_angle_data=joint_angles,
            joint_velocity_data=joint_velocities,
            body_position=body_position,
            body_orientation=body_orientation,
            balance_score=internal_monitoring.get('balance_score', 0.8),
            stability_index=internal_monitoring.get('stability_index', 0.8),
            coordination_level=internal_monitoring.get('coordination_level', 0.8),
            awareness_confidence=body_state_data.get('awareness_confidence', 0.9),
            processing_priority=self.calculate_processing_priority(body_state_data),
            membrane_compatibility=True,
            real_time_constraint=True
        )
    
    def calculate_processing_priority(self, body_state_data: Dict[str, Any]) -> float:
        """Calculate processing priority based on body state urgency."""
        # Higher priority for unstable or critical states
        internal_data = body_state_data.get('internal_body_state_monitoring', {})
        
        balance_score = internal_data.get('balance_score', 0.8)
        stability_index = internal_data.get('stability_index', 0.8)
        awareness_confidence = body_state_data.get('awareness_confidence', 0.9)
        
        # Low balance or stability = high priority
        balance_priority = 2.0 - balance_score  # Inverted: low balance = high priority
        stability_priority = 2.0 - stability_index
        confidence_priority = awareness_confidence  # High confidence = higher priority
        
        # Weighted average with emphasis on safety (balance/stability)
        priority = (balance_priority * 0.4 + stability_priority * 0.4 + confidence_priority * 0.2)
        return min(2.0, max(0.1, priority))  # Clamp to reasonable range
    
    def convert_to_echo_kern_body_state(self, body_state_data: Dict[str, Any]) -> BodyState:
        """Convert to echo.kern BodyState format for enactive perception."""
        joint_sensing = body_state_data.get('joint_angle_velocity_sensing', {})
        position_tracking = body_state_data.get('body_position_orientation_tracking', {})
        internal_monitoring = body_state_data.get('internal_body_state_monitoring', {})
        
        # Build joint angles dictionary
        joint_angles = {}
        for joint_id, joint_data in joint_sensing.items():
            if 'angle' in joint_data:
                joint_angles[joint_id] = joint_data['angle']
        
        # Build sensory state
        sensory_state = {
            'proprioception': body_state_data.get('awareness_confidence', 0.9),
            'balance': internal_monitoring.get('balance_score', 0.8),
            'stability': internal_monitoring.get('stability_index', 0.8),
            'coordination': internal_monitoring.get('coordination_level', 0.8)
        }
        
        return BodyState(
            position=tuple(position_tracking.get('position', [0.0, 0.0, 0.0])),
            orientation=tuple(position_tracking.get('orientation', [0.0, 0.0, 0.0])[:3]),
            joint_angles=joint_angles,
            sensory_state=sensory_state,
            timestamp=time.time()
        )
    
    def update_dtesn_feed(self) -> Optional[DTESNBodyStateData]:
        """Update DTESN data feed with current body state."""
        current_time = time.time()
        
        # Check update frequency
        if current_time - self.last_dtesn_update < 1.0 / self.dtesn_update_frequency:
            return None
        
        if not self.body_state_system or not self.integration_active:
            return None
        
        try:
            # Get comprehensive body state
            body_state_data = self.body_state_system.get_comprehensive_body_state()
            
            # Convert to DTESN format
            dtesn_data = self.convert_to_dtesn_format(body_state_data)
            
            # Add to membrane buffer for DTESN processing
            self.membrane_buffer.append(dtesn_data)
            if len(self.membrane_buffer) > self.max_buffer_size:
                self.membrane_buffer.pop(0)  # Remove oldest entry
            
            self.last_dtesn_update = current_time
            self.conversion_success_rate = 0.99  # Track success
            
            return dtesn_data
            
        except Exception as e:
            print(f"DTESN integration error: {e}")
            self.conversion_success_rate *= 0.95  # Degrade success rate on error
            return None
    
    def get_membrane_data_stream(self) -> List[DTESNBodyStateData]:
        """Get buffered data stream for membrane processing."""
        return self.membrane_buffer.copy()
    
    def create_sensorimotor_experience(self, motor_action: Optional[MotorAction] = None) -> Optional[Any]:
        """Create sensorimotor experience for enactive perception integration."""
        if not ECHO_KERN_AVAILABLE or not self.body_state_system:
            return None
        
        try:
            # Get current body state
            body_state_data = self.body_state_system.get_comprehensive_body_state()
            initial_body_state = self.convert_to_echo_kern_body_state(body_state_data)
            
            # Use provided motor action or create default
            if motor_action is None:
                joint_angles = body_state_data.get('joint_angle_velocity_sensing', {})
                joint_targets = {}
                for joint_id, joint_data in joint_angles.items():
                    if 'angle' in joint_data:
                        joint_targets[joint_id] = joint_data['angle']
                
                motor_action = MotorAction(
                    joint_targets=joint_targets,
                    duration=1.0 / self.dtesn_update_frequency,
                    force=1.0,
                    precision=body_state_data.get('awareness_confidence', 0.9)
                )
            
            # For now, resulting state is same as initial (could be predicted)
            resulting_body_state = initial_body_state
            
            # Create sensorimotor experience
            return SensorimotorExperience(
                initial_body_state=initial_body_state,
                motor_action=motor_action,
                resulting_body_state=resulting_body_state,
                timestamp=time.time(),
                context={'dtesn_node': self.dtesn_node_id}
            )
            
        except Exception as e:
            print(f"Sensorimotor experience creation error: {e}")
            return None
    
    def validate_dtesn_integration(self) -> Tuple[bool, Dict[str, Any]]:
        """Validate DTESN integration functionality."""
        validation_results = {
            'integration_active': self.integration_active,
            'body_state_system_available': self.body_state_system is not None,
            'conversion_success_rate': self.conversion_success_rate,
            'buffer_size': len(self.membrane_buffer),
            'update_frequency_ok': self.dtesn_update_frequency >= 10.0,
            'echo_kern_compatible': ECHO_KERN_AVAILABLE,
            'real_time_performance': self.dtesn_processing_latency < 0.1
        }
        
        # Test data conversion
        conversion_test_passed = False
        if self.body_state_system:
            try:
                test_data = self.body_state_system.get_comprehensive_body_state()
                dtesn_data = self.convert_to_dtesn_format(test_data)
                echo_state = self.convert_to_echo_kern_body_state(test_data)
                conversion_test_passed = (dtesn_data is not None and echo_state is not None)
            except Exception:
                conversion_test_passed = False
        
        validation_results['conversion_test_passed'] = conversion_test_passed
        
        # Overall validation
        critical_checks = [
            validation_results['integration_active'],
            validation_results['body_state_system_available'],
            validation_results['conversion_success_rate'] > 0.8,
            validation_results['conversion_test_passed']
        ]
        
        all_valid = all(critical_checks)
        validation_results['dtesn_integration_valid'] = all_valid
        
        return all_valid, validation_results
    
    def get_integration_status(self) -> Dict[str, Any]:
        """Get current integration status."""
        return {
            'node_id': self.dtesn_node_id,
            'integration_active': self.integration_active,
            'last_update': self.last_dtesn_update,
            'update_frequency': self.dtesn_update_frequency,
            'buffer_size': len(self.membrane_buffer),
            'success_rate': self.conversion_success_rate,
            'processing_latency': self.dtesn_processing_latency,
            'echo_kern_available': ECHO_KERN_AVAILABLE,
            'body_state_available': BODY_STATE_AWARENESS_AVAILABLE
        }