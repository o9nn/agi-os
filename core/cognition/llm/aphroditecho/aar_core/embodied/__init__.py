"""
Embodied AI Components for 4E Framework

Implements virtual body representation and hardware abstractions for embodied agents
as part of the Deep Tree Echo Phase 2.1.1 and Phase 2.2.3 implementations.

Task 2.2.3: Build Embedded Hardware Abstractions
- Virtual sensor and actuator interfaces
- Hardware simulation for embodied systems
- Real-time system integration

Task 3.2.1: Design Hierarchical Motor Control (Phase 3.2)
- High-level goal planning
- Mid-level trajectory generation
- Low-level motor execution
- Smooth and coordinated movement execution

Task 3.3.1: Implement Body State Awareness (Phase 3.3)
- Joint angle and velocity sensing
- Body position and orientation tracking
- Internal body state monitoring
"""

from .virtual_body import VirtualBody, BodyJoint, BodySchema
from .embodied_agent import EmbodiedAgent
from .proprioception import ProprioceptiveSystem
from .hardware_abstraction import (
    EmbeddedHardwareSimulator,
    VirtualSensor, VirtualActuator,
    HardwareDevice, HardwareRegistry,
    SensorType, ActuatorType, HardwareType,
    SensorReading, ActuatorCommand, HardwareEvent,
    VisionSensor, AuditorySensor, TactileSensor,
    MultiModalSensorManager
)
from .hardware_integration import (
    EmbodiedHardwareManager,
    ProprioceptiveHardwareBridge,
    HardwareMapping
)
# Task 3.2.1: Hierarchical Motor Control System
from .hierarchical_motor_control import (
    HierarchicalMotorController, HighLevelGoalPlanner, MidLevelTrajectoryGenerator,
    LowLevelMotorExecutor, MotorGoal, MotorGoalType, Trajectory
)

# Task 3.3.1: Body State Awareness System
try:
    from .body_state_awareness import (
        BodyStateAwarenessSystem,
        BodyStateType,
        BodyStateReading,
        InternalBodyState
    )
    BODY_STATE_AWARENESS_AVAILABLE = True
except ImportError:
    BODY_STATE_AWARENESS_AVAILABLE = False

# DTESN Integration for Body State Awareness
try:
    from .dtesn_integration import (
        DTESNBodyStateIntegration,
        DTESNBodyStateData
    )
    DTESN_INTEGRATION_AVAILABLE = True
except ImportError:
    DTESN_INTEGRATION_AVAILABLE = False

__all__ = [
    # Virtual Body Components (Phase 2.1.1)
    'VirtualBody',
    'BodyJoint', 
    'BodySchema',
    'EmbodiedAgent',
    'ProprioceptiveSystem',
    
    # Hardware Abstraction Components (Phase 2.2.3)
    'EmbeddedHardwareSimulator',
    'VirtualSensor', 'VirtualActuator', 'HardwareDevice', 'HardwareRegistry',
    'SensorType', 'ActuatorType', 'HardwareType',
    'SensorReading', 'ActuatorCommand', 'HardwareEvent',
    'VisionSensor', 'AuditorySensor', 'TactileSensor', 'MultiModalSensorManager',
    
    # Hardware Integration Components (Phase 2.2.3)
    'EmbodiedHardwareManager', 'ProprioceptiveHardwareBridge', 'HardwareMapping',
    
    # Hierarchical Motor Control Components (Phase 3.2.1)
    'HierarchicalMotorController', 'HighLevelGoalPlanner', 'MidLevelTrajectoryGenerator',
    'LowLevelMotorExecutor', 'MotorGoal', 'MotorGoalType', 'Trajectory'
]

# Add Phase 3.3.1 components if available
if BODY_STATE_AWARENESS_AVAILABLE:
    __all__.extend([
        'BodyStateAwarenessSystem',
        'BodyStateType', 
        'BodyStateReading',
        'InternalBodyState'
    ])

if DTESN_INTEGRATION_AVAILABLE:
    __all__.extend([
        'DTESNBodyStateIntegration',
        'DTESNBodyStateData'
    ])

# Availability flags for runtime checking
__all__.extend([
    'BODY_STATE_AWARENESS_AVAILABLE',
    'DTESN_INTEGRATION_AVAILABLE'
])