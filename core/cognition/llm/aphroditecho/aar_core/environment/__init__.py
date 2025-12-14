"""
AAR Environment Coupling Integration Package

This package provides integration between the Environment Coupling System
and the existing Agent-Arena-Relation (AAR) orchestration framework.
"""

from .aar_bridge import (
    AAREnvironmentBridge,
    AAREnvironmentAdapter,
    aar_environment_adapter,
    initialize_aar_environment_coupling,
    update_aar_environment_state,
    get_aar_coupling_status
)

__all__ = [
    'AAREnvironmentBridge',
    'AAREnvironmentAdapter', 
    'aar_environment_adapter',
    'initialize_aar_environment_coupling',
    'update_aar_environment_state',
    'get_aar_coupling_status'
]