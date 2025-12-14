#!/usr/bin/env python3
"""
DTESN Resource Constraint Integration
====================================

Provides integration layer between the ResourceConstraintManager and existing
DTESN components (P-System membranes, B-Series computation, ESN reservoirs).

This integration ensures that all DTESN operations are subject to resource
constraints and real-time validation, maintaining the mathematical rigor
of the OEIS A000081 enumeration while operating under realistic resource limits.

Features:
- Automatic resource constraint wrapping for DTESN operations
- Energy consumption tracking for neuromorphic hardware
- Real-time constraint validation with graceful degradation
- Integration with existing ECAN attention allocation
- Performance monitoring and constraint violation reporting
"""

import time
import logging
from typing import Dict, Any, Optional, Callable, List
from functools import wraps
from dataclasses import dataclass

from resource_constraint_manager import (
    ResourceConstraintManager, OperationType, ResourceError
)

# Import existing DTESN components for integration
try:
    from psystem_membranes import PSystemMembranes
    from esn_reservoir import ESNReservoir  
    from bseries_tree_classifier import BSeriesTreeClassifier
    from oeis_a000081_enumerator import OEISA000081Enumerator
except ImportError:
    # Graceful handling for development environment
    PSystemMembranes = None
    ESNReservoir = None
    BSeriesTreeClassifier = None
    OEISA000081Enumerator = None

logger = logging.getLogger(__name__)

@dataclass
class ConstrainedAgent:
    """Represents an agent operating under resource constraints."""
    agent_id: str
    max_operations_per_second: int = 1000
    priority_level: int = 1  # 1-10, higher is more important
    energy_budget_joules: float = 0.1
    allocated_resources: Dict[str, float] = None
    
    def __post_init__(self):
        if self.allocated_resources is None:
            self.allocated_resources = {}

class DTESNResourceIntegrator:
    """
    Main integration class that applies resource constraints to DTESN operations.
    
    This class wraps existing DTESN components with resource constraint enforcement,
    ensuring that all membrane computing, B-Series computations, and ESN operations
    respect computational, energy, and timing constraints.
    """
    
    def __init__(self, constraint_manager: ResourceConstraintManager = None):
        """Initialize the DTESN resource integrator.
        
        Args:
            constraint_manager: Optional existing constraint manager
        """
        self._constraint_manager = constraint_manager or ResourceConstraintManager()
        self._agents: Dict[str, ConstrainedAgent] = {}
        self._operation_metrics: Dict[str, List[float]] = {}
        
        # Component instances with resource constraints applied
        self._constrained_components = {}
        
        logger.info("DTESNResourceIntegrator initialized")
    
    def register_agent(self, agent: ConstrainedAgent) -> bool:
        """Register an agent to operate under resource constraints.
        
        Args:
            agent: Agent configuration with resource limits
            
        Returns:
            True if registration successful
        """
        if agent.agent_id in self._agents:
            logger.warning(f"Agent {agent.agent_id} already registered")
            return False
        
        self._agents[agent.agent_id] = agent
        logger.info(f"Registered agent {agent.agent_id} with resource constraints")
        return True
    
    def unregister_agent(self, agent_id: str) -> bool:
        """Unregister an agent and release its resources.
        
        Args:
            agent_id: ID of agent to unregister
            
        Returns:
            True if unregistration successful
        """
        if agent_id not in self._agents:
            logger.warning(f"Agent {agent_id} not found for unregistration")
            return False
        
        # Release any allocated resources
        self._constraint_manager.release_resources(agent_id)
        
        # Remove from registry
        del self._agents[agent_id]
        logger.info(f"Unregistered agent {agent_id}")
        return True
    
    def get_constrained_psystem(self, agent_id: str) -> Optional['ConstrainedPSystemWrapper']:
        """Get a resource-constrained P-System membrane computing wrapper.
        
        Args:
            agent_id: Agent requesting P-System access
            
        Returns:
            Constrained P-System wrapper or None if agent not registered
        """
        if agent_id not in self._agents:
            logger.error(f"Agent {agent_id} not registered for P-System access")
            return None
        
        if PSystemMembranes is None:
            logger.warning("PSystemMembranes not available - using mock for development")
            return MockConstrainedPSystemWrapper(agent_id, self._constraint_manager)
        
        return ConstrainedPSystemWrapper(agent_id, self._constraint_manager)
    
    def get_constrained_esn(self, agent_id: str) -> Optional['ConstrainedESNWrapper']:
        """Get a resource-constrained ESN reservoir wrapper.
        
        Args:
            agent_id: Agent requesting ESN access
            
        Returns:
            Constrained ESN wrapper or None if agent not registered
        """
        if agent_id not in self._agents:
            logger.error(f"Agent {agent_id} not registered for ESN access")
            return None
        
        if ESNReservoir is None:
            logger.warning("ESNReservoir not available - using mock for development")
            return MockConstrainedESNWrapper(agent_id, self._constraint_manager)
        
        return ConstrainedESNWrapper(agent_id, self._constraint_manager)
    
    def get_constrained_bseries(self, agent_id: str) -> Optional['ConstrainedBSeriesWrapper']:
        """Get a resource-constrained B-Series computation wrapper.
        
        Args:
            agent_id: Agent requesting B-Series access
            
        Returns:
            Constrained B-Series wrapper or None if agent not registered
        """
        if agent_id not in self._agents:
            logger.error(f"Agent {agent_id} not registered for B-Series access")
            return None
        
        if BSeriesTreeClassifier is None:
            logger.warning("BSeriesTreeClassifier not available - using mock for development")
            return MockConstrainedBSeriesWrapper(agent_id, self._constraint_manager)
        
        return ConstrainedBSeriesWrapper(agent_id, self._constraint_manager)
    
    def execute_constrained_operation(self, agent_id: str, operation_type: OperationType,
                                    operation_func: Callable, *args, **kwargs) -> Any:
        """Execute any operation under resource constraints.
        
        Args:
            agent_id: Agent performing the operation
            operation_type: Type of DTESN operation
            operation_func: Function to execute
            *args, **kwargs: Function arguments
            
        Returns:
            Operation result
            
        Raises:
            ResourceError: If resource constraints are violated
        """
        if agent_id not in self._agents:
            raise ResourceError(f"Agent {agent_id} not registered")
        
        return self._constraint_manager.enforce_agent_constraints(
            agent_id, operation_type, operation_func, *args, **kwargs)
    
    def get_agent_resource_status(self, agent_id: str) -> Optional[Dict[str, Any]]:
        """Get current resource status for a specific agent.
        
        Args:
            agent_id: Agent to check
            
        Returns:
            Resource status dictionary or None if agent not found
        """
        if agent_id not in self._agents:
            return None
        
        agent = self._agents[agent_id]
        global_status = self._constraint_manager.get_resource_status()
        
        return {
            'agent_id': agent_id,
            'priority_level': agent.priority_level,
            'energy_budget': agent.energy_budget_joules,
            'max_operations_per_second': agent.max_operations_per_second,
            'allocated_resources': agent.allocated_resources,
            'global_constraints': global_status
        }
    
    def get_system_performance_metrics(self) -> Dict[str, Any]:
        """Get comprehensive system performance metrics."""
        constraint_metrics = self._constraint_manager.get_performance_metrics()
        
        return {
            'constraint_manager': constraint_metrics,
            'registered_agents': len(self._agents),
            'agent_list': list(self._agents.keys()),
            'operation_history': self._operation_metrics
        }
    
    @property
    def constraint_manager(self) -> ResourceConstraintManager:
        """Access to underlying constraint manager."""
        return self._constraint_manager

class ConstrainedPSystemWrapper:
    """Resource-constrained wrapper for P-System membrane computing operations."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._psystem = PSystemMembranes() if PSystemMembranes else None
        logger.debug(f"ConstrainedPSystemWrapper initialized for agent {agent_id}")
    
    def evolve_membrane(self, membrane_config: Dict[str, Any]) -> Dict[str, Any]:
        """Execute membrane evolution under resource constraints."""
        def _evolve():
            if self._psystem:
                return self._psystem.evolve_membranes(membrane_config)
            else:
                # Development mock - return expected structure
                time.sleep(0.000005)  # Simulate 5μs operation
                return {
                    'status': 'evolved',
                    'cycles': 1,
                    'membrane_count': membrane_config.get('initial_membranes', 1) + 1,
                    'oeis_compliant': True
                }
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.MEMBRANE_EVOLUTION, _evolve)
    
    def validate_oeis_compliance(self, tree_structure: Dict[str, Any]) -> bool:
        """Validate OEIS A000081 compliance under resource constraints."""
        def _validate():
            if OEISA000081Enumerator:
                enumerator = OEISA000081Enumerator()
                return enumerator.validate_tree_structure(tree_structure)
            else:
                # Development mock - assume compliance
                time.sleep(0.000001)  # Simulate 1μs operation
                return True
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.OEIS_VALIDATION, _validate)

class ConstrainedESNWrapper:
    """Resource-constrained wrapper for Echo State Network operations."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._esn = ESNReservoir() if ESNReservoir else None
        logger.debug(f"ConstrainedESNWrapper initialized for agent {agent_id}")
    
    def update_reservoir_state(self, input_data: List[float]) -> List[float]:
        """Update ESN reservoir state under resource constraints."""
        def _update():
            if self._esn:
                return self._esn.update_state(input_data)
            else:
                # Development mock - return modified input
                time.sleep(0.0005)  # Simulate 500μs operation
                return [x * 0.9 + 0.1 for x in input_data]
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.ESN_STATE_UPDATE, _update)
    
    def train_readout(self, target_outputs: List[float]) -> Dict[str, Any]:
        """Train ESN readout weights under resource constraints."""
        def _train():
            if self._esn:
                return self._esn.train_readout(target_outputs)
            else:
                # Development mock
                time.sleep(0.001)  # Simulate 1ms operation
                return {
                    'training_error': 0.05,
                    'iterations': 10,
                    'convergence': True
                }
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.ESN_STATE_UPDATE, _train)

class ConstrainedBSeriesWrapper:
    """Resource-constrained wrapper for B-Series tree computations."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._bseries = BSeriesTreeClassifier() if BSeriesTreeClassifier else None
        logger.debug(f"ConstrainedBSeriesWrapper initialized for agent {agent_id}")
    
    def classify_tree(self, tree_structure: Dict[str, Any]) -> Dict[str, Any]:
        """Classify B-Series tree structure under resource constraints."""
        def _classify():
            if self._bseries:
                return self._bseries.classify_tree(tree_structure)
            else:
                # Development mock - return classification
                time.sleep(0.00005)  # Simulate 50μs operation
                return {
                    'tree_type': 'elementary',
                    'order': tree_structure.get('depth', 1),
                    'symmetry_factor': 1,
                    'coefficients': [1.0]
                }
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.BSERIES_COMPUTATION, _classify)
    
    def compute_elementary_differential(self, tree: Dict[str, Any], 
                                      order: int) -> Dict[str, Any]:
        """Compute elementary differential under resource constraints."""
        def _compute():
            if self._bseries:
                return self._bseries.compute_differential(tree, order)
            else:
                # Development mock
                time.sleep(0.0001)  # Simulate 100μs operation
                return {
                    'differential': f"D^{order}",
                    'coefficient': 1.0 / order if order > 0 else 1.0,
                    'tree_order': order
                }
        
        return self._constraint_manager.enforce_agent_constraints(
            self.agent_id, OperationType.BSERIES_COMPUTATION, _compute)

# Mock implementations for development environment
class MockConstrainedPSystemWrapper(ConstrainedPSystemWrapper):
    """Mock P-System wrapper for development without full DTESN components."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._psystem = None  # Explicitly set to None for mock
        logger.debug(f"MockConstrainedPSystemWrapper initialized for agent {agent_id}")

class MockConstrainedESNWrapper(ConstrainedESNWrapper):
    """Mock ESN wrapper for development without full DTESN components."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._esn = None  # Explicitly set to None for mock
        logger.debug(f"MockConstrainedESNWrapper initialized for agent {agent_id}")

class MockConstrainedBSeriesWrapper(ConstrainedBSeriesWrapper):
    """Mock B-Series wrapper for development without full DTESN components."""
    
    def __init__(self, agent_id: str, constraint_manager: ResourceConstraintManager):
        self.agent_id = agent_id
        self._constraint_manager = constraint_manager
        self._bseries = None  # Explicitly set to None for mock
        logger.debug(f"MockConstrainedBSeriesWrapper initialized for agent {agent_id}")

def constrained_operation(operation_type: OperationType):
    """Decorator to automatically apply resource constraints to functions.
    
    Usage:
        @constrained_operation(OperationType.MEMBRANE_EVOLUTION)
        def my_membrane_operation(agent_id, ...):
            # operation implementation
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            # Extract agent_id from arguments
            agent_id = kwargs.get('agent_id') or (args[0] if args else None)
            if not agent_id:
                raise ValueError("agent_id must be provided for constrained operations")
            
            # Get global constraint manager (this could be improved with dependency injection)
            global _global_integrator
            if '_global_integrator' not in globals():
                _global_integrator = DTESNResourceIntegrator()
            
            return _global_integrator.execute_constrained_operation(
                agent_id, operation_type, func, *args, **kwargs)
        
        return wrapper
    return decorator

# Export main classes for integration
__all__ = [
    'DTESNResourceIntegrator',
    'ConstrainedAgent',
    'ConstrainedPSystemWrapper',
    'ConstrainedESNWrapper', 
    'ConstrainedBSeriesWrapper',
    'constrained_operation'
]