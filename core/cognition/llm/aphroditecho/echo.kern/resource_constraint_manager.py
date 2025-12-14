#!/usr/bin/env python3
"""
DTESN Resource Constraint Manager
=================================

Implements computational resource limitations, energy consumption modeling,
and real-time processing constraints for the Deep Tree Echo State Networks
(DTESN) architecture as part of Phase 2.2.2 of the development roadmap.

This manager enforces realistic resource limits on agents operating within
the DTESN ecosystem, ensuring proper resource allocation and constraint
enforcement across membrane computing operations, B-Series computations,
and ESN reservoir updates.

Key Features:
- Computational resource budgeting and enforcement
- Energy consumption modeling for DTESN operations  
- Real-time constraint validation with graceful degradation
- Integration with existing ECAN resource allocators
- OEIS A000081 compliant tree operation constraints

Performance Targets (aligned with DTESN specs):
- Constraint checking: ≤ 5μs overhead
- Resource allocation: ≤ 10μs decision time
- Energy calculation: ≤ 1μs per operation
- Real-time validation: ≤ 2μs per check
"""

import time
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Union, Callable
from enum import Enum
import logging
from pathlib import Path
import json

# Import existing DTESN components for integration
try:
    from psystem_membranes import PSystemMembranes
    from esn_reservoir import ESNReservoir
    from bseries_tree_classifier import BSeriesTreeClassifier
    from oeis_a000081_enumerator import OEISA000081Enumerator
except ImportError:
    # Handle missing dependencies gracefully in development
    PSystemMembranes = None
    ESNReservoir = None
    BSeriesTreeClassifier = None
    OEISA000081Enumerator = None

logger = logging.getLogger(__name__)

class ResourceType(Enum):
    """Types of computational resources managed by the constraint system."""
    CPU_CYCLES = "cpu_cycles"
    MEMORY_BYTES = "memory_bytes"
    ENERGY_JOULES = "energy_joules" 
    BANDWIDTH_BPS = "bandwidth_bps"
    GPU_MEMORY = "gpu_memory"
    NEUROMORPHIC_UNITS = "neuromorphic_units"

class OperationType(Enum):
    """DTESN operation types with associated resource costs."""
    MEMBRANE_EVOLUTION = "membrane_evolution"
    BSERIES_COMPUTATION = "bseries_computation"
    ESN_STATE_UPDATE = "esn_state_update"
    CONTEXT_SWITCH = "context_switch"
    ATTENTION_ALLOCATION = "attention_allocation"
    TREE_CLASSIFICATION = "tree_classification"
    OEIS_VALIDATION = "oeis_validation"

@dataclass
class ResourceConstraint:
    """Defines resource limitations for system components."""
    resource_type: ResourceType
    max_allocation: float
    current_usage: float = 0.0
    reserved: float = 0.0
    min_reserved: float = 0.0
    hard_limit: bool = True  # If True, exceeding limit causes failure
    
    @property
    def available(self) -> float:
        """Calculate available resource capacity."""
        return max(0.0, self.max_allocation - self.current_usage - self.reserved)
    
    @property
    def utilization_percent(self) -> float:
        """Calculate current resource utilization percentage."""
        if self.max_allocation == 0:
            return 0.0
        return (self.current_usage / self.max_allocation) * 100.0
    
    def can_allocate(self, amount: float) -> bool:
        """Check if resource can accommodate additional allocation."""
        return self.available >= amount

@dataclass 
class EnergyModel:
    """Energy consumption model for DTESN operations."""
    base_power_watts: float  # Base system power consumption
    operation_costs: Dict[OperationType, float] = field(default_factory=dict)
    
    def __post_init__(self):
        """Initialize default energy costs based on DTESN specifications."""
        if not self.operation_costs:
            # Energy costs in microjoules based on timing constraints
            # These are realistic estimates based on neuromorphic hardware specs
            self.operation_costs = {
                OperationType.MEMBRANE_EVOLUTION: 0.05,      # 50 nJ (10μs @ 5mW)
                OperationType.BSERIES_COMPUTATION: 0.5,      # 500 nJ (100μs @ 5mW)
                OperationType.ESN_STATE_UPDATE: 5.0,         # 5 μJ (1ms @ 5mW)
                OperationType.CONTEXT_SWITCH: 0.025,         # 25 nJ (5μs @ 5mW)
                OperationType.ATTENTION_ALLOCATION: 0.1,     # 100 nJ
                OperationType.TREE_CLASSIFICATION: 0.01,     # 10 nJ
                OperationType.OEIS_VALIDATION: 0.02          # 20 nJ
            }
    
    def calculate_energy_cost(self, operation: OperationType, 
                             duration_seconds: float = None,
                             complexity: float = 1.0) -> float:
        """Calculate energy cost for an operation.
        
        Args:
            operation: Type of DTESN operation
            duration_seconds: Actual operation duration (if available)
            complexity: Complexity multiplier for the operation
            
        Returns:
            Energy cost in joules
        """
        base_cost = self.operation_costs.get(operation, 0.001)  # 1 μJ default
        
        if duration_seconds:
            # Use actual duration and base power
            actual_cost = self.base_power_watts * duration_seconds
            return actual_cost * complexity
        else:
            # Use estimated cost from operation type
            return base_cost * complexity * 1e-6  # Convert μJ to J

@dataclass
class RealTimeConstraint:
    """Real-time processing constraints for DTESN operations."""
    operation: OperationType
    max_duration_us: int  # Maximum duration in microseconds
    deadline_us: int      # Hard deadline in microseconds
    priority: int = 0     # Higher number = higher priority
    
    def is_within_deadline(self, start_time_ns: int, current_time_ns: int) -> bool:
        """Check if operation is still within deadline."""
        elapsed_us = (current_time_ns - start_time_ns) // 1000
        return elapsed_us <= self.deadline_us
    
    def time_remaining_us(self, start_time_ns: int, current_time_ns: int) -> int:
        """Calculate remaining time before deadline."""
        elapsed_us = (current_time_ns - start_time_ns) // 1000
        return max(0, self.deadline_us - elapsed_us)

class ResourceConstraintManager:
    """
    Main resource constraint management system for DTESN architecture.
    
    Enforces computational resource limitations, models energy consumption,
    and validates real-time processing constraints across all DTESN components.
    """
    
    def __init__(self, config_path: Optional[Path] = None):
        """Initialize the resource constraint manager.
        
        Args:
            config_path: Path to configuration file (optional)
        """
        self._lock = threading.RLock()
        self._constraints: Dict[str, ResourceConstraint] = {}
        self._energy_model = EnergyModel(base_power_watts=0.005)  # 5mW base
        self._rt_constraints: Dict[OperationType, RealTimeConstraint] = {}
        self._operation_history: List[Dict] = []
        self._active_operations: Dict[str, Dict] = {}
        
        # Initialize default constraints based on DTESN specifications
        self._initialize_default_constraints()
        self._initialize_realtime_constraints()
        
        # Load custom configuration if provided
        if config_path and config_path.exists():
            self._load_configuration(config_path)
        
        # Statistics tracking
        self._total_operations = 0
        self._constraint_violations = 0
        self._energy_consumed = 0.0
        
        logger.info("ResourceConstraintManager initialized with DTESN constraints")
    
    def _initialize_default_constraints(self):
        """Initialize default resource constraints based on DTESN specs."""
        # CPU constraints (assuming embedded/neuromorphic hardware)
        self._constraints["cpu_primary"] = ResourceConstraint(
            resource_type=ResourceType.CPU_CYCLES,
            max_allocation=1e9,  # 1 GHz worth of cycles
            min_reserved=1e8     # Reserve 100 MHz for critical tasks
        )
        
        # Memory constraints (typical embedded system)
        self._constraints["memory_main"] = ResourceConstraint(
            resource_type=ResourceType.MEMORY_BYTES,
            max_allocation=512 * 1024 * 1024,  # 512 MB
            min_reserved=64 * 1024 * 1024       # Reserve 64 MB
        )
        
        # Energy budget (battery-powered system)
        self._constraints["energy_budget"] = ResourceConstraint(
            resource_type=ResourceType.ENERGY_JOULES,
            max_allocation=1.0,   # 1 Joule budget per allocation period
            hard_limit=False      # Allow exceeding for critical operations
        )
        
        # Neuromorphic processing units
        self._constraints["neuromorphic_units"] = ResourceConstraint(
            resource_type=ResourceType.NEUROMORPHIC_UNITS,
            max_allocation=64,    # 64 processing units
            min_reserved=8        # Reserve units for critical operations
        )
    
    def _initialize_realtime_constraints(self):
        """Initialize real-time constraints based on DTESN timing requirements."""
        self._rt_constraints = {
            OperationType.MEMBRANE_EVOLUTION: RealTimeConstraint(
                operation=OperationType.MEMBRANE_EVOLUTION,
                max_duration_us=10,
                deadline_us=15,
                priority=10
            ),
            OperationType.BSERIES_COMPUTATION: RealTimeConstraint(
                operation=OperationType.BSERIES_COMPUTATION,
                max_duration_us=100,
                deadline_us=150,
                priority=8
            ),
            OperationType.ESN_STATE_UPDATE: RealTimeConstraint(
                operation=OperationType.ESN_STATE_UPDATE,
                max_duration_us=1000,
                deadline_us=1500,
                priority=6
            ),
            OperationType.CONTEXT_SWITCH: RealTimeConstraint(
                operation=OperationType.CONTEXT_SWITCH,
                max_duration_us=5,
                deadline_us=10,
                priority=12
            )
        }
    
    def _load_configuration(self, config_path: Path):
        """Load configuration from JSON file."""
        try:
            with open(config_path, 'r') as f:
                config = json.load(f)
            
            # Load custom constraints
            if 'constraints' in config:
                for name, constraint_data in config['constraints'].items():
                    self._constraints[name] = ResourceConstraint(**constraint_data)
            
            # Load energy model configuration
            if 'energy_model' in config:
                energy_config = config['energy_model']
                self._energy_model.base_power_watts = energy_config.get(
                    'base_power_watts', self._energy_model.base_power_watts)
            
            logger.info(f"Loaded configuration from {config_path}")
            
        except Exception as e:
            logger.warning(f"Failed to load configuration from {config_path}: {e}")
    
    def allocate_resources(self, agent_id: str, resource_requests: Dict[str, float],
                          operation: OperationType = None) -> Tuple[bool, str]:
        """
        Allocate resources for an agent operation.
        
        Args:
            agent_id: Unique identifier for the agent
            resource_requests: Dict mapping resource names to requested amounts
            operation: Type of operation requesting resources
            
        Returns:
            Tuple of (success, error_message)
        """
        with self._lock:
            # Validate all requests can be satisfied
            allocation_plan = {}
            
            for resource_name, amount in resource_requests.items():
                if resource_name not in self._constraints:
                    return False, f"Unknown resource: {resource_name}"
                
                constraint = self._constraints[resource_name]
                if not constraint.can_allocate(amount):
                    return False, f"Insufficient {resource_name}: requested {amount}, available {constraint.available}"
                
                allocation_plan[resource_name] = amount
            
            # All requests can be satisfied - perform allocation
            for resource_name, amount in allocation_plan.items():
                self._constraints[resource_name].current_usage += amount
            
            # Record allocation
            allocation_record = {
                'agent_id': agent_id,
                'timestamp': time.time_ns(),
                'operation': operation.value if operation else 'unknown',
                'resources': allocation_plan,
                'status': 'allocated'
            }
            self._operation_history.append(allocation_record)
            self._active_operations[agent_id] = allocation_record
            
            logger.debug(f"Resources allocated for agent {agent_id}: {allocation_plan}")
            return True, "Resources allocated successfully"
    
    def release_resources(self, agent_id: str) -> bool:
        """
        Release all resources allocated to an agent.
        
        Args:
            agent_id: Agent whose resources should be released
            
        Returns:
            True if resources were released successfully
        """
        with self._lock:
            if agent_id not in self._active_operations:
                logger.warning(f"No active allocation found for agent {agent_id}")
                return False
            
            allocation = self._active_operations[agent_id]
            
            # Release each allocated resource
            for resource_name, amount in allocation['resources'].items():
                if resource_name in self._constraints:
                    self._constraints[resource_name].current_usage -= amount
                    # Ensure usage doesn't go negative due to floating point errors
                    self._constraints[resource_name].current_usage = max(
                        0.0, self._constraints[resource_name].current_usage)
            
            # Update allocation record
            allocation['status'] = 'released'
            allocation['release_time'] = time.time_ns()
            
            # Remove from active operations
            del self._active_operations[agent_id]
            
            logger.debug(f"Resources released for agent {agent_id}")
            return True
    
    def validate_realtime_constraint(self, operation: OperationType, 
                                   start_time_ns: int) -> Tuple[bool, str]:
        """
        Validate that an operation meets its real-time constraints.
        
        Args:
            operation: Type of operation to validate
            start_time_ns: Operation start time in nanoseconds
            
        Returns:
            Tuple of (is_valid, message)
        """
        if operation not in self._rt_constraints:
            return True, f"No real-time constraints defined for {operation.value}"
        
        constraint = self._rt_constraints[operation]
        current_time_ns = time.time_ns()
        
        if constraint.is_within_deadline(start_time_ns, current_time_ns):
            remaining_us = constraint.time_remaining_us(start_time_ns, current_time_ns)
            return True, f"Within deadline, {remaining_us}μs remaining"
        else:
            elapsed_us = (current_time_ns - start_time_ns) // 1000
            self._constraint_violations += 1
            return False, f"Deadline exceeded: {elapsed_us}μs > {constraint.deadline_us}μs"
    
    def calculate_operation_energy(self, operation: OperationType,
                                 duration_seconds: float = None,
                                 complexity: float = 1.0) -> float:
        """
        Calculate energy consumption for a DTESN operation.
        
        Args:
            operation: Type of operation
            duration_seconds: Actual duration (optional)
            complexity: Complexity factor
            
        Returns:
            Energy cost in joules
        """
        energy_cost = self._energy_model.calculate_energy_cost(
            operation, duration_seconds, complexity)
        
        # Update total energy consumption
        with self._lock:
            self._energy_consumed += energy_cost
            
            # Check energy budget constraint
            if "energy_budget" in self._constraints:
                constraint = self._constraints["energy_budget"]
                if constraint.current_usage + energy_cost > constraint.max_allocation:
                    if constraint.hard_limit:
                        logger.warning(f"Energy budget exceeded for {operation.value}")
                    else:
                        logger.debug(f"Energy budget soft limit exceeded for {operation.value}")
                
                constraint.current_usage += energy_cost
        
        return energy_cost
    
    def get_resource_status(self) -> Dict[str, Dict]:
        """Get current status of all resource constraints."""
        with self._lock:
            status = {}
            for name, constraint in self._constraints.items():
                status[name] = {
                    'type': constraint.resource_type.value,
                    'max_allocation': constraint.max_allocation,
                    'current_usage': constraint.current_usage,
                    'available': constraint.available,
                    'utilization_percent': constraint.utilization_percent,
                    'reserved': constraint.reserved,
                    'hard_limit': constraint.hard_limit
                }
            return status
    
    def get_performance_metrics(self) -> Dict[str, Union[int, float]]:
        """Get performance metrics for the constraint system."""
        with self._lock:
            return {
                'total_operations': self._total_operations,
                'constraint_violations': self._constraint_violations,
                'violation_rate': (self._constraint_violations / max(1, self._total_operations)) * 100,
                'total_energy_consumed': self._energy_consumed,
                'active_allocations': len(self._active_operations),
                'operations_in_history': len(self._operation_history)
            }
    
    def enforce_agent_constraints(self, agent_id: str, operation: OperationType,
                                operation_func: Callable, *args, **kwargs):
        """
        Execute an agent operation while enforcing resource constraints.
        
        This is the main enforcement mechanism that wraps agent operations
        to ensure they operate within resource limits.
        
        Args:
            agent_id: Agent performing the operation
            operation: Type of operation
            operation_func: Function to execute
            *args, **kwargs: Arguments for the operation function
            
        Returns:
            Operation result or raises exception if constraints violated
        """
        start_time_ns = time.time_ns()
        
        # Pre-operation: allocate resources and validate constraints
        resource_requests = self._estimate_resource_requirements(operation)
        success, message = self.allocate_resources(agent_id, resource_requests, operation)
        
        if not success:
            raise ResourceError(f"Resource allocation failed for {agent_id}: {message}")
        
        try:
            # Execute the operation
            result = operation_func(*args, **kwargs)
            
            # Post-operation: validate timing and update energy consumption
            end_time_ns = time.time_ns()
            duration_seconds = (end_time_ns - start_time_ns) / 1e9
            
            # Validate real-time constraints
            is_valid, rt_message = self.validate_realtime_constraint(operation, start_time_ns)
            if not is_valid:
                logger.warning(f"Real-time constraint violation for {agent_id}: {rt_message}")
            
            # Calculate and record energy consumption
            energy_cost = self.calculate_operation_energy(operation, duration_seconds)
            
            # Update operation statistics
            with self._lock:
                self._total_operations += 1
                if not is_valid:
                    self._constraint_violations += 1
            
            logger.debug(f"Operation {operation.value} completed for {agent_id}: "
                        f"{duration_seconds*1e6:.1f}μs, {energy_cost*1e6:.2f}μJ")
            
            return result
            
        except Exception as e:
            logger.error(f"Operation failed for {agent_id}: {e}")
            raise
        finally:
            # Always release resources
            self.release_resources(agent_id)
    
    def _estimate_resource_requirements(self, operation: OperationType) -> Dict[str, float]:
        """Estimate resource requirements for different operation types."""
        # Base resource estimates for different DTESN operations
        estimates = {
            OperationType.MEMBRANE_EVOLUTION: {
                "cpu_primary": 1e6,     # 1M cycles
                "memory_main": 1024,    # 1KB
            },
            OperationType.BSERIES_COMPUTATION: {
                "cpu_primary": 5e6,     # 5M cycles  
                "memory_main": 4096,    # 4KB
                "neuromorphic_units": 1 # 1 unit
            },
            OperationType.ESN_STATE_UPDATE: {
                "cpu_primary": 10e6,    # 10M cycles
                "memory_main": 8192,    # 8KB
                "neuromorphic_units": 2 # 2 units
            },
            OperationType.CONTEXT_SWITCH: {
                "cpu_primary": 1e5,     # 100K cycles
                "memory_main": 512,     # 512B
            }
        }
        
        return estimates.get(operation, {"cpu_primary": 1e5, "memory_main": 512})

class ResourceError(Exception):
    """Exception raised when resource constraints are violated."""
    pass

# Export main classes for integration
__all__ = [
    'ResourceConstraintManager',
    'ResourceConstraint', 
    'EnergyModel',
    'RealTimeConstraint',
    'ResourceType',
    'OperationType',
    'ResourceError'
]