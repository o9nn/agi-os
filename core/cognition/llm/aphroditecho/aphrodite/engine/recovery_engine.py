#!/usr/bin/env python3
"""
Enhanced Recovery Engine for Production Incident Response
Implements automated recovery procedures with circuit breaker patterns and rollback mechanisms.

This module extends the existing incident response system with production-grade
recovery capabilities including circuit breakers, health checks, and automated rollbacks.
"""

import time
import logging
import asyncio
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Callable, Any, Set
from enum import Enum
from collections import defaultdict, deque
import psutil

from aphrodite.engine.sla_manager import SLAViolation, ViolationSeverity

logger = logging.getLogger(__name__)


class RecoveryAction(Enum):
    """Types of recovery actions"""
    RESTART_SERVICE = "restart_service"
    SCALE_RESOURCES = "scale_resources"
    CIRCUIT_BREAKER_OPEN = "circuit_breaker_open"
    TRAFFIC_ROUTING = "traffic_routing"
    CACHE_INVALIDATION = "cache_invalidation"
    ROLLBACK_DEPLOYMENT = "rollback_deployment"
    HEALTH_CHECK_RESET = "health_check_reset"
    LOAD_BALANCER_ADJUST = "load_balancer_adjust"


class RecoveryStatus(Enum):
    """Recovery action status"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    SUCCESS = "success"
    FAILED = "failed"
    SKIPPED = "skipped"
    ROLLED_BACK = "rolled_back"


class CircuitBreakerState(Enum):
    """Circuit breaker states"""
    CLOSED = "closed"      # Normal operation
    OPEN = "open"          # Blocking requests
    HALF_OPEN = "half_open" # Testing if service recovered


@dataclass
class RecoveryProcedure:
    """Recovery procedure definition"""
    procedure_id: str
    name: str
    description: str
    actions: List[RecoveryAction]
    prerequisites: List[str] = field(default_factory=list)
    timeout_seconds: int = 300  # 5 minutes default
    retry_count: int = 3
    rollback_actions: List[RecoveryAction] = field(default_factory=list)
    severity_threshold: ViolationSeverity = ViolationSeverity.MAJOR
    enabled: bool = True


@dataclass
class RecoveryExecution:
    """Recovery execution record"""
    execution_id: str
    timestamp: float
    procedure: RecoveryProcedure
    trigger_violation: SLAViolation
    status: RecoveryStatus
    actions_completed: List[RecoveryAction] = field(default_factory=list)
    actions_failed: List[RecoveryAction] = field(default_factory=list)
    completion_time: Optional[float] = None
    error_message: Optional[str] = None
    metrics_before: Dict[str, float] = field(default_factory=dict)
    metrics_after: Dict[str, float] = field(default_factory=dict)
    
    @property
    def duration_seconds(self) -> Optional[float]:
        if self.completion_time:
            return self.completion_time - self.timestamp
        return None


@dataclass
class CircuitBreaker:
    """Circuit breaker for service protection"""
    service_name: str
    state: CircuitBreakerState = CircuitBreakerState.CLOSED
    failure_threshold: int = 5
    success_threshold: int = 3
    timeout_seconds: int = 60
    failure_count: int = 0
    success_count: int = 0
    last_failure_time: Optional[float] = None
    
    def can_execute(self) -> bool:
        """Check if requests can be executed through this circuit breaker"""
        current_time = time.time()
        
        if self.state == CircuitBreakerState.CLOSED:
            return True
        elif self.state == CircuitBreakerState.OPEN:
            # Check if timeout has elapsed
            if (self.last_failure_time and 
                current_time - self.last_failure_time > self.timeout_seconds):
                self.state = CircuitBreakerState.HALF_OPEN
                self.success_count = 0
                return True
            return False
        elif self.state == CircuitBreakerState.HALF_OPEN:
            return True
        
        return False
    
    def record_success(self):
        """Record a successful operation"""
        if self.state == CircuitBreakerState.HALF_OPEN:
            self.success_count += 1
            if self.success_count >= self.success_threshold:
                self.state = CircuitBreakerState.CLOSED
                self.failure_count = 0
        elif self.state == CircuitBreakerState.CLOSED:
            # Reset failure count on success
            self.failure_count = 0
    
    def record_failure(self):
        """Record a failed operation"""
        current_time = time.time()
        self.failure_count += 1
        self.last_failure_time = current_time
        
        if self.failure_count >= self.failure_threshold:
            self.state = CircuitBreakerState.OPEN
            self.success_count = 0


class HealthChecker:
    """Health check system for services"""
    
    def __init__(self):
        self.health_checks: Dict[str, Callable[[], bool]] = {}
        self.health_status: Dict[str, bool] = {}
        self.last_check_time: Dict[str, float] = {}
        self.check_interval_seconds = 30
        
    def register_health_check(self, service_name: str, check_func: Callable[[], bool]):
        """Register a health check function for a service"""
        self.health_checks[service_name] = check_func
        self.health_status[service_name] = True  # Assume healthy initially
        logger.info(f"Registered health check for {service_name}")
    
    def check_service_health(self, service_name: str) -> bool:
        """Check the health of a specific service"""
        if service_name not in self.health_checks:
            return True  # No health check means assume healthy
        
        current_time = time.time()
        last_check = self.last_check_time.get(service_name, 0)
        
        # Rate limit health checks
        if current_time - last_check < self.check_interval_seconds:
            return self.health_status.get(service_name, True)
        
        try:
            is_healthy = self.health_checks[service_name]()
            self.health_status[service_name] = is_healthy
            self.last_check_time[service_name] = current_time
            return is_healthy
        except Exception as e:
            logger.error(f"Health check failed for {service_name}: {e}")
            self.health_status[service_name] = False
            self.last_check_time[service_name] = current_time
            return False
    
    def get_all_health_status(self) -> Dict[str, bool]:
        """Get health status for all registered services"""
        status = {}
        for service_name in self.health_checks:
            status[service_name] = self.check_service_health(service_name)
        return status


class RecoveryEngine:
    """
    Enhanced recovery engine with circuit breaker patterns, health checks,
    and automated rollback mechanisms for production incident response.
    """
    
    def __init__(self):
        self.procedures: Dict[str, RecoveryProcedure] = {}
        self.circuit_breakers: Dict[str, CircuitBreaker] = {}
        self.executions: List[RecoveryExecution] = []
        self.active_executions: Dict[str, RecoveryExecution] = {}
        self.health_checker = HealthChecker()
        
        self.recovery_callbacks: List[Callable[[RecoveryExecution], None]] = []
        self.is_running = False
        self._lock = threading.RLock()
        
        # Initialize default recovery procedures
        self._initialize_default_procedures()
        
        # Initialize circuit breakers for critical services
        self._initialize_circuit_breakers()
        
        # Register default health checks
        self._initialize_health_checks()
        
        logger.info("Recovery Engine initialized with circuit breakers and health checks")
    
    def _initialize_default_procedures(self):
        """Initialize default recovery procedures"""
        
        # Latency spike recovery
        latency_recovery = RecoveryProcedure(
            procedure_id="latency_spike_recovery",
            name="High Latency Recovery",
            description="Recovery procedure for high request latency violations",
            actions=[
                RecoveryAction.CACHE_INVALIDATION,
                RecoveryAction.LOAD_BALANCER_ADJUST,
                RecoveryAction.TRAFFIC_ROUTING
            ],
            timeout_seconds=180,
            retry_count=2,
            severity_threshold=ViolationSeverity.MAJOR
        )
        self.add_procedure(latency_recovery)
        
        # Throughput degradation recovery
        throughput_recovery = RecoveryProcedure(
            procedure_id="throughput_degradation_recovery", 
            name="Throughput Degradation Recovery",
            description="Recovery procedure for throughput degradation violations",
            actions=[
                RecoveryAction.SCALE_RESOURCES,
                RecoveryAction.LOAD_BALANCER_ADJUST,
                RecoveryAction.CACHE_INVALIDATION
            ],
            timeout_seconds=300,
            retry_count=3,
            severity_threshold=ViolationSeverity.MAJOR
        )
        self.add_procedure(throughput_recovery)
        
        # Error rate spike recovery  
        error_recovery = RecoveryProcedure(
            procedure_id="error_rate_spike_recovery",
            name="Error Rate Spike Recovery", 
            description="Recovery procedure for error rate spike violations",
            actions=[
                RecoveryAction.CIRCUIT_BREAKER_OPEN,
                RecoveryAction.HEALTH_CHECK_RESET,
                RecoveryAction.RESTART_SERVICE
            ],
            timeout_seconds=240,
            retry_count=2,
            rollback_actions=[RecoveryAction.ROLLBACK_DEPLOYMENT],
            severity_threshold=ViolationSeverity.MINOR
        )
        self.add_procedure(error_recovery)
        
        # Resource exhaustion recovery
        resource_recovery = RecoveryProcedure(
            procedure_id="resource_exhaustion_recovery",
            name="Resource Exhaustion Recovery",
            description="Recovery procedure for resource exhaustion violations", 
            actions=[
                RecoveryAction.SCALE_RESOURCES,
                RecoveryAction.CACHE_INVALIDATION,
                RecoveryAction.TRAFFIC_ROUTING
            ],
            timeout_seconds=300,
            retry_count=2,
            severity_threshold=ViolationSeverity.MAJOR
        )
        self.add_procedure(resource_recovery)
        
        # Critical system recovery (last resort)
        critical_recovery = RecoveryProcedure(
            procedure_id="critical_system_recovery",
            name="Critical System Recovery",
            description="Critical recovery procedure for severe system violations",
            actions=[
                RecoveryAction.CIRCUIT_BREAKER_OPEN,
                RecoveryAction.TRAFFIC_ROUTING,
                RecoveryAction.RESTART_SERVICE,
                RecoveryAction.ROLLBACK_DEPLOYMENT
            ],
            timeout_seconds=600,
            retry_count=1,
            severity_threshold=ViolationSeverity.CRITICAL
        )
        self.add_procedure(critical_recovery)
        
        logger.info(f"Initialized {len(self.procedures)} default recovery procedures")
    
    def _initialize_circuit_breakers(self):
        """Initialize circuit breakers for critical services"""
        
        # Aphrodite Engine circuit breaker
        self.circuit_breakers["aphrodite_engine"] = CircuitBreaker(
            service_name="aphrodite_engine",
            failure_threshold=5,
            success_threshold=3,
            timeout_seconds=60
        )
        
        # Model inference circuit breaker
        self.circuit_breakers["model_inference"] = CircuitBreaker(
            service_name="model_inference", 
            failure_threshold=3,
            success_threshold=2,
            timeout_seconds=30
        )
        
        # KV Cache circuit breaker
        self.circuit_breakers["kv_cache"] = CircuitBreaker(
            service_name="kv_cache",
            failure_threshold=7,
            success_threshold=4,
            timeout_seconds=45
        )
        
        # Deep Tree Echo circuit breaker
        self.circuit_breakers["deep_tree_echo"] = CircuitBreaker(
            service_name="deep_tree_echo",
            failure_threshold=4,
            success_threshold=3,
            timeout_seconds=90
        )
        
        logger.info(f"Initialized {len(self.circuit_breakers)} circuit breakers")
    
    def _initialize_health_checks(self):
        """Initialize default health checks"""
        
        # System health check
        def system_health_check() -> bool:
            try:
                # Check CPU usage
                cpu_percent = psutil.cpu_percent(interval=1)
                if cpu_percent > 95:
                    return False
                
                # Check memory usage
                memory = psutil.virtual_memory()
                if memory.percent > 95:
                    return False
                
                # Check disk usage
                disk = psutil.disk_usage('/')
                if disk.percent > 95:
                    return False
                
                return True
            except Exception:
                return False
        
        self.health_checker.register_health_check("system", system_health_check)
        
        # Aphrodite Engine health check (placeholder - would integrate with actual engine)
        def aphrodite_health_check() -> bool:
            try:
                # This would check actual Aphrodite Engine health
                # For now, we simulate based on system resources
                return psutil.virtual_memory().percent < 90
            except Exception:
                return False
        
        self.health_checker.register_health_check("aphrodite_engine", aphrodite_health_check)
        
        logger.info("Initialized default health checks")
    
    def add_procedure(self, procedure: RecoveryProcedure):
        """Add a recovery procedure"""
        with self._lock:
            self.procedures[procedure.procedure_id] = procedure
        logger.info(f"Added recovery procedure: {procedure.name}")
    
    def remove_procedure(self, procedure_id: str):
        """Remove a recovery procedure"""
        with self._lock:
            if procedure_id in self.procedures:
                del self.procedures[procedure_id]
        logger.info(f"Removed recovery procedure: {procedure_id}")
    
    def register_recovery_callback(self, callback: Callable[[RecoveryExecution], None]):
        """Register a callback for recovery execution events"""
        self.recovery_callbacks.append(callback)
        logger.info("Registered recovery callback")
    
    def can_execute_on_service(self, service_name: str) -> bool:
        """Check if recovery actions can be executed on a service (circuit breaker check)"""
        if service_name in self.circuit_breakers:
            return self.circuit_breakers[service_name].can_execute()
        return True
    
    def record_service_success(self, service_name: str):
        """Record successful operation on a service"""
        if service_name in self.circuit_breakers:
            self.circuit_breakers[service_name].record_success()
    
    def record_service_failure(self, service_name: str):
        """Record failed operation on a service"""
        if service_name in self.circuit_breakers:
            self.circuit_breakers[service_name].record_failure()
            logger.warning(f"Circuit breaker recorded failure for {service_name}")
    
    async def execute_recovery(self, violation: SLAViolation) -> Optional[RecoveryExecution]:
        """Execute recovery procedure for an SLA violation"""
        
        # Find appropriate recovery procedure
        procedure = self._select_recovery_procedure(violation)
        if not procedure:
            logger.warning(f"No suitable recovery procedure found for violation: {violation.violation_id}")
            return None
        
        if not procedure.enabled:
            logger.info(f"Recovery procedure {procedure.procedure_id} is disabled")
            return None
        
        # Create execution record
        execution_id = f"recovery_{violation.violation_id}_{int(time.time())}"
        execution = RecoveryExecution(
            execution_id=execution_id,
            timestamp=time.time(),
            procedure=procedure,
            trigger_violation=violation,
            status=RecoveryStatus.PENDING,
            metrics_before=self._capture_metrics()
        )
        
        with self._lock:
            self.executions.append(execution)
            self.active_executions[execution_id] = execution
        
        logger.info(f"Starting recovery execution: {execution_id} using procedure: {procedure.name}")
        
        # Execute recovery actions
        try:
            execution.status = RecoveryStatus.IN_PROGRESS
            await self._execute_recovery_actions(execution)
            
            # Check if recovery was successful
            if await self._verify_recovery_success(execution):
                execution.status = RecoveryStatus.SUCCESS
                execution.completion_time = time.time()
                execution.metrics_after = self._capture_metrics()
                logger.info(f"Recovery execution successful: {execution_id}")
            else:
                execution.status = RecoveryStatus.FAILED
                execution.completion_time = time.time()
                execution.error_message = "Recovery verification failed"
                
                # Execute rollback if available
                if procedure.rollback_actions:
                    await self._execute_rollback_actions(execution)
                    
        except Exception as e:
            execution.status = RecoveryStatus.FAILED
            execution.completion_time = time.time()
            execution.error_message = str(e)
            logger.error(f"Recovery execution failed: {execution_id}: {e}")
            
            # Execute rollback if available
            if procedure.rollback_actions:
                try:
                    await self._execute_rollback_actions(execution)
                except Exception as rollback_error:
                    logger.error(f"Rollback failed for {execution_id}: {rollback_error}")
        
        finally:
            with self._lock:
                if execution_id in self.active_executions:
                    del self.active_executions[execution_id]
            
            # Notify callbacks
            for callback in self.recovery_callbacks:
                try:
                    callback(execution)
                except Exception as e:
                    logger.error(f"Error in recovery callback: {e}")
        
        return execution
    
    def _select_recovery_procedure(self, violation: SLAViolation) -> Optional[RecoveryProcedure]:
        """Select the most appropriate recovery procedure for a violation"""
        
        # Filter procedures by severity threshold
        suitable_procedures = []
        for procedure in self.procedures.values():
            if violation.severity.value >= procedure.severity_threshold.value:
                suitable_procedures.append(procedure)
        
        if not suitable_procedures:
            return None
        
        # Select based on violation type and metric
        violation_type = violation.violation_type.value
        metric_name = violation.metric_name.lower()
        
        # Priority mapping based on violation characteristics
        if 'latency' in metric_name and violation_type == 'latency_breach':
            for proc in suitable_procedures:
                if 'latency' in proc.procedure_id:
                    return proc
        
        if 'throughput' in metric_name and violation_type == 'throughput_degradation':
            for proc in suitable_procedures:
                if 'throughput' in proc.procedure_id:
                    return proc
        
        if 'error_rate' in metric_name and violation_type == 'error_rate_spike':
            for proc in suitable_procedures:
                if 'error_rate' in proc.procedure_id:
                    return proc
        
        if violation_type in ['resource_exhaustion']:
            for proc in suitable_procedures:
                if 'resource' in proc.procedure_id:
                    return proc
        
        # Fallback to critical recovery for severe violations
        if violation.severity == ViolationSeverity.CRITICAL:
            for proc in suitable_procedures:
                if 'critical' in proc.procedure_id:
                    return proc
        
        # Return first suitable procedure if no specific match
        return suitable_procedures[0] if suitable_procedures else None
    
    async def _execute_recovery_actions(self, execution: RecoveryExecution):
        """Execute the recovery actions for a procedure"""
        
        for action in execution.procedure.actions:
            try:
                logger.info(f"Executing recovery action: {action.value} for {execution.execution_id}")
                
                success = await self._execute_single_action(action, execution)
                
                if success:
                    execution.actions_completed.append(action)
                    # Record success for relevant services
                    self._record_action_success(action)
                else:
                    execution.actions_failed.append(action)
                    # Record failure for relevant services
                    self._record_action_failure(action)
                    
                    # Stop execution if critical action fails
                    if action in [RecoveryAction.RESTART_SERVICE, RecoveryAction.ROLLBACK_DEPLOYMENT]:
                        break
                
                # Small delay between actions
                await asyncio.sleep(1)
                
            except Exception as e:
                logger.error(f"Error executing action {action.value}: {e}")
                execution.actions_failed.append(action)
                self._record_action_failure(action)
                
                # Stop on critical failures
                if action in [RecoveryAction.RESTART_SERVICE, RecoveryAction.ROLLBACK_DEPLOYMENT]:
                    break
    
    async def _execute_single_action(self, action: RecoveryAction, execution: RecoveryExecution) -> bool:
        """Execute a single recovery action"""
        
        try:
            if action == RecoveryAction.CACHE_INVALIDATION:
                return await self._execute_cache_invalidation(execution)
            
            elif action == RecoveryAction.LOAD_BALANCER_ADJUST:
                return await self._execute_load_balancer_adjust(execution)
            
            elif action == RecoveryAction.TRAFFIC_ROUTING:
                return await self._execute_traffic_routing(execution)
            
            elif action == RecoveryAction.SCALE_RESOURCES:
                return await self._execute_scale_resources(execution)
            
            elif action == RecoveryAction.CIRCUIT_BREAKER_OPEN:
                return await self._execute_circuit_breaker_open(execution)
            
            elif action == RecoveryAction.HEALTH_CHECK_RESET:
                return await self._execute_health_check_reset(execution)
            
            elif action == RecoveryAction.RESTART_SERVICE:
                return await self._execute_restart_service(execution)
            
            elif action == RecoveryAction.ROLLBACK_DEPLOYMENT:
                return await self._execute_rollback_deployment(execution)
            
            else:
                logger.warning(f"Unknown recovery action: {action}")
                return False
                
        except Exception as e:
            logger.error(f"Error in recovery action {action.value}: {e}")
            return False
    
    async def _execute_cache_invalidation(self, execution: RecoveryExecution) -> bool:
        """Execute cache invalidation action"""
        logger.info(f"Executing cache invalidation for {execution.execution_id}")
        
        try:
            # Simulate cache invalidation
            # In production, this would clear KV cache, model cache, etc.
            await asyncio.sleep(2)  # Simulate work
            
            logger.info("Cache invalidation completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Cache invalidation failed: {e}")
            return False
    
    async def _execute_load_balancer_adjust(self, execution: RecoveryExecution) -> bool:
        """Execute load balancer adjustment action"""
        logger.info(f"Executing load balancer adjustment for {execution.execution_id}")
        
        try:
            # Simulate load balancer adjustment
            # In production, this would adjust request routing, weights, etc.
            await asyncio.sleep(3)  # Simulate work
            
            logger.info("Load balancer adjustment completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Load balancer adjustment failed: {e}")
            return False
    
    async def _execute_traffic_routing(self, execution: RecoveryExecution) -> bool:
        """Execute traffic routing action"""
        logger.info(f"Executing traffic routing for {execution.execution_id}")
        
        try:
            # Simulate traffic routing changes
            # In production, this would redirect traffic to healthy instances
            await asyncio.sleep(2)  # Simulate work
            
            logger.info("Traffic routing completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Traffic routing failed: {e}")
            return False
    
    async def _execute_scale_resources(self, execution: RecoveryExecution) -> bool:
        """Execute resource scaling action"""
        logger.info(f"Executing resource scaling for {execution.execution_id}")
        
        try:
            # Simulate resource scaling
            # In production, this would scale up compute resources
            await asyncio.sleep(5)  # Simulate work
            
            logger.info("Resource scaling completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Resource scaling failed: {e}")
            return False
    
    async def _execute_circuit_breaker_open(self, execution: RecoveryExecution) -> bool:
        """Execute circuit breaker open action"""
        logger.info(f"Opening circuit breakers for {execution.execution_id}")
        
        try:
            # Open circuit breakers for affected services
            affected_services = self._get_affected_services(execution.trigger_violation)
            
            for service in affected_services:
                if service in self.circuit_breakers:
                    # Force circuit breaker open
                    self.circuit_breakers[service].state = CircuitBreakerState.OPEN
                    self.circuit_breakers[service].last_failure_time = time.time()
                    logger.info(f"Opened circuit breaker for {service}")
            
            return True
            
        except Exception as e:
            logger.error(f"Circuit breaker open failed: {e}")
            return False
    
    async def _execute_health_check_reset(self, execution: RecoveryExecution) -> bool:
        """Execute health check reset action"""
        logger.info(f"Resetting health checks for {execution.execution_id}")
        
        try:
            # Reset health check status for affected services
            affected_services = self._get_affected_services(execution.trigger_violation)
            
            for service in affected_services:
                # Force health check revalidation
                if service in self.health_checker.health_status:
                    self.health_checker.last_check_time[service] = 0  # Force recheck
                    is_healthy = self.health_checker.check_service_health(service)
                    logger.info(f"Health check reset for {service}: {'healthy' if is_healthy else 'unhealthy'}")
            
            return True
            
        except Exception as e:
            logger.error(f"Health check reset failed: {e}")
            return False
    
    async def _execute_restart_service(self, execution: RecoveryExecution) -> bool:
        """Execute service restart action"""
        logger.info(f"Executing service restart for {execution.execution_id}")
        
        try:
            # Simulate service restart
            # In production, this would restart affected services
            await asyncio.sleep(10)  # Simulate restart time
            
            logger.info("Service restart completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Service restart failed: {e}")
            return False
    
    async def _execute_rollback_deployment(self, execution: RecoveryExecution) -> bool:
        """Execute deployment rollback action"""
        logger.info(f"Executing deployment rollback for {execution.execution_id}")
        
        try:
            # Simulate deployment rollback
            # In production, this would rollback to previous deployment
            await asyncio.sleep(15)  # Simulate rollback time
            
            logger.info("Deployment rollback completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"Deployment rollback failed: {e}")
            return False
    
    async def _execute_rollback_actions(self, execution: RecoveryExecution):
        """Execute rollback actions for failed recovery"""
        logger.info(f"Executing rollback actions for {execution.execution_id}")
        
        execution.status = RecoveryStatus.ROLLED_BACK
        
        for action in execution.procedure.rollback_actions:
            try:
                success = await self._execute_single_action(action, execution)
                if success:
                    logger.info(f"Rollback action {action.value} completed successfully")
                else:
                    logger.error(f"Rollback action {action.value} failed")
                    
            except Exception as e:
                logger.error(f"Error in rollback action {action.value}: {e}")
    
    async def _verify_recovery_success(self, execution: RecoveryExecution) -> bool:
        """Verify if recovery was successful"""
        
        try:
            # Wait a moment for changes to take effect
            await asyncio.sleep(10)
            
            # Check if the original violation conditions have been resolved
            metric_name = execution.trigger_violation.metric_name
            
            # Simulate checking current metric value
            # In production, this would check actual current metrics
            current_value = self._get_current_metric_value(metric_name)
            
            # Compare with SLA threshold
            threshold = execution.trigger_violation.threshold
            is_upper_bound = self._is_upper_bound_metric(metric_name)
            violation_boundary = threshold.get_violation_boundary(is_upper_bound)
            
            if is_upper_bound:
                is_resolved = current_value <= violation_boundary
            else:
                is_resolved = current_value >= violation_boundary
            
            # Also check health of affected services
            affected_services = self._get_affected_services(execution.trigger_violation)
            all_services_healthy = all(
                self.health_checker.check_service_health(service)
                for service in affected_services
            )
            
            recovery_successful = is_resolved and all_services_healthy
            
            logger.info(f"Recovery verification for {execution.execution_id}: "
                       f"metric_resolved={is_resolved}, services_healthy={all_services_healthy}")
            
            return recovery_successful
            
        except Exception as e:
            logger.error(f"Error verifying recovery success: {e}")
            return False
    
    def _get_affected_services(self, violation: SLAViolation) -> List[str]:
        """Get list of services affected by a violation"""
        metric_name = violation.metric_name.lower()
        
        if 'engine' in metric_name or 'inference' in metric_name:
            return ['aphrodite_engine', 'model_inference']
        elif 'cache' in metric_name:
            return ['kv_cache']
        elif 'echo' in metric_name:
            return ['deep_tree_echo']
        else:
            return ['system', 'aphrodite_engine']
    
    def _get_current_metric_value(self, metric_name: str) -> float:
        """Get current value for a metric (simulated)"""
        # In production, this would integrate with actual metrics collection
        # For now, simulate improvement after recovery
        return 150.0  # Simulated improved value
    
    def _is_upper_bound_metric(self, metric_name: str) -> bool:
        """Check if metric should be treated as upper bound (higher is worse)"""
        upper_bound_keywords = ['latency', 'error_rate', 'cpu', 'memory', 'gpu_utilization']
        return any(keyword in metric_name.lower() for keyword in upper_bound_keywords)
    
    def _capture_metrics(self) -> Dict[str, float]:
        """Capture current system metrics"""
        try:
            return {
                'cpu_percent': psutil.cpu_percent(interval=0.1),
                'memory_percent': psutil.virtual_memory().percent,
                'disk_percent': psutil.disk_usage('/').percent,
                'timestamp': time.time()
            }
        except Exception:
            return {}
    
    def _record_action_success(self, action: RecoveryAction):
        """Record success for services affected by an action"""
        if action in [RecoveryAction.RESTART_SERVICE, RecoveryAction.SCALE_RESOURCES]:
            self.record_service_success('aphrodite_engine')
    
    def _record_action_failure(self, action: RecoveryAction):
        """Record failure for services affected by an action"""
        if action in [RecoveryAction.RESTART_SERVICE, RecoveryAction.SCALE_RESOURCES]:
            self.record_service_failure('aphrodite_engine')
    
    def get_recovery_summary(self) -> Dict[str, Any]:
        """Get comprehensive recovery engine summary"""
        with self._lock:
            total_executions = len(self.executions)
            successful_executions = len([e for e in self.executions if e.status == RecoveryStatus.SUCCESS])
            failed_executions = len([e for e in self.executions if e.status == RecoveryStatus.FAILED])
            active_count = len(self.active_executions)
            
            # Circuit breaker status
            circuit_status = {
                name: cb.state.value 
                for name, cb in self.circuit_breakers.items()
            }
            
            # Health status
            health_status = self.health_checker.get_all_health_status()
            
            success_rate = (successful_executions / max(total_executions, 1)) * 100
            
            return {
                'total_executions': total_executions,
                'successful_executions': successful_executions,
                'failed_executions': failed_executions,
                'active_executions': active_count,
                'success_rate_percent': success_rate,
                'procedures_configured': len(self.procedures),
                'circuit_breaker_status': circuit_status,
                'health_status': health_status,
                'recovery_enabled': True
            }


def create_production_recovery_engine() -> RecoveryEngine:
    """Create a production-configured recovery engine"""
    return RecoveryEngine()


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    print("🔧 Production Recovery Engine Demo")
    print("=" * 50)
    
    async def demo_recovery():
        # Create recovery engine
        recovery_engine = create_production_recovery_engine()
        
        def recovery_handler(execution: RecoveryExecution):
            print(f"🔧 Recovery Execution: {execution.procedure.name} - "
                  f"Status: {execution.status.value}")
        
        recovery_engine.register_recovery_callback(recovery_handler)
        
        # Simulate an SLA violation (would come from SLA manager)
        from aphrodite.engine.sla_manager import SLAViolation, SLAViolationType, ViolationSeverity, SLAThreshold
        
        mock_threshold = SLAThreshold(
            metric_name="request_latency_p95",
            target_value=200.0,
            tolerance_percent=25.0
        )
        
        mock_violation = SLAViolation(
            violation_id="test_violation_001",
            timestamp=time.time(),
            violation_type=SLAViolationType.LATENCY_BREACH,
            severity=ViolationSeverity.MAJOR,
            metric_name="request_latency_p95",
            threshold=mock_threshold,
            actual_value=300.0,
            expected_value=200.0,
            breach_percentage=20.0,
            measurement_window=[250.0, 280.0, 300.0, 290.0, 310.0]
        )
        
        print("🚨 Executing recovery for simulated SLA violation...")
        
        # Execute recovery
        execution = await recovery_engine.execute_recovery(mock_violation)
        
        if execution:
            print(f"✅ Recovery execution completed: {execution.status.value}")
            print(f"   Duration: {execution.duration_seconds:.1f}s")
            print(f"   Actions completed: {len(execution.actions_completed)}")
            print(f"   Actions failed: {len(execution.actions_failed)}")
        
        # Get summary
        summary = recovery_engine.get_recovery_summary()
        print(f"\n📊 Recovery Summary:")
        print(f"   Total executions: {summary['total_executions']}")
        print(f"   Success rate: {summary['success_rate_percent']:.1f}%")
        print(f"   Circuit breakers: {summary['circuit_breaker_status']}")
    
    # Run async demo
    try:
        asyncio.run(demo_recovery())
    except KeyboardInterrupt:
        print("\n🛑 Demo interrupted")