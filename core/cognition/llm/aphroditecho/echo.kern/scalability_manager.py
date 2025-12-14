#!/usr/bin/env python3
"""
Scalability Manager for Deep Tree Echo Architecture

Centralized management of horizontal scaling, load balancing, and resource optimization
across the distributed Deep Tree Echo system.

Features:
- Centralized scaling orchestration
- Resource monitoring and optimization
- Cost-effective resource management
- Integration with DTESN components
- Agent-Arena-Relation (AAR) scaling
- Performance-driven scaling decisions
"""

import asyncio
import time
import logging
import json
import sys
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from enum import Enum
try:
    import aioredis
    AIOREDIS_AVAILABLE = True
except ImportError:
    AIOREDIS_AVAILABLE = False
    aioredis = None

try:
    from aiohttp import ClientSession
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    ClientSession = None

# Add echo.kern to path for DTESN integration
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/echo.kern')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')

logger = logging.getLogger(__name__)


class ScalingAction(Enum):
    """Types of scaling actions"""
    SCALE_UP = "scale_up"
    SCALE_DOWN = "scale_down"
    MAINTAIN = "maintain"
    OPTIMIZE = "optimize"


class ResourceType(Enum):
    """Types of resources to manage"""
    COGNITIVE_SERVICE = "cognitive_service"
    CACHE_SERVICE = "cache_service"
    AGENT_INSTANCES = "agent_instances"
    DTESN_MEMBRANES = "dtesn_membranes"
    LOAD_BALANCER = "load_balancer"


@dataclass
class ResourceMetrics:
    """Resource utilization metrics"""
    resource_type: ResourceType
    instance_id: str
    cpu_usage: float = 0.0
    memory_usage: float = 0.0
    active_connections: int = 0
    request_rate: float = 0.0
    response_time_ms: float = 0.0
    error_rate: float = 0.0
    throughput: float = 0.0
    cost_per_hour: float = 0.0
    efficiency_score: float = 0.0
    last_updated: float = field(default_factory=time.time)
    
    @property
    def utilization_score(self) -> float:
        """Combined utilization score"""
        return (self.cpu_usage + self.memory_usage) / 2.0
    
    @property
    def performance_score(self) -> float:
        """Performance score based on response time and throughput"""
        if self.response_time_ms <= 0:
            return 1.0
        # Lower response time and higher throughput = better performance
        response_score = max(0.0, 1.0 - (self.response_time_ms / 1000.0))
        throughput_score = min(1.0, self.throughput / 100.0)
        return (response_score + throughput_score) / 2.0


@dataclass
class ScalingPolicy:
    """Scaling policy configuration"""
    resource_type: ResourceType
    min_instances: int = 1
    max_instances: int = 10
    target_utilization: float = 0.7
    scale_up_threshold: float = 0.8
    scale_down_threshold: float = 0.3
    scale_up_cooldown: int = 300  # seconds
    scale_down_cooldown: int = 600  # seconds
    cost_optimization_enabled: bool = True
    performance_weight: float = 0.6  # vs cost weight
    
    def should_scale_up(self, current_utilization: float, current_instances: int, last_scale_time: float) -> bool:
        """Check if should scale up"""
        cooldown_ok = time.time() - last_scale_time > self.scale_up_cooldown
        utilization_high = current_utilization > self.scale_up_threshold
        under_max = current_instances < self.max_instances
        return cooldown_ok and utilization_high and under_max
    
    def should_scale_down(self, current_utilization: float, current_instances: int, last_scale_time: float) -> bool:
        """Check if should scale down"""
        cooldown_ok = time.time() - last_scale_time > self.scale_down_cooldown
        utilization_low = current_utilization < self.scale_down_threshold
        above_min = current_instances > self.min_instances
        return cooldown_ok and utilization_low and above_min


@dataclass
class ScalingEvent:
    """Scaling event record"""
    timestamp: float
    resource_type: ResourceType
    action: ScalingAction
    instance_count_before: int
    instance_count_after: int
    trigger_metrics: ResourceMetrics
    cost_impact: float = 0.0
    performance_impact: float = 0.0
    success: bool = False
    error_message: Optional[str] = None


class ScalabilityManager:
    """
    Centralized scalability management for Deep Tree Echo system
    
    Enhanced for Task 6.2.3 with:
    - Dynamic resource allocation with adaptive thresholds
    - Load balancing for distributed DTESN operations  
    - Graceful degradation under resource constraints
    """
    
    def __init__(self,
                 redis_url: str = 'redis://localhost:6379',
                 monitoring_interval: int = 30,
                 cost_optimization: bool = True,
                 performance_weight: float = 0.6):
        
        self.redis_url = redis_url
        self.monitoring_interval = monitoring_interval
        self.cost_optimization = cost_optimization
        self.performance_weight = performance_weight
        
        # Redis connection
        self.redis: Optional[Any] = None
        
        # Resource tracking
        self.resource_metrics: Dict[str, ResourceMetrics] = {}
        self.resource_instances: Dict[ResourceType, List[str]] = {
            ResourceType.COGNITIVE_SERVICE: [],
            ResourceType.CACHE_SERVICE: [],
            ResourceType.AGENT_INSTANCES: [],
            ResourceType.DTESN_MEMBRANES: [],
            ResourceType.LOAD_BALANCER: []
        }
        
        # Enhanced for Task 6.2.3: Dynamic resource allocation
        self.system_load_history: List[float] = []
        self.performance_history: List[float] = []
        self.degradation_active = False
        self.current_system_load = 0.0
        self.load_balancer_pools: Dict[ResourceType, List[str]] = {}
        
        # Adaptive thresholds (will adjust based on system state)
        self.adaptive_thresholds = {
            'scale_up_base': 0.8,
            'scale_down_base': 0.3,
            'performance_threshold': 0.7,
            'degradation_threshold': 0.9
        }
        
        # Scaling policies
        self.scaling_policies: Dict[ResourceType, ScalingPolicy] = {
            ResourceType.COGNITIVE_SERVICE: ScalingPolicy(
                resource_type=ResourceType.COGNITIVE_SERVICE,
                min_instances=2,
                max_instances=20,
                target_utilization=0.7,
                scale_up_threshold=0.8,
                scale_down_threshold=0.3
            ),
            ResourceType.CACHE_SERVICE: ScalingPolicy(
                resource_type=ResourceType.CACHE_SERVICE,
                min_instances=1,
                max_instances=5,
                target_utilization=0.8,
                scale_up_threshold=0.9,
                scale_down_threshold=0.4
            ),
            ResourceType.AGENT_INSTANCES: ScalingPolicy(
                resource_type=ResourceType.AGENT_INSTANCES,
                min_instances=5,
                max_instances=100,
                target_utilization=0.6,
                scale_up_threshold=0.75,
                scale_down_threshold=0.25
            ),
            ResourceType.DTESN_MEMBRANES: ScalingPolicy(
                resource_type=ResourceType.DTESN_MEMBRANES,
                min_instances=3,
                max_instances=50,
                target_utilization=0.65,
                scale_up_threshold=0.8,
                scale_down_threshold=0.2
            )
        }
        
        # Scaling history
        self.scaling_events: List[ScalingEvent] = []
        self.last_scaling_times: Dict[ResourceType, float] = {}
        
        # Background tasks
        self.monitoring_task: Optional[asyncio.Task] = None
        self.optimization_task: Optional[asyncio.Task] = None
        self.cost_analysis_task: Optional[asyncio.Task] = None
        
        # Service URLs
        self.service_urls = {
            'load_balancer': 'http://load-balancer:8000',
            'cache_service': 'http://cache-service:8002',
            'cognitive_service_base': 'http://cognitive-service'
        }
        
        # Performance targets
        self.performance_targets = {
            'max_response_time_ms': 500,
            'min_throughput': 100,
            'max_error_rate': 0.05,
            'target_availability': 0.999
        }

    async def initialize(self):
        """Initialize the scalability manager"""
        logger.info("Initializing Scalability Manager...")
        
        # Connect to Redis
        try:
            if AIOREDIS_AVAILABLE and aioredis:
                self.redis = aioredis.from_url(self.redis_url, decode_responses=True)
                await self.redis.ping()
                logger.info("✅ Connected to Redis")
            else:
                logger.warning("aioredis not available, running without Redis")
                self.redis = None
        except Exception as e:
            logger.warning(f"Redis connection failed: {e}, continuing without Redis")
            self.redis = None
        
        # Initialize DTESN integration
        await self._initialize_dtesn_integration()
        
        # Discover existing service instances
        await self._discover_service_instances()
        
        # Start background tasks
        self.monitoring_task = asyncio.create_task(self._monitoring_loop())
        self.optimization_task = asyncio.create_task(self._optimization_loop())
        if self.cost_optimization:
            self.cost_analysis_task = asyncio.create_task(self._cost_analysis_loop())
        
        logger.info("Scalability Manager initialized")

    async def shutdown(self):
        """Shutdown the scalability manager"""
        logger.info("Shutting down Scalability Manager...")
        
        # Cancel background tasks
        tasks = [self.monitoring_task, self.optimization_task, self.cost_analysis_task]
        for task in tasks:
            if task:
                task.cancel()
        
        await asyncio.gather(*tasks, return_exceptions=True)
        
        # Close Redis connection
        if self.redis:
            await self.redis.close()
        
        logger.info("Scalability Manager shut down")

    async def _initialize_dtesn_integration(self):
        """Initialize DTESN integration components"""
        try:
            from dtesn_integration import DTESNIntegratedSystem, DTESNConfiguration, DTESNIntegrationMode
            from performance_monitor import PerformanceMonitor
            
            logger.info("✅ DTESN integration available")
            
            # Initialize performance monitor for DTESN metrics
            self.dtesn_performance_monitor = PerformanceMonitor()
            
        except ImportError as e:
            logger.warning(f"DTESN integration not available: {e}")
        except Exception as e:
            logger.error(f"Failed to initialize DTESN integration: {e}")

    async def _discover_service_instances(self):
        """Discover existing service instances"""
        # Check load balancer for registered services
        try:
            async with ClientSession() as session:
                async with session.get(f"{self.service_urls['load_balancer']}/metrics") as response:
                    if response.status == 200:
                        data = await response.json()
                        services = data.get('services', {})
                        
                        for service_type, instances in services.items():
                            if service_type == 'cognitive':
                                self.resource_instances[ResourceType.COGNITIVE_SERVICE] = [
                                    inst['id'] for inst in instances if isinstance(inst, dict)
                                ]
        
        except Exception as e:
            logger.error(f"Service discovery error: {e}")
        
        logger.info(f"Discovered service instances: {dict(self.resource_instances)}")

    async def _monitoring_loop(self):
        """Background monitoring loop"""
        while True:
            try:
                await self._collect_all_metrics()
                await asyncio.sleep(self.monitoring_interval)
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Monitoring error: {e}")
                await asyncio.sleep(self.monitoring_interval)

    async def _optimization_loop(self):
        """Background optimization loop"""
        while True:
            try:
                await self._evaluate_scaling_needs()
                await asyncio.sleep(60)  # Evaluate every minute
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Optimization error: {e}")
                await asyncio.sleep(60)

    async def _cost_analysis_loop(self):
        """Background cost analysis loop"""
        while True:
            try:
                await self._analyze_cost_efficiency()
                await asyncio.sleep(300)  # Analyze every 5 minutes
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Cost analysis error: {e}")
                await asyncio.sleep(300)

    async def _collect_all_metrics(self):
        """Collect metrics from all service instances"""
        # Collect from load balancer
        await self._collect_load_balancer_metrics()
        
        # Collect from cognitive services
        await self._collect_cognitive_service_metrics()
        
        # Collect from cache service
        await self._collect_cache_service_metrics()
        
        # Collect agent manager metrics
        await self._collect_agent_metrics()
        
        # Collect DTESN metrics
        await self._collect_dtesn_metrics()
        
        # Store aggregated metrics in Redis
        await self._store_aggregated_metrics()

    async def _collect_load_balancer_metrics(self):
        """Collect load balancer metrics"""
        try:
            if not AIOHTTP_AVAILABLE or not ClientSession:
                logger.debug("aiohttp not available, skipping load balancer metrics collection")
                return
            async with ClientSession() as session:
                async with session.get(f"{self.service_urls['load_balancer']}/metrics") as response:
                    if response.status == 200:
                        data = await response.json()
                        lb_metrics = data.get('load_balancer', {})
                        
                        metrics = ResourceMetrics(
                            resource_type=ResourceType.LOAD_BALANCER,
                            instance_id='lb-main',
                            cpu_usage=lb_metrics.get('cpu_usage', 0.0) / 100.0,
                            memory_usage=lb_metrics.get('memory_usage', 0.0) / 100.0,
                            active_connections=lb_metrics.get('active_services', 0),
                            request_rate=lb_metrics.get('requests_routed', 0) / 60.0,  # per second
                            response_time_ms=lb_metrics.get('average_response_time', 0.0) * 1000,
                            error_rate=lb_metrics.get('failed_requests', 0) / max(1, lb_metrics.get('requests_routed', 1))
                        )
                        
                        self.resource_metrics['lb-main'] = metrics
        
        except Exception as e:
            logger.error(f"Load balancer metrics collection error: {e}")

    async def _collect_cognitive_service_metrics(self):
        """Collect cognitive service metrics"""
        for instance_id in self.resource_instances[ResourceType.COGNITIVE_SERVICE]:
            try:
                port = 8001 if instance_id == 'cognitive-1' else 8003
                url = f"http://cognitive-service-{instance_id.split('-')[1]}:{8001}/metrics"
                
                async with ClientSession() as session:
                    async with session.get(url) as response:
                        if response.status == 200:
                            data = await response.json()
                            service_metrics = data.get('metrics', {})
                            system_metrics = data.get('system', {})
                            
                            metrics = ResourceMetrics(
                                resource_type=ResourceType.COGNITIVE_SERVICE,
                                instance_id=instance_id,
                                cpu_usage=system_metrics.get('cpu_usage', 0.0) / 100.0,
                                memory_usage=system_metrics.get('memory_usage', 0.0) / 100.0,
                                active_connections=system_metrics.get('queue_size', 0),
                                request_rate=service_metrics.get('total_requests', 0) / 60.0,
                                response_time_ms=service_metrics.get('average_processing_time', 0.0),
                                error_rate=service_metrics.get('failed_requests', 0) / max(1, service_metrics.get('total_requests', 1)),
                                throughput=service_metrics.get('successful_requests', 0) / 60.0
                            )
                            
                            self.resource_metrics[instance_id] = metrics
            
            except Exception as e:
                logger.error(f"Cognitive service {instance_id} metrics collection error: {e}")

    async def _collect_cache_service_metrics(self):
        """Collect cache service metrics"""
        try:
            async with ClientSession() as session:
                async with session.get(f"{self.service_urls['cache_service']}/stats") as response:
                    if response.status == 200:
                        data = await response.json()
                        global_stats = data.get('global_stats', {})
                        system_metrics = data.get('system', {})
                        
                        hit_ratio = global_stats.get('global_hit_ratio', 0.0)
                        
                        metrics = ResourceMetrics(
                            resource_type=ResourceType.CACHE_SERVICE,
                            instance_id='cache-main',
                            cpu_usage=0.1,  # Cache service is lightweight
                            memory_usage=system_metrics.get('memory_usage', 0.0) / 100.0,
                            active_connections=0,
                            request_rate=global_stats.get('total_requests', 0) / 60.0,
                            response_time_ms=5.0,  # Cache is fast
                            efficiency_score=hit_ratio,
                            throughput=global_stats.get('total_hits', 0) / 60.0
                        )
                        
                        self.resource_metrics['cache-main'] = metrics
        
        except Exception as e:
            logger.error(f"Cache service metrics collection error: {e}")

    async def _collect_agent_metrics(self):
        """Collect agent manager metrics"""
        try:
            # This would collect from the AAR agent manager
            # For now, simulate based on what we know exists
            agent_count = len(self.resource_instances.get(ResourceType.AGENT_INSTANCES, []))
            
            if agent_count == 0:
                # Simulate some agents for testing
                agent_count = 10
                self.resource_instances[ResourceType.AGENT_INSTANCES] = [f"agent-{i}" for i in range(agent_count)]
            
            # Simulate aggregate agent metrics
            avg_cpu = 0.4 + (agent_count / 100.0) * 0.3  # Higher load with more agents
            avg_memory = 0.3 + (agent_count / 100.0) * 0.2
            
            metrics = ResourceMetrics(
                resource_type=ResourceType.AGENT_INSTANCES,
                instance_id='agent-manager',
                cpu_usage=min(1.0, avg_cpu),
                memory_usage=min(1.0, avg_memory),
                active_connections=agent_count,
                throughput=agent_count * 2.0,  # Agents per second processing capability
                efficiency_score=0.8 if agent_count < 50 else 0.6
            )
            
            self.resource_metrics['agent-manager'] = metrics
        
        except Exception as e:
            logger.error(f"Agent metrics collection error: {e}")

    async def _collect_dtesn_metrics(self):
        """Collect DTESN membrane metrics"""
        try:
            if hasattr(self, 'dtesn_performance_monitor'):
                # This would collect from the DTESN performance monitor
                # For now, simulate DTESN metrics
                membrane_count = len(self.resource_instances.get(ResourceType.DTESN_MEMBRANES, []))
                
                if membrane_count == 0:
                    membrane_count = 5
                    self.resource_instances[ResourceType.DTESN_MEMBRANES] = [f"membrane-{i}" for i in range(membrane_count)]
                
                # Simulate DTESN processing metrics
                evolution_rate = 0.6 + (membrane_count / 50.0) * 0.3
                
                metrics = ResourceMetrics(
                    resource_type=ResourceType.DTESN_MEMBRANES,
                    instance_id='dtesn-system',
                    cpu_usage=min(1.0, 0.5 + (membrane_count / 50.0) * 0.4),
                    memory_usage=min(1.0, 0.3 + (membrane_count / 50.0) * 0.3),
                    throughput=membrane_count * 1.5,  # Membrane transitions per second
                    efficiency_score=min(1.0, evolution_rate),
                    response_time_ms=20.0 + (membrane_count * 2.0)  # More membranes = longer processing
                )
                
                self.resource_metrics['dtesn-system'] = metrics
        
        except Exception as e:
            logger.error(f"DTESN metrics collection error: {e}")

    async def _store_aggregated_metrics(self):
        """Store aggregated metrics in Redis"""
        try:
            aggregated = {
                'timestamp': time.time(),
                'resource_metrics': {}
            }
            
            for resource_id, metrics in self.resource_metrics.items():
                aggregated['resource_metrics'][resource_id] = {
                    'resource_type': metrics.resource_type.value,
                    'cpu_usage': metrics.cpu_usage,
                    'memory_usage': metrics.memory_usage,
                    'utilization_score': metrics.utilization_score,
                    'performance_score': metrics.performance_score,
                    'efficiency_score': metrics.efficiency_score,
                    'response_time_ms': metrics.response_time_ms,
                    'throughput': metrics.throughput,
                    'error_rate': metrics.error_rate
                }
            
            await self.redis.setex(
                'scalability_metrics',
                300,  # 5 minute expiry
                json.dumps(aggregated)
            )
        
        except Exception as e:
            logger.error(f"Failed to store aggregated metrics: {e}")

    async def _evaluate_scaling_needs(self):
        """Evaluate scaling needs for all resource types"""
        for resource_type in ResourceType:
            await self._evaluate_resource_scaling(resource_type)

    async def _evaluate_resource_scaling(self, resource_type: ResourceType):
        """
        Enhanced resource scaling evaluation with dynamic thresholds and graceful degradation.
        Implements Task 6.2.3 requirements.
        """
        policy = self.scaling_policies.get(resource_type)
        if not policy:
            return
        
        # Get current instances and metrics
        current_instances = self.resource_instances.get(resource_type, [])
        instance_count = len(current_instances)
        
        if instance_count == 0:
            return
        
        # Calculate average utilization
        resource_metrics = [
            metrics for metrics in self.resource_metrics.values()
            if metrics.resource_type == resource_type
        ]
        
        if not resource_metrics:
            return
        
        avg_utilization = sum(m.utilization_score for m in resource_metrics) / len(resource_metrics)
        avg_performance = sum(m.performance_score for m in resource_metrics) / len(resource_metrics)
        
        # Task 6.2.3: Calculate adaptive thresholds based on system state
        adaptive_up_threshold, adaptive_down_threshold = self._calculate_adaptive_thresholds(
            avg_utilization, avg_performance
        )
        
        last_scaling_time = self.last_scaling_times.get(resource_type, 0)
        
        # Update system state tracking
        self._update_system_load_tracking(avg_utilization, avg_performance)
        
        # Task 6.2.3: Check for graceful degradation needs
        if await self._should_activate_degradation(avg_utilization, avg_performance):
            await self._activate_graceful_degradation(resource_type, resource_metrics)
            return
        
        # Determine scaling action with adaptive thresholds
        action = ScalingAction.MAINTAIN
        target_count = instance_count
        
        if self._should_scale_up_adaptive(avg_utilization, instance_count, 
                                         last_scaling_time, adaptive_up_threshold, policy):
            if avg_performance < self.adaptive_thresholds['performance_threshold']:
                action = ScalingAction.SCALE_UP
                target_count = min(policy.max_instances, instance_count + 1)
        
        elif self._should_scale_down_adaptive(avg_utilization, instance_count,
                                            last_scaling_time, adaptive_down_threshold, policy):
            if avg_performance > 0.8 and not self.degradation_active:
                action = ScalingAction.SCALE_DOWN
                target_count = max(policy.min_instances, instance_count - 1)
        
        # Task 6.2.3: Enhanced load balancing for DTESN operations
        if resource_type == ResourceType.DTESN_MEMBRANES:
            await self._balance_dtesn_load(resource_metrics, action, target_count)
        
        # Cost optimization with degradation awareness
        if self.cost_optimization and action == ScalingAction.MAINTAIN and not self.degradation_active:
            cost_efficiency = await self._calculate_cost_efficiency(resource_type, resource_metrics)
            if cost_efficiency < 0.6 and avg_performance > 0.7:
                action = ScalingAction.OPTIMIZE
        
        # Execute scaling action if needed
        if action != ScalingAction.MAINTAIN:
            await self._execute_scaling_action(
                resource_type, 
                action, 
                instance_count, 
                target_count,
                resource_metrics[0] if resource_metrics else None
            )

    async def _calculate_cost_efficiency(self, resource_type: ResourceType, metrics: List[ResourceMetrics]) -> float:
        """Calculate cost efficiency for a resource type"""
        if not metrics:
            return 1.0
        
        # Simplified cost efficiency calculation
        avg_utilization = sum(m.utilization_score for m in metrics) / len(metrics)
        avg_performance = sum(m.performance_score for m in metrics) / len(metrics)
        
        # Efficiency = (Performance * Utilization) / (1 + Error Rate)
        avg_error_rate = sum(m.error_rate for m in metrics) / len(metrics)
        efficiency = (avg_performance * avg_utilization) / (1 + avg_error_rate)
        
        return efficiency

    async def _execute_scaling_action(self, 
                                    resource_type: ResourceType,
                                    action: ScalingAction,
                                    current_count: int,
                                    target_count: int,
                                    trigger_metrics: Optional[ResourceMetrics]):
        """Execute a scaling action"""
        logger.info(f"Executing {action.value} for {resource_type.value}: {current_count} -> {target_count}")
        
        success = False
        error_message = None
        
        try:
            if action == ScalingAction.SCALE_UP:
                success = await self._scale_up_resource(resource_type, target_count - current_count)
            elif action == ScalingAction.SCALE_DOWN:
                success = await self._scale_down_resource(resource_type, current_count - target_count)
            elif action == ScalingAction.OPTIMIZE:
                success = await self._optimize_resource(resource_type)
            
            if success:
                self.last_scaling_times[resource_type] = time.time()
        
        except Exception as e:
            error_message = str(e)
            logger.error(f"Scaling action failed: {e}")
        
        # Record scaling event
        event = ScalingEvent(
            timestamp=time.time(),
            resource_type=resource_type,
            action=action,
            instance_count_before=current_count,
            instance_count_after=target_count if success else current_count,
            trigger_metrics=trigger_metrics,
            success=success,
            error_message=error_message
        )
        
        self.scaling_events.append(event)
        
        # Store event in Redis
        await self._store_scaling_event(event)

    async def _scale_up_resource(self, resource_type: ResourceType, count: int) -> bool:
        """Scale up a resource type"""
        if resource_type == ResourceType.COGNITIVE_SERVICE:
            # In a real implementation, this would trigger container orchestration
            # For now, we simulate by adding to the instance list
            current_instances = self.resource_instances[resource_type]
            for i in range(count):
                new_id = f"cognitive-{len(current_instances) + i + 1}"
                current_instances.append(new_id)
            logger.info(f"🚀 Scaled up cognitive services: +{count} instances")
            return True
        
        elif resource_type == ResourceType.AGENT_INSTANCES:
            # Scale up agent instances through AAR
            current_instances = self.resource_instances[resource_type]
            for i in range(count):
                new_id = f"agent-{len(current_instances) + i + 1}"
                current_instances.append(new_id)
            logger.info(f"🚀 Scaled up agent instances: +{count} agents")
            return True
        
        elif resource_type == ResourceType.DTESN_MEMBRANES:
            # Scale up DTESN membranes
            current_instances = self.resource_instances[resource_type]
            for i in range(count):
                new_id = f"membrane-{len(current_instances) + i + 1}"
                current_instances.append(new_id)
            logger.info(f"🚀 Scaled up DTESN membranes: +{count} membranes")
            return True
        
        return False

    async def _scale_down_resource(self, resource_type: ResourceType, count: int) -> bool:
        """Scale down a resource type"""
        current_instances = self.resource_instances.get(resource_type, [])
        if len(current_instances) <= count:
            return False
        
        # Remove instances (in real implementation, would gracefully shutdown)
        for _ in range(count):
            if current_instances:
                removed = current_instances.pop()
                logger.info(f"📉 Scaled down {resource_type.value}: removed {removed}")
        
        return True

    async def _optimize_resource(self, resource_type: ResourceType) -> bool:
        """Optimize a resource type for cost efficiency"""
        logger.info(f"🔧 Optimizing {resource_type.value} for cost efficiency")
        
        # Optimization strategies would be implemented here
        # For now, just log the optimization
        
        return True

    async def _analyze_cost_efficiency(self):
        """Analyze cost efficiency across all resources"""
        total_cost = 0.0
        total_value = 0.0
        
        efficiency_by_type = {}
        
        for resource_type in ResourceType:
            resource_metrics = [
                metrics for metrics in self.resource_metrics.values()
                if metrics.resource_type == resource_type
            ]
            
            if resource_metrics:
                efficiency = await self._calculate_cost_efficiency(resource_type, resource_metrics)
                efficiency_by_type[resource_type.value] = efficiency
                
                # Simulate cost calculation
                instance_count = len(self.resource_instances.get(resource_type, []))
                type_cost = instance_count * 0.1  # $0.10 per hour per instance
                type_value = efficiency * type_cost
                
                total_cost += type_cost
                total_value += type_value
        
        overall_efficiency = total_value / total_cost if total_cost > 0 else 1.0
        
        cost_analysis = {
            'timestamp': time.time(),
            'overall_efficiency': overall_efficiency,
            'total_estimated_cost_per_hour': total_cost,
            'efficiency_by_type': efficiency_by_type,
            'recommendations': await self._generate_cost_recommendations(efficiency_by_type)
        }
        
        if self.redis:
            await self.redis.setex(
                'cost_analysis',
                3600,  # 1 hour expiry
                json.dumps(cost_analysis)
            )
        
        logger.info(f"Cost efficiency analysis: Overall={overall_efficiency:.2f}, Cost/hour=${total_cost:.2f}")

    async def _generate_cost_recommendations(self, efficiency_by_type: Dict[str, float]) -> List[str]:
        """Generate cost optimization recommendations"""
        recommendations = []
        
        for resource_type, efficiency in efficiency_by_type.items():
            if efficiency < 0.5:
                recommendations.append(f"Consider optimizing {resource_type} (efficiency: {efficiency:.2f})")
            elif efficiency > 0.9:
                recommendations.append(f"{resource_type} is highly efficient (efficiency: {efficiency:.2f})")
        
        return recommendations

    async def _store_scaling_event(self, event: ScalingEvent):
        """Store scaling event in Redis"""
        try:
            event_data = {
                'timestamp': event.timestamp,
                'resource_type': event.resource_type.value,
                'action': event.action.value,
                'instance_count_before': event.instance_count_before,
                'instance_count_after': event.instance_count_after,
                'success': event.success,
                'error_message': event.error_message
            }
            
            await self.redis.lpush('scaling_events', json.dumps(event_data))
            
            # Keep only last 1000 events
            await self.redis.ltrim('scaling_events', 0, 999)
        
        except Exception as e:
            logger.error(f"Failed to store scaling event: {e}")

    async def get_scaling_status(self) -> Dict[str, Any]:
        """Get current scaling status"""
        status = {
            'timestamp': time.time(),
            'resource_instances': {
                resource_type.value: len(instances)
                for resource_type, instances in self.resource_instances.items()
            },
            'resource_utilization': {},
            'recent_scaling_events': len([
                e for e in self.scaling_events
                if time.time() - e.timestamp < 3600  # Last hour
            ]),
            'performance_summary': {},
            'cost_efficiency': {}
        }
        
        # Add utilization and performance data
        for resource_type in ResourceType:
            resource_metrics = [
                metrics for metrics in self.resource_metrics.values()
                if metrics.resource_type == resource_type
            ]
            
            if resource_metrics:
                avg_utilization = sum(m.utilization_score for m in resource_metrics) / len(resource_metrics)
                avg_performance = sum(m.performance_score for m in resource_metrics) / len(resource_metrics)
                
                status['resource_utilization'][resource_type.value] = avg_utilization
                status['performance_summary'][resource_type.value] = avg_performance
        
        return status

    async def trigger_manual_scaling(self, 
                                   resource_type: ResourceType,
                                   action: ScalingAction,
                                   target_count: Optional[int] = None) -> bool:
        """Manually trigger scaling action"""
        current_instances = self.resource_instances.get(resource_type, [])
        current_count = len(current_instances)
        
        if target_count is None:
            if action == ScalingAction.SCALE_UP:
                target_count = current_count + 1
            elif action == ScalingAction.SCALE_DOWN:
                target_count = max(1, current_count - 1)
            else:
                target_count = current_count
        
        # Get current metrics for trigger
        trigger_metrics = next(
            (metrics for metrics in self.resource_metrics.values() 
             if metrics.resource_type == resource_type),
            None
        )
        
        await self._execute_scaling_action(
            resource_type,
            action,
            current_count,
            target_count,
            trigger_metrics
        )
        
        return True

    def _calculate_adaptive_thresholds(self, avg_utilization: float, avg_performance: float) -> tuple:
        """
        Task 6.2.3: Calculate adaptive scaling thresholds based on system state.
        Dynamic resource allocation implementation.
        """
        # Base thresholds
        base_up = self.adaptive_thresholds['scale_up_base']
        base_down = self.adaptive_thresholds['scale_down_base']
        
        # Adjust based on recent performance history
        if len(self.performance_history) >= 3:
            recent_trend = sum(self.performance_history[-3:]) / 3
            if recent_trend < 0.6:  # Poor recent performance
                adaptive_up = max(0.6, base_up - 0.1)  # Scale up more aggressively
                adaptive_down = max(0.1, base_down - 0.1)
            elif recent_trend > 0.9:  # Excellent performance
                adaptive_up = min(0.9, base_up + 0.1)  # Scale up more conservatively
                adaptive_down = min(0.5, base_down + 0.1)
            else:
                adaptive_up = base_up
                adaptive_down = base_down
        else:
            adaptive_up = base_up
            adaptive_down = base_down
        
        # Adjust based on current system load
        if self.current_system_load > 0.85:
            adaptive_up *= 0.8  # Scale up more aggressively under high load
        elif self.current_system_load < 0.3:
            adaptive_down *= 1.2  # Scale down more aggressively under low load
        
        return adaptive_up, adaptive_down
    
    def _update_system_load_tracking(self, avg_utilization: float, avg_performance: float):
        """Update system load and performance history for adaptive scaling."""
        self.system_load_history.append(avg_utilization)
        self.performance_history.append(avg_performance)
        
        # Keep only recent history (last 10 measurements)
        if len(self.system_load_history) > 10:
            self.system_load_history.pop(0)
        if len(self.performance_history) > 10:
            self.performance_history.pop(0)
        
        # Update current system load as weighted average
        if self.system_load_history:
            self.current_system_load = sum(self.system_load_history) / len(self.system_load_history)
    
    async def _should_activate_degradation(self, avg_utilization: float, avg_performance: float) -> bool:
        """
        Task 6.2.3: Determine if graceful degradation should be activated.
        """
        degradation_threshold = self.adaptive_thresholds['degradation_threshold']
        
        # Activate degradation if:
        # 1. Utilization is very high AND performance is dropping
        # 2. System has been under sustained load
        sustained_high_load = (
            len(self.system_load_history) >= 5 and 
            all(load > 0.8 for load in self.system_load_history[-5:])
        )
        
        performance_dropping = (
            len(self.performance_history) >= 3 and
            all(perf < 0.6 for perf in self.performance_history[-3:])
        )
        
        should_degrade = (
            avg_utilization > degradation_threshold or
            (sustained_high_load and performance_dropping)
        )
        
        return should_degrade and not self.degradation_active
    
    async def _activate_graceful_degradation(self, resource_type: ResourceType, metrics: List[ResourceMetrics]):
        """
        Task 6.2.3: Activate graceful degradation under resource constraints.
        """
        if self.degradation_active:
            return
        
        logger.warning(f"🚨 Activating graceful degradation for {resource_type.value}")
        self.degradation_active = True
        
        # Implement degradation strategies per resource type
        if resource_type == ResourceType.DTESN_MEMBRANES:
            await self._degrade_dtesn_processing()
        elif resource_type == ResourceType.COGNITIVE_SERVICE:
            await self._degrade_cognitive_services()
        elif resource_type == ResourceType.AGENT_INSTANCES:
            await self._degrade_agent_processing()
        
        # Store degradation event in Redis for monitoring
        if self.redis:
            await self.redis.setex(
                f'degradation_active_{resource_type.value}',
                300,  # 5 minutes
                json.dumps({
                    'timestamp': time.time(),
                    'resource_type': resource_type.value,
                    'trigger_metrics': [m.dict() if hasattr(m, 'dict') else vars(m) for m in metrics]
                })
            )
    
    async def _degrade_dtesn_processing(self):
        """Reduce DTESN processing complexity during resource constraints."""
        # Could reduce membrane depth, ESN reservoir size, etc.
        logger.info("📉 DTESN processing degraded: reducing membrane complexity")
    
    async def _degrade_cognitive_services(self):
        """Reduce cognitive service capabilities during resource constraints.""" 
        logger.info("📉 Cognitive services degraded: reducing concurrent processing")
    
    async def _degrade_agent_processing(self):
        """Reduce agent processing capabilities during resource constraints."""
        logger.info("📉 Agent processing degraded: reducing agent simulation complexity")
    
    def _should_scale_up_adaptive(self, avg_utilization: float, instance_count: int, 
                                 last_scaling_time: float, adaptive_threshold: float, 
                                 policy: ScalingPolicy) -> bool:
        """Adaptive scale-up decision with dynamic thresholds."""
        cooldown_ok = time.time() - last_scaling_time > policy.scale_up_cooldown
        utilization_high = avg_utilization > adaptive_threshold
        under_max = instance_count < policy.max_instances
        return cooldown_ok and utilization_high and under_max
    
    def _should_scale_down_adaptive(self, avg_utilization: float, instance_count: int,
                                   last_scaling_time: float, adaptive_threshold: float,
                                   policy: ScalingPolicy) -> bool:
        """Adaptive scale-down decision with dynamic thresholds."""
        cooldown_ok = time.time() - last_scaling_time > policy.scale_down_cooldown
        utilization_low = avg_utilization < adaptive_threshold
        above_min = instance_count > policy.min_instances
        return cooldown_ok and utilization_low and above_min
    
    async def _balance_dtesn_load(self, metrics: List[ResourceMetrics], action: ScalingAction, target_count: int):
        """
        Task 6.2.3: Implement load balancing for distributed DTESN operations.
        """
        if not metrics:
            return
        
        # Create or update load balancer pools for DTESN membranes
        membrane_ids = [m.instance_id for m in metrics if m.resource_type == ResourceType.DTESN_MEMBRANES]
        
        if ResourceType.DTESN_MEMBRANES not in self.load_balancer_pools:
            self.load_balancer_pools[ResourceType.DTESN_MEMBRANES] = []
        
        current_pool = self.load_balancer_pools[ResourceType.DTESN_MEMBRANES]
        
        # Balance load based on individual membrane performance
        membrane_loads = []
        for metric in metrics:
            if metric.resource_type == ResourceType.DTESN_MEMBRANES:
                load_score = (metric.cpu_usage * 0.4 + 
                            metric.memory_usage * 0.3 + 
                            (1 - metric.efficiency_score) * 0.3)
                membrane_loads.append((metric.instance_id, load_score))
        
        # Sort by load (ascending - least loaded first)
        membrane_loads.sort(key=lambda x: x[1])
        
        # Update load balancer pool with balanced distribution
        balanced_pool = [membrane_id for membrane_id, _ in membrane_loads]
        self.load_balancer_pools[ResourceType.DTESN_MEMBRANES] = balanced_pool
        
        logger.info(f"🔄 DTESN load balanced across {len(balanced_pool)} membranes")
        
        # Store load balancing info in Redis
        if self.redis:
            await self.redis.setex(
                'dtesn_load_balance',
                60,  # 1 minute
                json.dumps({
                    'timestamp': time.time(),
                    'balanced_pool': balanced_pool,
                    'membrane_loads': membrane_loads
                })
            )
    
    async def deactivate_degradation(self):
        """Deactivate graceful degradation when conditions improve."""
        if not self.degradation_active:
            return
        
        # Check if conditions have improved
        if (self.current_system_load < 0.7 and 
            len(self.performance_history) >= 3 and
            all(perf > 0.7 for perf in self.performance_history[-3:])):
            
            logger.info("✅ Deactivating graceful degradation - conditions improved")
            self.degradation_active = False
            
            # Clear degradation flags in Redis
            if self.redis:
                for resource_type in ResourceType:
                    await self.redis.delete(f'degradation_active_{resource_type.value}')


async def main():
    """Main entry point for running the scalability manager"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Create and run scalability manager
    manager = ScalabilityManager()
    
    try:
        await manager.initialize()
        
        logger.info("🚀 Scalability Manager is running")
        logger.info("Monitoring and managing resource scaling for Deep Tree Echo system")
        
        # Run forever
        await asyncio.Future()
    
    except KeyboardInterrupt:
        logger.info("Shutting down...")
    finally:
        await manager.shutdown()


if __name__ == '__main__':
    asyncio.run(main())