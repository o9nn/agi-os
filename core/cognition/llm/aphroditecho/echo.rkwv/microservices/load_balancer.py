#!/usr/bin/env python3
"""
Load Balancer Microservice for Deep Tree Echo Architecture

Implements intelligent request routing, auto-scaling, and service discovery
for the distributed Deep Tree Echo system.

Features:
- Multiple load balancing strategies (round-robin, weighted, least-connections)
- Auto-scaling based on resource utilization
- Health checking and failover
- Service registry management  
- Real-time metrics collection
"""

import asyncio
import time
import logging
import json
import os
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum
from aiohttp import web, ClientSession, ClientError
import aioredis
import psutil
from collections import defaultdict, deque

logger = logging.getLogger(__name__)


class LoadBalancingStrategy(Enum):
    """Load balancing strategy options"""
    ROUND_ROBIN = "round_robin"
    WEIGHTED = "weighted" 
    LEAST_CONNECTIONS = "least_connections"
    LEAST_RESPONSE_TIME = "least_response_time"
    CPU_BASED = "cpu_based"


@dataclass
class ServiceInstance:
    """Service instance configuration"""
    id: str
    host: str
    port: int
    weight: float = 1.0
    active_connections: int = 0
    total_requests: int = 0
    avg_response_time: float = 0.0
    cpu_usage: float = 0.0
    memory_usage: float = 0.0
    last_health_check: float = 0.0
    is_healthy: bool = True
    
    @property
    def endpoint(self) -> str:
        return f"http://{self.host}:{self.port}"


@dataclass
class AutoScalingConfig:
    """Auto-scaling configuration"""
    enabled: bool = True
    min_instances: int = 1
    max_instances: int = 10
    scale_up_threshold: float = 0.8
    scale_down_threshold: float = 0.3
    scale_up_cooldown: int = 300  # 5 minutes
    scale_down_cooldown: int = 600  # 10 minutes
    health_check_interval: int = 30


class LoadBalancerService:
    """
    Intelligent load balancer with auto-scaling capabilities
    """
    
    def __init__(self, 
                 port: int = 8000,
                 strategy: LoadBalancingStrategy = LoadBalancingStrategy.WEIGHTED,
                 auto_scaling_config: Optional[AutoScalingConfig] = None):
        self.port = port
        self.strategy = strategy
        self.auto_scaling = auto_scaling_config or AutoScalingConfig()
        
        # Service registry
        self.services: Dict[str, List[ServiceInstance]] = defaultdict(list)
        self.service_round_robin_counters: Dict[str, int] = defaultdict(int)
        
        # Metrics and monitoring
        self.metrics: Dict[str, Any] = {
            'requests_routed': 0,
            'failed_requests': 0,
            'average_response_time': 0.0,
            'active_services': 0
        }
        self.request_times: deque = deque(maxlen=1000)
        
        # Redis connection for distributed state
        self.redis: Optional[aioredis.Redis] = None
        
        # Background tasks
        self.health_check_task: Optional[asyncio.Task] = None
        self.auto_scaling_task: Optional[asyncio.Task] = None
        self.metrics_task: Optional[asyncio.Task] = None
        
        # Last scaling actions
        self.last_scale_up = 0
        self.last_scale_down = 0

    async def initialize(self):
        """Initialize the load balancer service"""
        logger.info("Initializing Load Balancer Service...")
        
        # Connect to Redis for distributed state
        redis_url = os.getenv('REDIS_URL', 'redis://localhost:6379')
        try:
            self.redis = aioredis.from_url(redis_url, decode_responses=True)
            await self.redis.ping()
            logger.info("✅ Connected to Redis")
        except Exception as e:
            logger.warning(f"Redis connection failed: {e}, continuing without distributed state")
        
        # Start background tasks
        self.health_check_task = asyncio.create_task(self._health_check_loop())
        if self.auto_scaling.enabled:
            self.auto_scaling_task = asyncio.create_task(self._auto_scaling_loop())
        self.metrics_task = asyncio.create_task(self._metrics_collection_loop())
        
        logger.info("Load Balancer Service initialized")

    async def shutdown(self):
        """Shutdown the load balancer service"""
        logger.info("Shutting down Load Balancer Service...")
        
        # Cancel background tasks
        for task in [self.health_check_task, self.auto_scaling_task, self.metrics_task]:
            if task:
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        
        # Close Redis connection
        if self.redis:
            await self.redis.close()
        
        logger.info("Load Balancer Service shut down")

    def register_service(self, service_type: str, instance: ServiceInstance):
        """Register a service instance"""
        self.services[service_type].append(instance)
        logger.info(f"Registered {service_type} service: {instance.endpoint}")
        
        # Update metrics
        self.metrics['active_services'] = sum(len(instances) for instances in self.services.values())

    def unregister_service(self, service_type: str, instance_id: str):
        """Unregister a service instance"""
        instances = self.services.get(service_type, [])
        self.services[service_type] = [inst for inst in instances if inst.id != instance_id]
        logger.info(f"Unregistered {service_type} service: {instance_id}")
        
        # Update metrics
        self.metrics['active_services'] = sum(len(instances) for instances in self.services.values())

    async def route_request(self, service_type: str, request_data: Dict[str, Any]) -> Tuple[bool, Any]:
        """Route request to appropriate service instance"""
        instances = self.services.get(service_type, [])
        healthy_instances = [inst for inst in instances if inst.is_healthy]
        
        if not healthy_instances:
            logger.error(f"No healthy instances available for {service_type}")
            self.metrics['failed_requests'] += 1
            return False, {"error": "No healthy instances available"}
        
        # Select instance based on load balancing strategy
        selected_instance = await self._select_instance(service_type, healthy_instances)
        
        if not selected_instance:
            self.metrics['failed_requests'] += 1
            return False, {"error": "Failed to select instance"}
        
        # Make request to selected instance
        start_time = time.time()
        success, response = await self._make_request(selected_instance, request_data)
        response_time = time.time() - start_time
        
        # Update metrics
        self.metrics['requests_routed'] += 1
        self.request_times.append(response_time)
        if self.request_times:
            self.metrics['average_response_time'] = sum(self.request_times) / len(self.request_times)
        
        # Update instance metrics
        selected_instance.total_requests += 1
        selected_instance.avg_response_time = (
            (selected_instance.avg_response_time * (selected_instance.total_requests - 1) + response_time)
            / selected_instance.total_requests
        )
        
        if not success:
            self.metrics['failed_requests'] += 1
        
        return success, response

    async def _select_instance(self, service_type: str, instances: List[ServiceInstance]) -> Optional[ServiceInstance]:
        """Select instance based on load balancing strategy"""
        if not instances:
            return None
        
        if self.strategy == LoadBalancingStrategy.ROUND_ROBIN:
            counter = self.service_round_robin_counters[service_type]
            selected = instances[counter % len(instances)]
            self.service_round_robin_counters[service_type] = (counter + 1) % len(instances)
            return selected
        
        elif self.strategy == LoadBalancingStrategy.LEAST_CONNECTIONS:
            return min(instances, key=lambda x: x.active_connections)
        
        elif self.strategy == LoadBalancingStrategy.LEAST_RESPONSE_TIME:
            return min(instances, key=lambda x: x.avg_response_time)
        
        elif self.strategy == LoadBalancingStrategy.CPU_BASED:
            return min(instances, key=lambda x: x.cpu_usage)
        
        elif self.strategy == LoadBalancingStrategy.WEIGHTED:
            # Weighted selection based on inverse resource utilization
            weights = []
            for instance in instances:
                # Calculate weight based on CPU, memory, and active connections
                cpu_weight = max(0.1, 1.0 - instance.cpu_usage)
                memory_weight = max(0.1, 1.0 - instance.memory_usage)
                connection_weight = max(0.1, 1.0 - (instance.active_connections / 100.0))
                combined_weight = (cpu_weight + memory_weight + connection_weight) * instance.weight
                weights.append(combined_weight)
            
            if not weights:
                return instances[0]
            
            # Select based on weighted probability
            import random
            total_weight = sum(weights)
            if total_weight <= 0:
                return instances[0]
            
            rand_val = random.uniform(0, total_weight)
            cumulative = 0
            for i, weight in enumerate(weights):
                cumulative += weight
                if rand_val <= cumulative:
                    return instances[i]
            
            return instances[-1]
        
        return instances[0]

    async def _make_request(self, instance: ServiceInstance, request_data: Dict[str, Any]) -> Tuple[bool, Any]:
        """Make HTTP request to service instance"""
        instance.active_connections += 1
        
        try:
            async with ClientSession() as session:
                async with session.post(
                    f"{instance.endpoint}/process",
                    json=request_data,
                    timeout=30
                ) as response:
                    if response.status == 200:
                        result = await response.json()
                        return True, result
                    else:
                        error_text = await response.text()
                        logger.error(f"Request failed with status {response.status}: {error_text}")
                        return False, {"error": f"HTTP {response.status}", "details": error_text}
        
        except ClientError as e:
            logger.error(f"Client error for {instance.endpoint}: {e}")
            return False, {"error": "Connection error", "details": str(e)}
        except Exception as e:
            logger.error(f"Unexpected error for {instance.endpoint}: {e}")
            return False, {"error": "Internal error", "details": str(e)}
        finally:
            instance.active_connections -= 1

    async def _health_check_loop(self):
        """Background health checking loop"""
        while True:
            try:
                await self._perform_health_checks()
                await asyncio.sleep(self.auto_scaling.health_check_interval)
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Health check error: {e}")
                await asyncio.sleep(self.auto_scaling.health_check_interval)

    async def _perform_health_checks(self):
        """Perform health checks on all service instances"""
        for service_type, instances in self.services.items():
            for instance in instances:
                try:
                    async with ClientSession() as session:
                        async with session.get(
                            f"{instance.endpoint}/health",
                            timeout=10
                        ) as response:
                            instance.is_healthy = response.status == 200
                            instance.last_health_check = time.time()
                            
                            if response.status == 200:
                                # Update resource metrics if available
                                try:
                                    health_data = await response.json()
                                    instance.cpu_usage = health_data.get('cpu_usage', instance.cpu_usage)
                                    instance.memory_usage = health_data.get('memory_usage', instance.memory_usage)
                                except:
                                    pass
                
                except Exception as e:
                    logger.warning(f"Health check failed for {instance.endpoint}: {e}")
                    instance.is_healthy = False
                    instance.last_health_check = time.time()

    async def _auto_scaling_loop(self):
        """Background auto-scaling loop"""
        while True:
            try:
                await self._evaluate_scaling()
                await asyncio.sleep(60)  # Check every minute
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Auto-scaling error: {e}")
                await asyncio.sleep(60)

    async def _evaluate_scaling(self):
        """Evaluate and perform auto-scaling actions"""
        current_time = time.time()
        
        for service_type, instances in self.services.items():
            if not instances:
                continue
            
            healthy_instances = [inst for inst in instances if inst.is_healthy]
            if not healthy_instances:
                continue
            
            # Calculate average resource utilization
            avg_cpu = sum(inst.cpu_usage for inst in healthy_instances) / len(healthy_instances)
            avg_memory = sum(inst.memory_usage for inst in healthy_instances) / len(healthy_instances)
            avg_connections = sum(inst.active_connections for inst in healthy_instances) / len(healthy_instances)
            
            # Combined utilization score
            utilization_score = (avg_cpu + avg_memory) / 2.0 + (avg_connections / 50.0)  # Normalize connections
            
            # Scale up decision
            if (utilization_score > self.auto_scaling.scale_up_threshold and
                len(instances) < self.auto_scaling.max_instances and
                current_time - self.last_scale_up > self.auto_scaling.scale_up_cooldown):
                
                logger.info(f"Scaling up {service_type} (utilization: {utilization_score:.2f})")
                await self._scale_up_service(service_type)
                self.last_scale_up = current_time
            
            # Scale down decision
            elif (utilization_score < self.auto_scaling.scale_down_threshold and
                  len(instances) > self.auto_scaling.min_instances and
                  current_time - self.last_scale_down > self.auto_scaling.scale_down_cooldown):
                
                logger.info(f"Scaling down {service_type} (utilization: {utilization_score:.2f})")
                await self._scale_down_service(service_type)
                self.last_scale_down = current_time

    async def _scale_up_service(self, service_type: str):
        """Scale up a service by adding a new instance"""
        # In a real implementation, this would trigger container orchestration
        # For now, we log the scaling action
        logger.info(f"🚀 SCALE UP triggered for {service_type}")
        
        if self.redis:
            scaling_event = {
                'service_type': service_type,
                'action': 'scale_up',
                'timestamp': time.time(),
                'trigger_reason': 'high_utilization'
            }
            await self.redis.lpush('scaling_events', json.dumps(scaling_event))

    async def _scale_down_service(self, service_type: str):
        """Scale down a service by removing an instance"""
        instances = self.services[service_type]
        if len(instances) <= self.auto_scaling.min_instances:
            return
        
        # Find least utilized instance to remove
        least_utilized = min(instances, key=lambda x: x.active_connections + x.cpu_usage)
        
        logger.info(f"📉 SCALE DOWN triggered for {service_type}, removing {least_utilized.id}")
        
        # Remove the instance
        self.unregister_service(service_type, least_utilized.id)
        
        if self.redis:
            scaling_event = {
                'service_type': service_type,
                'action': 'scale_down',
                'timestamp': time.time(),
                'trigger_reason': 'low_utilization',
                'removed_instance': least_utilized.id
            }
            await self.redis.lpush('scaling_events', json.dumps(scaling_event))

    async def _metrics_collection_loop(self):
        """Background metrics collection loop"""
        while True:
            try:
                await self._collect_system_metrics()
                await asyncio.sleep(30)  # Collect metrics every 30 seconds
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Metrics collection error: {e}")
                await asyncio.sleep(30)

    async def _collect_system_metrics(self):
        """Collect system-level metrics"""
        # System metrics
        cpu_percent = psutil.cpu_percent()
        memory = psutil.virtual_memory()
        
        system_metrics = {
            'timestamp': time.time(),
            'load_balancer': {
                'cpu_usage': cpu_percent,
                'memory_usage': memory.percent,
                'active_services': self.metrics['active_services'],
                'requests_routed': self.metrics['requests_routed'],
                'failed_requests': self.metrics['failed_requests'],
                'average_response_time': self.metrics['average_response_time']
            }
        }
        
        if self.redis:
            await self.redis.setex(
                'lb_metrics',
                300,  # 5 minute expiry
                json.dumps(system_metrics)
            )

    # HTTP API endpoints
    async def health_handler(self, request):
        """Health check endpoint"""
        return web.json_response({
            'status': 'healthy',
            'timestamp': time.time(),
            'metrics': self.metrics,
            'services': {
                service_type: len(instances) 
                for service_type, instances in self.services.items()
            }
        })

    async def metrics_handler(self, request):
        """Metrics endpoint"""
        detailed_metrics = {
            'load_balancer': self.metrics,
            'services': {}
        }
        
        for service_type, instances in self.services.items():
            detailed_metrics['services'][service_type] = [
                {
                    'id': inst.id,
                    'endpoint': inst.endpoint,
                    'is_healthy': inst.is_healthy,
                    'active_connections': inst.active_connections,
                    'total_requests': inst.total_requests,
                    'avg_response_time': inst.avg_response_time,
                    'cpu_usage': inst.cpu_usage,
                    'memory_usage': inst.memory_usage
                }
                for inst in instances
            ]
        
        return web.json_response(detailed_metrics)

    async def route_handler(self, request):
        """Main routing endpoint"""
        try:
            request_data = await request.json()
            service_type = request_data.get('service_type', 'cognitive')
            
            success, response = await self.route_request(service_type, request_data)
            
            if success:
                return web.json_response(response)
            else:
                return web.json_response(response, status=503)
        
        except Exception as e:
            logger.error(f"Route handler error: {e}")
            return web.json_response({'error': 'Internal server error'}, status=500)

    def create_app(self):
        """Create the web application"""
        app = web.Application()
        app.router.add_get('/health', self.health_handler)
        app.router.add_get('/metrics', self.metrics_handler)
        app.router.add_post('/route', self.route_handler)
        return app

    async def run(self):
        """Run the load balancer service"""
        await self.initialize()
        
        # Register some default cognitive services for testing
        # In production, these would be registered by the actual services
        test_instances = [
            ServiceInstance(
                id="cognitive-1",
                host="cognitive-service-1", 
                port=8001,
                weight=1.0
            ),
            ServiceInstance(
                id="cognitive-2", 
                host="cognitive-service-2",
                port=8001,
                weight=1.0
            )
        ]
        
        for instance in test_instances:
            self.register_service("cognitive", instance)
        
        app = self.create_app()
        runner = web.AppRunner(app)
        await runner.setup()
        
        site = web.TCPSite(runner, '0.0.0.0', self.port)
        await site.start()
        
        logger.info(f"🚀 Load Balancer Service running on port {self.port}")
        logger.info(f"Strategy: {self.strategy.value}")
        logger.info(f"Auto-scaling: {'enabled' if self.auto_scaling.enabled else 'disabled'}")
        
        try:
            await asyncio.Future()  # Run forever
        finally:
            await self.shutdown()


async def main():
    """Main entry point"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Configuration from environment
    port = int(os.getenv('LB_PORT', 8000))
    strategy_name = os.getenv('LB_STRATEGY', 'weighted')
    
    try:
        strategy = LoadBalancingStrategy(strategy_name)
    except ValueError:
        logger.warning(f"Invalid strategy '{strategy_name}', using weighted")
        strategy = LoadBalancingStrategy.WEIGHTED
    
    # Auto-scaling configuration
    auto_scaling = AutoScalingConfig(
        enabled=os.getenv('AUTO_SCALING_ENABLED', 'true').lower() == 'true',
        scale_up_threshold=float(os.getenv('SCALE_UP_THRESHOLD', 0.8)),
        scale_down_threshold=float(os.getenv('SCALE_DOWN_THRESHOLD', 0.3)),
        min_instances=int(os.getenv('MIN_INSTANCES', 1)),
        max_instances=int(os.getenv('MAX_INSTANCES', 10)),
        health_check_interval=int(os.getenv('HEALTH_CHECK_INTERVAL', 30))
    )
    
    # Create and run load balancer
    load_balancer = LoadBalancerService(
        port=port,
        strategy=strategy,
        auto_scaling_config=auto_scaling
    )
    
    await load_balancer.run()


if __name__ == '__main__':
    asyncio.run(main())