#!/usr/bin/env python3
"""
Service Discovery for Distributed DTESN Components

Implements service registration, discovery, and health monitoring for
distributed Deep Tree Echo system components.

Features:
- Dynamic service registration and deregistration  
- Health checking with configurable intervals
- Service endpoint resolution
- Load balancing support integration
- Redis-backed service registry with fallback to in-memory
"""

import asyncio
import time
import logging
import json
import hashlib
from typing import Dict, List, Any, Optional, Set, Callable
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from enum import Enum

try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None

try:
    from aiohttp import ClientSession, ClientError
    HTTP_AVAILABLE = True
except ImportError:
    HTTP_AVAILABLE = False
    ClientSession = None

logger = logging.getLogger(__name__)


class ServiceStatus(Enum):
    """Service health status"""
    HEALTHY = "healthy"
    UNHEALTHY = "unhealthy"
    UNKNOWN = "unknown"
    STARTING = "starting"
    STOPPING = "stopping"


class ServiceType(Enum):
    """Types of services in DTESN ecosystem"""
    DTESN_MEMBRANE = "dtesn_membrane"
    COGNITIVE_SERVICE = "cognitive_service"
    CACHE_SERVICE = "cache_service"
    LOAD_BALANCER = "load_balancer"
    API_GATEWAY = "api_gateway"
    MONITORING = "monitoring"
    AGENT_ARENA = "agent_arena"


@dataclass
class ServiceEndpoint:
    """Service endpoint configuration"""
    service_id: str
    service_type: ServiceType
    host: str
    port: int
    protocol: str = "http"
    path: str = "/"
    weight: float = 1.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def url(self) -> str:
        """Get full service URL"""
        return f"{self.protocol}://{self.host}:{self.port}{self.path}"
    
    @property
    def address(self) -> str:
        """Get service address"""
        return f"{self.host}:{self.port}"


@dataclass 
class ServiceHealth:
    """Service health information"""
    service_id: str
    status: ServiceStatus
    last_check: float
    response_time_ms: float
    error_count: int = 0
    consecutive_failures: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)


class ServiceDiscovery:
    """
    Service discovery and health monitoring for DTESN components
    """
    
    def __init__(self,
                 redis_url: str = "redis://localhost:6379",
                 health_check_interval: float = 30.0,
                 health_check_timeout: float = 5.0,
                 max_consecutive_failures: int = 3,
                 service_ttl: int = 300):
        self.redis_url = redis_url
        self.health_check_interval = health_check_interval
        self.health_check_timeout = health_check_timeout
        self.max_consecutive_failures = max_consecutive_failures
        self.service_ttl = service_ttl
        
        # Service registry
        self.services: Dict[str, ServiceEndpoint] = {}
        self.service_health: Dict[str, ServiceHealth] = {}
        
        # Redis connection
        self.redis: Optional[aioredis.Redis] = None
        
        # Background tasks
        self.health_check_task: Optional[asyncio.Task] = None
        self.cleanup_task: Optional[asyncio.Task] = None
        
        # Event callbacks
        self.service_up_callbacks: List[Callable] = []
        self.service_down_callbacks: List[Callable] = []
        
        self._initialized = False
        self._lock = asyncio.Lock()

    async def initialize(self) -> None:
        """Initialize service discovery system"""
        if self._initialized:
            return
            
        async with self._lock:
            if self._initialized:
                return
                
            # Connect to Redis if available
            if REDIS_AVAILABLE and aioredis:
                try:
                    self.redis = aioredis.from_url(
                        self.redis_url, 
                        decode_responses=True,
                        retry_on_error=[ConnectionError, OSError]
                    )
                    await self.redis.ping()
                    logger.info("✅ Service discovery connected to Redis")
                except Exception as e:
                    logger.warning(f"Redis connection failed, using in-memory registry: {e}")
                    self.redis = None
            else:
                logger.warning("aioredis not available, using in-memory registry")
            
            # Start background tasks
            self.health_check_task = asyncio.create_task(self._health_check_loop())
            self.cleanup_task = asyncio.create_task(self._cleanup_loop())
            
            self._initialized = True
            logger.info("🚀 Service discovery initialized")

    async def shutdown(self) -> None:
        """Shutdown service discovery system"""
        if not self._initialized:
            return
            
        # Cancel background tasks
        if self.health_check_task:
            self.health_check_task.cancel()
            try:
                await self.health_check_task
            except asyncio.CancelledError:
                pass
                
        if self.cleanup_task:
            self.cleanup_task.cancel()
            try:
                await self.cleanup_task
            except asyncio.CancelledError:
                pass
        
        # Close Redis connection
        if self.redis:
            await self.redis.aclose()
            
        self._initialized = False
        logger.info("🔌 Service discovery shutdown")

    async def register_service(self, endpoint: ServiceEndpoint) -> bool:
        """
        Register a service endpoint
        
        Args:
            endpoint: Service endpoint to register
            
        Returns:
            True if registration successful, False otherwise
        """
        try:
            # Store locally
            self.services[endpoint.service_id] = endpoint
            
            # Initialize health status
            self.service_health[endpoint.service_id] = ServiceHealth(
                service_id=endpoint.service_id,
                status=ServiceStatus.STARTING,
                last_check=time.time(),
                response_time_ms=0.0
            )
            
            # Store in Redis if available
            if self.redis:
                service_data = {
                    **asdict(endpoint),
                    'service_type': endpoint.service_type.value,
                    'registered_at': time.time()
                }
                
                await self.redis.hset(
                    f"services:{endpoint.service_id}",
                    mapping=service_data
                )
                await self.redis.expire(f"services:{endpoint.service_id}", self.service_ttl)
                
                # Add to service type index
                await self.redis.sadd(f"service_types:{endpoint.service_type.value}", endpoint.service_id)
            
            logger.info(f"📝 Registered service: {endpoint.service_id} at {endpoint.address}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to register service {endpoint.service_id}: {e}")
            return False

    async def deregister_service(self, service_id: str) -> bool:
        """
        Deregister a service endpoint
        
        Args:
            service_id: ID of service to deregister
            
        Returns:
            True if deregistration successful, False otherwise
        """
        try:
            # Remove from local registry
            endpoint = self.services.pop(service_id, None)
            self.service_health.pop(service_id, None)
            
            # Remove from Redis if available
            if self.redis and endpoint:
                await self.redis.delete(f"services:{service_id}")
                await self.redis.srem(f"service_types:{endpoint.service_type.value}", service_id)
            
            logger.info(f"🗑️ Deregistered service: {service_id}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to deregister service {service_id}: {e}")
            return False

    async def discover_services(self, service_type: Optional[ServiceType] = None) -> List[ServiceEndpoint]:
        """
        Discover available services
        
        Args:
            service_type: Optional filter by service type
            
        Returns:
            List of available healthy service endpoints
        """
        try:
            services = []
            
            # Get services from Redis if available
            if self.redis:
                try:
                    if service_type:
                        service_ids = await self.redis.smembers(f"service_types:{service_type.value}")
                    else:
                        # Get all service IDs
                        service_keys = await self.redis.keys("services:*")
                        service_ids = [key.split(":", 1)[1] for key in service_keys]
                    
                    for service_id in service_ids:
                        service_data = await self.redis.hgetall(f"services:{service_id}")
                        if service_data:
                            # Convert back to ServiceEndpoint
                            service_data['service_type'] = ServiceType(service_data['service_type'])
                            service_data['metadata'] = json.loads(service_data.get('metadata', '{}'))
                            
                            endpoint = ServiceEndpoint(**{
                                k: v for k, v in service_data.items() 
                                if k in ServiceEndpoint.__annotations__
                            })
                            
                            # Only return healthy services
                            health = self.service_health.get(service_id)
                            if health and health.status == ServiceStatus.HEALTHY:
                                services.append(endpoint)
                                
                except Exception as e:
                    logger.warning(f"Redis discovery failed, using local registry: {e}")
            
            # Fallback to local registry
            if not services:
                for endpoint in self.services.values():
                    if service_type is None or endpoint.service_type == service_type:
                        health = self.service_health.get(endpoint.service_id)
                        if health and health.status == ServiceStatus.HEALTHY:
                            services.append(endpoint)
            
            logger.debug(f"🔍 Discovered {len(services)} services" + 
                        (f" of type {service_type.value}" if service_type else ""))
            return services
            
        except Exception as e:
            logger.error(f"Service discovery failed: {e}")
            return []

    async def get_service(self, service_id: str) -> Optional[ServiceEndpoint]:
        """Get specific service by ID"""
        try:
            # Check local registry first
            if service_id in self.services:
                return self.services[service_id]
            
            # Check Redis if available
            if self.redis:
                service_data = await self.redis.hgetall(f"services:{service_id}")
                if service_data:
                    service_data['service_type'] = ServiceType(service_data['service_type'])
                    service_data['metadata'] = json.loads(service_data.get('metadata', '{}'))
                    
                    return ServiceEndpoint(**{
                        k: v for k, v in service_data.items() 
                        if k in ServiceEndpoint.__annotations__
                    })
            
            return None
            
        except Exception as e:
            logger.error(f"Failed to get service {service_id}: {e}")
            return None

    async def get_service_health(self, service_id: str) -> Optional[ServiceHealth]:
        """Get health status for a service"""
        return self.service_health.get(service_id)

    def add_service_up_callback(self, callback: Callable[[str], None]) -> None:
        """Add callback for when service comes up"""
        self.service_up_callbacks.append(callback)

    def add_service_down_callback(self, callback: Callable[[str], None]) -> None:
        """Add callback for when service goes down"""
        self.service_down_callbacks.append(callback)

    async def _health_check_loop(self) -> None:
        """Background health checking loop"""
        while True:
            try:
                await asyncio.sleep(self.health_check_interval)
                await self._perform_health_checks()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Health check loop error: {e}")

    async def _perform_health_checks(self) -> None:
        """Perform health checks on all registered services"""
        if not HTTP_AVAILABLE:
            logger.warning("HTTP client not available, skipping health checks")
            return
            
        tasks = []
        for service_id in list(self.services.keys()):
            task = asyncio.create_task(self._check_service_health(service_id))
            tasks.append(task)
        
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)

    async def _check_service_health(self, service_id: str) -> None:
        """Check health of a specific service"""
        endpoint = self.services.get(service_id)
        if not endpoint:
            return
            
        health = self.service_health.get(service_id)
        if not health:
            return
            
        try:
            start_time = time.time()
            
            # Construct health check URL
            health_url = f"{endpoint.protocol}://{endpoint.host}:{endpoint.port}/health"
            
            async with ClientSession(timeout=self.health_check_timeout) as session:
                async with session.get(health_url) as response:
                    response_time = (time.time() - start_time) * 1000
                    
                    if response.status == 200:
                        # Service is healthy
                        old_status = health.status
                        health.status = ServiceStatus.HEALTHY
                        health.response_time_ms = response_time
                        health.consecutive_failures = 0
                        health.last_check = time.time()
                        
                        # Trigger service up callback if status changed
                        if old_status != ServiceStatus.HEALTHY:
                            logger.info(f"✅ Service {service_id} is now healthy")
                            for callback in self.service_up_callbacks:
                                try:
                                    if asyncio.iscoroutinefunction(callback):
                                        await callback(service_id)
                                    else:
                                        callback(service_id)
                                except Exception as e:
                                    logger.error(f"Service up callback error: {e}")
                    else:
                        await self._mark_service_unhealthy(service_id, f"HTTP {response.status}")
                        
        except asyncio.TimeoutError:
            await self._mark_service_unhealthy(service_id, "Health check timeout")
        except Exception as e:
            await self._mark_service_unhealthy(service_id, str(e))

    async def _mark_service_unhealthy(self, service_id: str, reason: str) -> None:
        """Mark a service as unhealthy"""
        health = self.service_health.get(service_id)
        if not health:
            return
            
        old_status = health.status
        health.consecutive_failures += 1
        health.error_count += 1
        health.last_check = time.time()
        
        if health.consecutive_failures >= self.max_consecutive_failures:
            health.status = ServiceStatus.UNHEALTHY
            
            if old_status == ServiceStatus.HEALTHY:
                logger.warning(f"❌ Service {service_id} marked unhealthy: {reason}")
                
                # Trigger service down callbacks
                for callback in self.service_down_callbacks:
                    try:
                        if asyncio.iscoroutinefunction(callback):
                            await callback(service_id)
                        else:
                            callback(service_id)
                    except Exception as e:
                        logger.error(f"Service down callback error: {e}")

    async def _cleanup_loop(self) -> None:
        """Background cleanup of expired services"""
        while True:
            try:
                await asyncio.sleep(60)  # Clean up every minute
                await self._cleanup_expired_services()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Cleanup loop error: {e}")

    async def _cleanup_expired_services(self) -> None:
        """Remove expired services from registry"""
        current_time = time.time()
        expired_services = []
        
        for service_id, health in list(self.service_health.items()):
            # Remove services that haven't been checked recently
            if current_time - health.last_check > self.service_ttl:
                expired_services.append(service_id)
        
        for service_id in expired_services:
            logger.info(f"🧹 Cleaning up expired service: {service_id}")
            await self.deregister_service(service_id)