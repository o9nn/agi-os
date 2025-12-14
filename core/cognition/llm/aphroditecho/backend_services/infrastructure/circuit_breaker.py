#!/usr/bin/env python3
"""
Circuit Breaker Pattern Implementation for DTESN Services

Implements circuit breaker pattern for fault tolerance and graceful degradation
in distributed Deep Tree Echo system components.

Features:
- Configurable failure thresholds and timeouts
- Automatic circuit state transitions (Closed -> Open -> Half-Open)
- Fallback mechanisms for degraded service
- Metrics collection and monitoring integration
- Redis-backed state sharing across instances
"""

import asyncio
import time
import logging
from typing import Dict, List, Any, Optional, Callable, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
import json

try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None

logger = logging.getLogger(__name__)


class CircuitState(Enum):
    """Circuit breaker states"""
    CLOSED = "closed"          # Normal operation
    OPEN = "open"              # Failing fast
    HALF_OPEN = "half_open"    # Testing recovery


@dataclass
class CircuitBreakerConfig:
    """Circuit breaker configuration"""
    failure_threshold: int = 5          # Failures before opening circuit
    timeout: float = 60.0               # Seconds to wait before trying half-open
    half_open_max_calls: int = 3        # Max calls to test in half-open state
    success_threshold: int = 2          # Successes needed to close from half-open
    request_timeout: float = 30.0       # Individual request timeout
    
    # Advanced configuration
    slow_call_threshold: float = 5.0    # Seconds to consider a call "slow"
    slow_call_rate_threshold: float = 0.5  # Percentage of slow calls to trigger
    minimum_throughput: int = 10        # Min calls before evaluating rates


@dataclass
class CircuitBreakerMetrics:
    """Circuit breaker metrics"""
    total_calls: int = 0
    successful_calls: int = 0
    failed_calls: int = 0
    slow_calls: int = 0
    consecutive_failures: int = 0
    consecutive_successes: int = 0
    last_failure_time: float = 0.0
    last_success_time: float = 0.0
    state_change_time: float = 0.0
    
    @property
    def failure_rate(self) -> float:
        """Calculate failure rate"""
        if self.total_calls == 0:
            return 0.0
        return self.failed_calls / self.total_calls
    
    @property
    def slow_call_rate(self) -> float:
        """Calculate slow call rate"""
        if self.total_calls == 0:
            return 0.0
        return self.slow_calls / self.total_calls


class CircuitBreakerException(Exception):
    """Exception raised when circuit breaker is open"""
    def __init__(self, circuit_name: str, state: CircuitState):
        self.circuit_name = circuit_name
        self.state = state
        super().__init__(f"Circuit breaker '{circuit_name}' is {state.value}")


class CircuitBreaker:
    """
    Circuit breaker implementation for service fault tolerance
    """
    
    def __init__(self,
                 name: str,
                 config: Optional[CircuitBreakerConfig] = None,
                 redis_url: Optional[str] = None,
                 fallback_function: Optional[Callable] = None):
        self.name = name
        self.config = config or CircuitBreakerConfig()
        self.redis_url = redis_url
        self.fallback_function = fallback_function
        
        # Circuit state
        self.state = CircuitState.CLOSED
        self.metrics = CircuitBreakerMetrics()
        self.metrics.state_change_time = time.time()
        
        # Half-open state tracking
        self.half_open_calls = 0
        self.half_open_successes = 0
        
        # Redis connection for distributed state
        self.redis: Optional[aioredis.Redis] = None
        
        self._lock = asyncio.Lock()

    async def initialize(self) -> None:
        """Initialize circuit breaker with Redis if available"""
        if self.redis_url and REDIS_AVAILABLE and aioredis:
            try:
                self.redis = aioredis.from_url(
                    self.redis_url,
                    decode_responses=True,
                    retry_on_error=[ConnectionError, OSError]
                )
                await self.redis.ping()
                
                # Load existing state from Redis
                await self._load_state_from_redis()
                
                logger.info(f"✅ Circuit breaker '{self.name}' connected to Redis")
            except Exception as e:
                logger.warning(f"Circuit breaker '{self.name}' Redis connection failed: {e}")
                self.redis = None

    async def __aenter__(self):
        """Async context manager entry"""
        await self._check_and_update_state()
        
        if self.state == CircuitState.OPEN:
            raise CircuitBreakerException(self.name, self.state)
        
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit"""
        if exc_type is None:
            # Success
            await self._record_success()
        else:
            # Failure
            await self._record_failure()

    async def call(self, func: Callable, *args, **kwargs) -> Any:
        """
        Execute function through circuit breaker
        
        Args:
            func: Function to execute
            *args, **kwargs: Arguments for the function
            
        Returns:
            Function result
            
        Raises:
            CircuitBreakerException: If circuit is open
        """
        start_time = time.time()
        
        async with self._lock:
            await self._check_and_update_state()
            
            if self.state == CircuitState.OPEN:
                # Try fallback if available
                if self.fallback_function:
                    logger.info(f"🔄 Circuit breaker '{self.name}' using fallback")
                    try:
                        if asyncio.iscoroutinefunction(self.fallback_function):
                            return await self.fallback_function(*args, **kwargs)
                        else:
                            return self.fallback_function(*args, **kwargs)
                    except Exception as e:
                        logger.error(f"Fallback function failed: {e}")
                
                raise CircuitBreakerException(self.name, self.state)
        
        try:
            # Execute the function
            if asyncio.iscoroutinefunction(func):
                result = await asyncio.wait_for(
                    func(*args, **kwargs), 
                    timeout=self.config.request_timeout
                )
            else:
                result = func(*args, **kwargs)
            
            # Record success
            execution_time = time.time() - start_time
            await self._record_success(execution_time)
            
            return result
            
        except asyncio.TimeoutError:
            await self._record_failure()
            raise
        except Exception as e:
            await self._record_failure()
            raise

    async def _check_and_update_state(self) -> None:
        """Check and update circuit breaker state"""
        current_time = time.time()
        
        if self.state == CircuitState.OPEN:
            # Check if we should move to half-open
            if current_time - self.metrics.state_change_time >= self.config.timeout:
                await self._transition_to_half_open()
                
        elif self.state == CircuitState.HALF_OPEN:
            # Check if we've exceeded half-open call limit
            if self.half_open_calls >= self.config.half_open_max_calls:
                if self.half_open_successes >= self.config.success_threshold:
                    await self._transition_to_closed()
                else:
                    await self._transition_to_open()

    async def _record_success(self, execution_time: float = 0.0) -> None:
        """Record a successful call"""
        async with self._lock:
            self.metrics.total_calls += 1
            self.metrics.successful_calls += 1
            self.metrics.consecutive_successes += 1
            self.metrics.consecutive_failures = 0
            self.metrics.last_success_time = time.time()
            
            # Check if call was slow
            if execution_time > self.config.slow_call_threshold:
                self.metrics.slow_calls += 1
            
            # Update half-open state tracking
            if self.state == CircuitState.HALF_OPEN:
                self.half_open_calls += 1
                self.half_open_successes += 1
            
            await self._save_state_to_redis()

    async def _record_failure(self) -> None:
        """Record a failed call"""
        async with self._lock:
            self.metrics.total_calls += 1
            self.metrics.failed_calls += 1
            self.metrics.consecutive_failures += 1
            self.metrics.consecutive_successes = 0
            self.metrics.last_failure_time = time.time()
            
            # Update half-open state tracking
            if self.state == CircuitState.HALF_OPEN:
                self.half_open_calls += 1
            
            # Check if we should open the circuit
            if self.state == CircuitState.CLOSED:
                if (self.metrics.consecutive_failures >= self.config.failure_threshold or
                    (self.metrics.total_calls >= self.config.minimum_throughput and
                     (self.metrics.failure_rate >= 0.5 or 
                      self.metrics.slow_call_rate >= self.config.slow_call_rate_threshold))):
                    await self._transition_to_open()
            
            await self._save_state_to_redis()

    async def _transition_to_open(self) -> None:
        """Transition circuit to open state"""
        old_state = self.state
        self.state = CircuitState.OPEN
        self.metrics.state_change_time = time.time()
        
        logger.warning(f"🔴 Circuit breaker '{self.name}' opened "
                      f"(failures: {self.metrics.consecutive_failures})")
        
        await self._save_state_to_redis()

    async def _transition_to_half_open(self) -> None:
        """Transition circuit to half-open state"""
        self.state = CircuitState.HALF_OPEN
        self.metrics.state_change_time = time.time()
        self.half_open_calls = 0
        self.half_open_successes = 0
        
        logger.info(f"🟡 Circuit breaker '{self.name}' half-open (testing recovery)")
        
        await self._save_state_to_redis()

    async def _transition_to_closed(self) -> None:
        """Transition circuit to closed state"""
        self.state = CircuitState.CLOSED
        self.metrics.state_change_time = time.time()
        self.metrics.consecutive_failures = 0
        
        logger.info(f"🟢 Circuit breaker '{self.name}' closed (recovered)")
        
        await self._save_state_to_redis()

    async def _save_state_to_redis(self) -> None:
        """Save circuit breaker state to Redis"""
        if not self.redis:
            return
            
        try:
            state_data = {
                'state': self.state.value,
                'metrics': json.dumps({
                    'total_calls': self.metrics.total_calls,
                    'successful_calls': self.metrics.successful_calls,
                    'failed_calls': self.metrics.failed_calls,
                    'slow_calls': self.metrics.slow_calls,
                    'consecutive_failures': self.metrics.consecutive_failures,
                    'consecutive_successes': self.metrics.consecutive_successes,
                    'last_failure_time': self.metrics.last_failure_time,
                    'last_success_time': self.metrics.last_success_time,
                    'state_change_time': self.metrics.state_change_time,
                }),
                'half_open_calls': self.half_open_calls,
                'half_open_successes': self.half_open_successes,
                'updated_at': time.time()
            }
            
            await self.redis.hset(f"circuit_breaker:{self.name}", mapping=state_data)
            await self.redis.expire(f"circuit_breaker:{self.name}", 86400)  # 24 hours
            
        except Exception as e:
            logger.error(f"Failed to save circuit breaker state to Redis: {e}")

    async def _load_state_from_redis(self) -> None:
        """Load circuit breaker state from Redis"""
        if not self.redis:
            return
            
        try:
            state_data = await self.redis.hgetall(f"circuit_breaker:{self.name}")
            if not state_data:
                return
                
            # Load state
            self.state = CircuitState(state_data.get('state', CircuitState.CLOSED.value))
            
            # Load metrics
            if 'metrics' in state_data:
                metrics_data = json.loads(state_data['metrics'])
                self.metrics = CircuitBreakerMetrics(**metrics_data)
            
            # Load half-open state
            self.half_open_calls = int(state_data.get('half_open_calls', 0))
            self.half_open_successes = int(state_data.get('half_open_successes', 0))
            
            logger.info(f"📥 Loaded circuit breaker '{self.name}' state: {self.state.value}")
            
        except Exception as e:
            logger.error(f"Failed to load circuit breaker state from Redis: {e}")

    def get_metrics(self) -> Dict[str, Any]:
        """Get current circuit breaker metrics"""
        return {
            'name': self.name,
            'state': self.state.value,
            'total_calls': self.metrics.total_calls,
            'successful_calls': self.metrics.successful_calls,
            'failed_calls': self.metrics.failed_calls,
            'slow_calls': self.metrics.slow_calls,
            'failure_rate': self.metrics.failure_rate,
            'slow_call_rate': self.metrics.slow_call_rate,
            'consecutive_failures': self.metrics.consecutive_failures,
            'consecutive_successes': self.metrics.consecutive_successes,
            'last_failure_time': self.metrics.last_failure_time,
            'last_success_time': self.metrics.last_success_time,
            'state_change_time': self.metrics.state_change_time,
            'half_open_calls': self.half_open_calls,
            'half_open_successes': self.half_open_successes
        }

    async def reset(self) -> None:
        """Reset circuit breaker to closed state"""
        async with self._lock:
            self.state = CircuitState.CLOSED
            self.metrics = CircuitBreakerMetrics()
            self.metrics.state_change_time = time.time()
            self.half_open_calls = 0
            self.half_open_successes = 0
            
            await self._save_state_to_redis()
            
            logger.info(f"🔄 Circuit breaker '{self.name}' reset")


class CircuitBreakerRegistry:
    """
    Registry for managing multiple circuit breakers
    """
    
    def __init__(self, redis_url: Optional[str] = None):
        self.redis_url = redis_url
        self.circuit_breakers: Dict[str, CircuitBreaker] = {}
        self._lock = asyncio.Lock()

    async def get_or_create_circuit_breaker(self,
                                           name: str,
                                           config: Optional[CircuitBreakerConfig] = None,
                                           fallback_function: Optional[Callable] = None) -> CircuitBreaker:
        """Get existing or create new circuit breaker"""
        async with self._lock:
            if name not in self.circuit_breakers:
                circuit_breaker = CircuitBreaker(
                    name=name,
                    config=config or CircuitBreakerConfig(),
                    redis_url=self.redis_url,
                    fallback_function=fallback_function
                )
                await circuit_breaker.initialize()
                self.circuit_breakers[name] = circuit_breaker
            
            return self.circuit_breakers[name]

    def get_all_metrics(self) -> Dict[str, Dict[str, Any]]:
        """Get metrics for all circuit breakers"""
        return {
            name: cb.get_metrics() 
            for name, cb in self.circuit_breakers.items()
        }

    async def reset_all(self) -> None:
        """Reset all circuit breakers"""
        for circuit_breaker in self.circuit_breakers.values():
            await circuit_breaker.reset()


# Global registry instance
_global_registry: Optional[CircuitBreakerRegistry] = None


async def get_circuit_breaker(name: str,
                             config: Optional[CircuitBreakerConfig] = None,
                             fallback_function: Optional[Callable] = None,
                             redis_url: Optional[str] = None) -> CircuitBreaker:
    """Get or create a circuit breaker from global registry"""
    global _global_registry
    
    if _global_registry is None:
        _global_registry = CircuitBreakerRegistry(redis_url=redis_url)
    
    return await _global_registry.get_or_create_circuit_breaker(
        name=name,
        config=config,
        fallback_function=fallback_function
    )


def circuit_breaker(name: str,
                   config: Optional[CircuitBreakerConfig] = None,
                   fallback_function: Optional[Callable] = None,
                   redis_url: Optional[str] = None):
    """
    Decorator for applying circuit breaker pattern to functions
    
    Usage:
        @circuit_breaker("external_service")
        async def call_external_service():
            # Service call implementation
            pass
    """
    def decorator(func: Callable):
        async def async_wrapper(*args, **kwargs):
            cb = await get_circuit_breaker(
                name=name,
                config=config,
                fallback_function=fallback_function,
                redis_url=redis_url
            )
            return await cb.call(func, *args, **kwargs)
        
        def sync_wrapper(*args, **kwargs):
            # For synchronous functions, we need to run in event loop
            loop = asyncio.get_event_loop()
            if loop.is_running():
                # If we're already in an async context, create a task
                return asyncio.create_task(async_wrapper(*args, **kwargs))
            else:
                # Run in new event loop
                return loop.run_until_complete(async_wrapper(*args, **kwargs))
        
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        else:
            return sync_wrapper
    
    return decorator