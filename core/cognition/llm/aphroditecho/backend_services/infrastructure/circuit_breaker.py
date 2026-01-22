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
    CLOSED = 'closed'
    OPEN = 'open'
    HALF_OPEN = 'half_open'
@dataclass
class CircuitBreakerConfig:
    failure_threshold: int = 5
    timeout: float = 60.0
    half_open_max_calls: int = 3
    success_threshold: int = 2
    request_timeout: float = 30.0
    slow_call_threshold: float = 5.0
    slow_call_rate_threshold: float = 0.5
    minimum_throughput: int = 10
@dataclass
class CircuitBreakerMetrics:
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
        if self.total_calls == 0:
            return 0.0
        return self.failed_calls / self.total_calls
    @property
    def slow_call_rate(self) -> float:
        if self.total_calls == 0:
            return 0.0
        return self.slow_calls / self.total_calls
class CircuitBreakerException(Exception):
    def __init__(self, circuit_name: str, state: CircuitState):
        self.circuit_name = circuit_name
        self.state = state
        super().__init__(f"Circuit breaker '{circuit_name}' is {state.value}")
class CircuitBreaker:
    def __init__(self, name: str, config: Optional[CircuitBreakerConfig]=None, redis_url: Optional[str]=None, fallback_function: Optional[Callable]=None):
        self.name = name
        self.config = config or CircuitBreakerConfig()
        self.redis_url = redis_url
        self.fallback_function = fallback_function
        self.state = CircuitState.CLOSED
        self.metrics = CircuitBreakerMetrics()
        self.metrics.state_change_time = time.time()
        self.half_open_calls = 0
        self.half_open_successes = 0
        self.redis: Optional[aioredis.Redis] = None
        self._lock = asyncio.Lock()
    async def initialize(self) -> None:
        if self.redis_url and REDIS_AVAILABLE and aioredis:
            try:
                self.redis = aioredis.from_url(self.redis_url, decode_responses=True, retry_on_error=[ConnectionError, OSError])
                await self.redis.ping()
                await self._load_state_from_redis()
                logger.info(f"✅ Circuit breaker '{self.name}' connected to Redis")
            except Exception as e:
                logger.warning(f"Circuit breaker '{self.name}' Redis connection failed: {e}")
                self.redis = None
    async def __aenter__(self):
        await self._check_and_update_state()
        if self.state == CircuitState.OPEN:
            raise CircuitBreakerException(self.name, self.state)
        return self
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if exc_type is None:
            await self._record_success()
        else:
            await self._record_failure()
    async def call(self, func: Callable, *args, **kwargs) -> Any:
        start_time = time.time()
        async with self._lock:
            await self._check_and_update_state()
            if self.state == CircuitState.OPEN:
                if self.fallback_function:
                    logger.info(f"🔄 Circuit breaker '{self.name}' using fallback")
                    try:
                        if asyncio.iscoroutinefunction(self.fallback_function):
                            return await self.fallback_function(*args, **kwargs)
                        else:
                            return self.fallback_function(*args, **kwargs)
                    except Exception as e:
                        logger.error(f'Fallback function failed: {e}')
                raise CircuitBreakerException(self.name, self.state)
        try:
            if asyncio.iscoroutinefunction(func):
                result = await asyncio.wait_for(func(*args, **kwargs), timeout=self.config.request_timeout)
            else:
                result = func(*args, **kwargs)
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
        current_time = time.time()
        if self.state == CircuitState.OPEN:
            if current_time - self.metrics.state_change_time >= self.config.timeout:
                await self._transition_to_half_open()
        elif self.state == CircuitState.HALF_OPEN:
            if self.half_open_calls >= self.config.half_open_max_calls:
                if self.half_open_successes >= self.config.success_threshold:
                    await self._transition_to_closed()
                else:
                    await self._transition_to_open()
    async def _record_success(self, execution_time: float=0.0) -> None:
        async with self._lock:
            self.metrics.total_calls += 1
            self.metrics.successful_calls += 1
            self.metrics.consecutive_successes += 1
            self.metrics.consecutive_failures = 0
            self.metrics.last_success_time = time.time()
            if execution_time > self.config.slow_call_threshold:
                self.metrics.slow_calls += 1
            if self.state == CircuitState.HALF_OPEN:
                self.half_open_calls += 1
                self.half_open_successes += 1
            await self._save_state_to_redis()
    async def _record_failure(self) -> None:
        async with self._lock:
            self.metrics.total_calls += 1
            self.metrics.failed_calls += 1
            self.metrics.consecutive_failures += 1
            self.metrics.consecutive_successes = 0
            self.metrics.last_failure_time = time.time()
            if self.state == CircuitState.HALF_OPEN:
                self.half_open_calls += 1
            if self.state == CircuitState.CLOSED:
                if self.metrics.consecutive_failures >= self.config.failure_threshold or (self.metrics.total_calls >= self.config.minimum_throughput and (self.metrics.failure_rate >= 0.5 or self.metrics.slow_call_rate >= self.config.slow_call_rate_threshold)):
                    await self._transition_to_open()
            await self._save_state_to_redis()
    async def _transition_to_open(self) -> None:
        old_state = self.state
        self.state = CircuitState.OPEN
        self.metrics.state_change_time = time.time()
        logger.warning(f"🔴 Circuit breaker '{self.name}' opened (failures: {self.metrics.consecutive_failures})")
        await self._save_state_to_redis()
    async def _transition_to_half_open(self) -> None:
        self.state = CircuitState.HALF_OPEN
        self.metrics.state_change_time = time.time()
        self.half_open_calls = 0
        self.half_open_successes = 0
        logger.info(f"🟡 Circuit breaker '{self.name}' half-open (testing recovery)")
        await self._save_state_to_redis()
    async def _transition_to_closed(self) -> None:
        self.state = CircuitState.CLOSED
        self.metrics.state_change_time = time.time()
        self.metrics.consecutive_failures = 0
        logger.info(f"🟢 Circuit breaker '{self.name}' closed (recovered)")
        await self._save_state_to_redis()
    async def _save_state_to_redis(self) -> None:
        if not self.redis:
            return
        try:
            state_data = {'state': self.state.value, 'metrics': json.dumps({'total_calls': self.metrics.total_calls, 'successful_calls': self.metrics.successful_calls, 'failed_calls': self.metrics.failed_calls, 'slow_calls': self.metrics.slow_calls, 'consecutive_failures': self.metrics.consecutive_failures, 'consecutive_successes': self.metrics.consecutive_successes, 'last_failure_time': self.metrics.last_failure_time, 'last_success_time': self.metrics.last_success_time, 'state_change_time': self.metrics.state_change_time}), 'half_open_calls': self.half_open_calls, 'half_open_successes': self.half_open_successes, 'updated_at': time.time()}
            await self.redis.hset(f'circuit_breaker:{self.name}', mapping=state_data)
            await self.redis.expire(f'circuit_breaker:{self.name}', 86400)
        except Exception as e:
            logger.error(f'Failed to save circuit breaker state to Redis: {e}')
    async def _load_state_from_redis(self) -> None:
        if not self.redis:
            return
        try:
            state_data = await self.redis.hgetall(f'circuit_breaker:{self.name}')
            if not state_data:
                return
            self.state = CircuitState(state_data.get('state', CircuitState.CLOSED.value))
            if 'metrics' in state_data:
                metrics_data = json.loads(state_data['metrics'])
                self.metrics = CircuitBreakerMetrics(**metrics_data)
            self.half_open_calls = int(state_data.get('half_open_calls', 0))
            self.half_open_successes = int(state_data.get('half_open_successes', 0))
            logger.info(f"📥 Loaded circuit breaker '{self.name}' state: {self.state.value}")
        except Exception as e:
            logger.error(f'Failed to load circuit breaker state from Redis: {e}')
    def get_metrics(self) -> Dict[str, Any]:
        return {'name': self.name, 'state': self.state.value, 'total_calls': self.metrics.total_calls, 'successful_calls': self.metrics.successful_calls, 'failed_calls': self.metrics.failed_calls, 'slow_calls': self.metrics.slow_calls, 'failure_rate': self.metrics.failure_rate, 'slow_call_rate': self.metrics.slow_call_rate, 'consecutive_failures': self.metrics.consecutive_failures, 'consecutive_successes': self.metrics.consecutive_successes, 'last_failure_time': self.metrics.last_failure_time, 'last_success_time': self.metrics.last_success_time, 'state_change_time': self.metrics.state_change_time, 'half_open_calls': self.half_open_calls, 'half_open_successes': self.half_open_successes}
    async def reset(self) -> None:
        async with self._lock:
            self.state = CircuitState.CLOSED
            self.metrics = CircuitBreakerMetrics()
            self.metrics.state_change_time = time.time()
            self.half_open_calls = 0
            self.half_open_successes = 0
            await self._save_state_to_redis()
            logger.info(f"🔄 Circuit breaker '{self.name}' reset")
class CircuitBreakerRegistry:
    def __init__(self, redis_url: Optional[str]=None):
        self.redis_url = redis_url
        self.circuit_breakers: Dict[str, CircuitBreaker] = {}
        self._lock = asyncio.Lock()
    async def get_or_create_circuit_breaker(self, name: str, config: Optional[CircuitBreakerConfig]=None, fallback_function: Optional[Callable]=None) -> CircuitBreaker:
        async with self._lock:
            if name not in self.circuit_breakers:
                circuit_breaker = CircuitBreaker(name=name, config=config or CircuitBreakerConfig(), redis_url=self.redis_url, fallback_function=fallback_function)
                await circuit_breaker.initialize()
                self.circuit_breakers[name] = circuit_breaker
            return self.circuit_breakers[name]
    def get_all_metrics(self) -> Dict[str, Dict[str, Any]]:
        return {name: cb.get_metrics() for name, cb in self.circuit_breakers.items()}
    async def reset_all(self) -> None:
        for circuit_breaker in self.circuit_breakers.values():
            await circuit_breaker.reset()
_global_registry: Optional[CircuitBreakerRegistry] = None
async def get_circuit_breaker(name: str, config: Optional[CircuitBreakerConfig]=None, fallback_function: Optional[Callable]=None, redis_url: Optional[str]=None) -> CircuitBreaker:
    global _global_registry
    if _global_registry is None:
        _global_registry = CircuitBreakerRegistry(redis_url=redis_url)
    return await _global_registry.get_or_create_circuit_breaker(name=name, config=config, fallback_function=fallback_function)
def circuit_breaker(name: str, config: Optional[CircuitBreakerConfig]=None, fallback_function: Optional[Callable]=None, redis_url: Optional[str]=None):
    def decorator(func: Callable):
        async def async_wrapper(*args, **kwargs):
            cb = await get_circuit_breaker(name=name, config=config, fallback_function=fallback_function, redis_url=redis_url)
            return await cb.call(func, *args, **kwargs)
        def sync_wrapper(*args, **kwargs):
            loop = asyncio.get_event_loop()
            if loop.is_running():
                return asyncio.create_task(async_wrapper(*args, **kwargs))
            else:
                return loop.run_until_complete(async_wrapper(*args, **kwargs))
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        else:
            return sync_wrapper
    return decorator