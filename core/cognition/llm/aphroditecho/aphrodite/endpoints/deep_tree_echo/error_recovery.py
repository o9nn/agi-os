"""
Error recovery mechanisms for Deep Tree Echo operations.

Implements retry logic, fallback strategies, and graceful degradation
to maintain service availability and achieve 99.9% uptime.
"""

import asyncio
import logging
import random
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Tuple, Union
from dataclasses import dataclass, field
from enum import Enum

from .errors import (
    DTESNError, DTESNProcessingError, DTESNResourceError, DTESNEngineError,
    ErrorSeverity, RecoveryStrategy, ErrorContext, error_aggregator
)


logger = logging.getLogger(__name__)


class FallbackMode(Enum):
    """Available fallback processing modes."""
    SIMPLIFIED = "simplified"
    CACHED = "cached"
    STATISTICAL = "statistical"
    MINIMAL = "minimal"
    OFFLINE = "offline"


@dataclass
class RetryConfig:
    """Configuration for retry mechanisms."""
    max_attempts: int = 3
    base_delay: float = 1.0
    max_delay: float = 60.0
    exponential_base: float = 2.0
    jitter: bool = True
    backoff_factor: float = 1.5


@dataclass
class RecoveryResult:
    """Result of error recovery attempt."""
    success: bool
    result: Any = None
    error: Optional[DTESNError] = None
    attempts_made: int = 0
    recovery_mode: Optional[str] = None
    degraded: bool = False
    fallback_used: bool = False
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for logging and responses."""
        return {
            "success": self.success,
            "attempts_made": self.attempts_made,
            "recovery_mode": self.recovery_mode,
            "degraded": self.degraded,
            "fallback_used": self.fallback_used,
            "error_info": self.error.to_dict() if self.error else None
        }


class RetryManager:
    """Manages retry logic with exponential backoff and jitter."""
    
    def __init__(self, config: RetryConfig = None):
        self.config = config or RetryConfig()
    
    async def retry_async(
        self,
        func: Callable,
        *args,
        context: Optional[ErrorContext] = None,
        retry_on: Tuple[type, ...] = (Exception,),
        **kwargs
    ) -> RecoveryResult:
        """Retry async function with exponential backoff."""
        
        last_error = None
        attempt = 0
        
        while attempt < self.config.max_attempts:
            try:
                attempt += 1
                
                if context:
                    context.retry_count = attempt - 1
                
                logger.info(f"Retry attempt {attempt}/{self.config.max_attempts} for {func.__name__}")
                
                result = await func(*args, **kwargs)
                
                return RecoveryResult(
                    success=True,
                    result=result,
                    attempts_made=attempt,
                    recovery_mode="retry_success"
                )
                
            except Exception as e:
                last_error = e
                
                if not isinstance(e, retry_on):
                    # Non-retryable error
                    break
                
                if attempt < self.config.max_attempts:
                    delay = self._calculate_delay(attempt)
                    logger.warning(f"Retry attempt {attempt} failed: {e}. Retrying in {delay:.2f}s")
                    await asyncio.sleep(delay)
                else:
                    logger.error(f"All {self.config.max_attempts} retry attempts failed")
        
        # All retries failed
        dtesn_error = last_error if isinstance(last_error, DTESNError) else DTESNProcessingError(
            f"Operation failed after {attempt} attempts: {last_error}",
            context=context,
            original_error=last_error
        )
        
        return RecoveryResult(
            success=False,
            error=dtesn_error,
            attempts_made=attempt,
            recovery_mode="retry_failed"
        )
    
    def _calculate_delay(self, attempt: int) -> float:
        """Calculate delay for next retry attempt."""
        delay = min(
            self.config.base_delay * (self.config.exponential_base ** (attempt - 1)),
            self.config.max_delay
        )
        
        if self.config.jitter:
            # Add jitter to prevent thundering herd
            jitter = delay * 0.1 * random.random()
            delay += jitter
        
        return delay


class FallbackProcessor:
    """Implements fallback processing strategies."""
    
    def __init__(self):
        self.cache: Dict[str, Any] = {}
        self.cache_ttl: Dict[str, datetime] = {}
        self.statistical_models: Dict[str, Dict[str, Any]] = {}
    
    async def process_with_fallback(
        self,
        input_data: str,
        primary_processor: Callable,
        context: Optional[ErrorContext] = None,
        fallback_mode: FallbackMode = FallbackMode.SIMPLIFIED
    ) -> RecoveryResult:
        """Process input with fallback strategy."""
        
        try:
            # Try primary processing first
            result = await primary_processor(input_data)
            self._update_cache(input_data, result)
            
            return RecoveryResult(
                success=True,
                result=result,
                recovery_mode="primary_success"
            )
            
        except Exception as e:
            logger.warning(f"Primary processor failed: {e}. Using fallback mode: {fallback_mode.value}")
            
            # Try fallback processing
            try:
                fallback_result = await self._process_fallback(input_data, fallback_mode, context)
                
                return RecoveryResult(
                    success=True,
                    result=fallback_result,
                    degraded=True,
                    fallback_used=True,
                    recovery_mode=f"fallback_{fallback_mode.value}"
                )
                
            except Exception as fallback_error:
                dtesn_error = DTESNProcessingError(
                    f"Both primary and fallback processing failed: {e}, {fallback_error}",
                    context=context,
                    original_error=e
                )
                
                return RecoveryResult(
                    success=False,
                    error=dtesn_error,
                    recovery_mode="fallback_failed"
                )
    
    async def _process_fallback(
        self,
        input_data: str,
        mode: FallbackMode,
        context: Optional[ErrorContext] = None
    ) -> Dict[str, Any]:
        """Process using specified fallback mode."""
        
        if mode == FallbackMode.CACHED:
            return await self._cached_processing(input_data)
        elif mode == FallbackMode.SIMPLIFIED:
            return await self._simplified_processing(input_data)
        elif mode == FallbackMode.STATISTICAL:
            return await self._statistical_processing(input_data)
        elif mode == FallbackMode.MINIMAL:
            return await self._minimal_processing(input_data)
        elif mode == FallbackMode.OFFLINE:
            return await self._offline_processing(input_data)
        else:
            raise DTESNProcessingError(f"Unknown fallback mode: {mode}")
    
    async def _cached_processing(self, input_data: str) -> Dict[str, Any]:
        """Use cached results if available."""
        cache_key = self._generate_cache_key(input_data)
        
        if cache_key in self.cache and self._is_cache_valid(cache_key):
            logger.info("Using cached result for fallback processing")
            cached_result = self.cache[cache_key]
            cached_result["cached"] = True
            cached_result["fallback_mode"] = "cached"
            return cached_result
        
        # No valid cache, use simplified processing
        return await self._simplified_processing(input_data)
    
    async def _simplified_processing(self, input_data: str) -> Dict[str, Any]:
        """Simplified DTESN processing with reduced complexity."""
        # Simulate simplified processing with basic analysis
        word_count = len(input_data.split())
        char_count = len(input_data)
        
        # Simple sentiment analysis (mock)
        positive_words = ["good", "great", "excellent", "positive", "happy"]
        negative_words = ["bad", "terrible", "awful", "negative", "sad"]
        
        input_lower = input_data.lower()
        positive_score = sum(1 for word in positive_words if word in input_lower)
        negative_score = sum(1 for word in negative_words if word in input_lower)
        
        return {
            "output": f"Simplified processing of {word_count} words",
            "membrane_layers": 1,  # Reduced complexity
            "processing_time_ms": 10.0,  # Fast processing
            "metadata": {
                "word_count": word_count,
                "char_count": char_count,
                "sentiment_score": positive_score - negative_score,
                "processing_mode": "simplified",
                "fallback_mode": "simplified"
            }
        }
    
    async def _statistical_processing(self, input_data: str) -> Dict[str, Any]:
        """Statistical analysis based processing."""
        # Use statistical models for prediction
        stats = {
            "length": len(input_data),
            "words": len(input_data.split()),
            "uppercase_ratio": sum(1 for c in input_data if c.isupper()) / max(len(input_data), 1),
            "digit_ratio": sum(1 for c in input_data if c.isdigit()) / max(len(input_data), 1),
            "punctuation_ratio": sum(1 for c in input_data if c in ".,!?;:") / max(len(input_data), 1)
        }
        
        # Mock statistical prediction
        complexity_score = (
            stats["length"] * 0.1 +
            stats["words"] * 0.5 +
            stats["uppercase_ratio"] * 10 +
            stats["punctuation_ratio"] * 5
        )
        
        return {
            "output": f"Statistical analysis: complexity score {complexity_score:.2f}",
            "membrane_layers": 2,
            "processing_time_ms": 25.0,
            "metadata": {
                "statistics": stats,
                "complexity_score": complexity_score,
                "processing_mode": "statistical",
                "fallback_mode": "statistical"
            }
        }
    
    async def _minimal_processing(self, input_data: str) -> Dict[str, Any]:
        """Minimal processing with basic echo."""
        return {
            "output": f"Echo: {input_data[:50]}{'...' if len(input_data) > 50 else ''}",
            "membrane_layers": 1,
            "processing_time_ms": 1.0,
            "metadata": {
                "original_length": len(input_data),
                "processing_mode": "minimal",
                "fallback_mode": "minimal"
            }
        }
    
    async def _offline_processing(self, input_data: str) -> Dict[str, Any]:
        """Offline processing without external dependencies."""
        # Basic text analysis without network or heavy computation
        words = input_data.split()
        sentences = input_data.split('.')
        
        return {
            "output": f"Offline analysis of {len(words)} words in {len(sentences)} sentences",
            "membrane_layers": 1,
            "processing_time_ms": 5.0,
            "metadata": {
                "word_count": len(words),
                "sentence_count": len(sentences),
                "average_word_length": sum(len(word) for word in words) / max(len(words), 1),
                "processing_mode": "offline",
                "fallback_mode": "offline"
            }
        }
    
    def _generate_cache_key(self, input_data: str) -> str:
        """Generate cache key for input data."""
        import hashlib
        return hashlib.md5(input_data.encode()).hexdigest()[:16]
    
    def _update_cache(self, input_data: str, result: Any) -> None:
        """Update cache with processing result."""
        cache_key = self._generate_cache_key(input_data)
        self.cache[cache_key] = result
        self.cache_ttl[cache_key] = datetime.now() + timedelta(hours=1)
        
        # Limit cache size
        if len(self.cache) > 1000:
            oldest_key = min(self.cache_ttl.keys(), key=lambda k: self.cache_ttl[k])
            del self.cache[oldest_key]
            del self.cache_ttl[oldest_key]
    
    def _is_cache_valid(self, cache_key: str) -> bool:
        """Check if cached result is still valid."""
        if cache_key not in self.cache_ttl:
            return False
        return datetime.now() < self.cache_ttl[cache_key]


class ResourceManager:
    """Manages system resources and implements load shedding."""
    
    def __init__(self):
        self.max_concurrent_requests = 100
        self.current_requests = 0
        self.request_queue: asyncio.Queue = asyncio.Queue(maxsize=200)
        self.resource_limits = {
            "memory_mb": 1024,
            "cpu_percent": 80,
            "processing_time_ms": 30000
        }
    
    async def acquire_resources(self, context: Optional[ErrorContext] = None) -> bool:
        """Acquire resources for processing."""
        if self.current_requests >= self.max_concurrent_requests:
            logger.warning(f"Resource limit reached: {self.current_requests}/{self.max_concurrent_requests}")
            
            # Try to queue request
            try:
                await asyncio.wait_for(
                    self.request_queue.put(context),
                    timeout=5.0
                )
                return True
            except asyncio.TimeoutError:
                raise DTESNResourceError(
                    "Request queue full - load shedding in effect",
                    resource_type="queue_capacity",
                    context=context
                )
        
        self.current_requests += 1
        return True
    
    def release_resources(self) -> None:
        """Release acquired resources."""
        self.current_requests = max(0, self.current_requests - 1)
        
        # Process queued requests
        if not self.request_queue.empty():
            try:
                self.request_queue.get_nowait()
                self.current_requests += 1
            except asyncio.QueueEmpty:
                pass
    
    def get_resource_status(self) -> Dict[str, Any]:
        """Get current resource utilization status."""
        return {
            "current_requests": self.current_requests,
            "max_requests": self.max_concurrent_requests,
            "queue_size": self.request_queue.qsize(),
            "utilization_percent": (self.current_requests / self.max_concurrent_requests) * 100,
            "available_capacity": self.max_concurrent_requests - self.current_requests,
            "load_shedding_active": self.current_requests >= self.max_concurrent_requests
        }


class ErrorRecoveryService:
    """Comprehensive error recovery service for DTESN operations."""
    
    def __init__(self):
        self.retry_manager = RetryManager()
        self.fallback_processor = FallbackProcessor()
        self.resource_manager = ResourceManager()
        self.recovery_stats = {
            "total_recoveries": 0,
            "successful_recoveries": 0,
            "fallback_activations": 0,
            "circuit_breaks": 0
        }
    
    async def execute_with_recovery(
        self,
        operation: Callable,
        input_data: str,
        context: Optional[ErrorContext] = None,
        enable_retry: bool = True,
        enable_fallback: bool = True,
        fallback_mode: FallbackMode = FallbackMode.SIMPLIFIED
    ) -> RecoveryResult:
        """Execute operation with comprehensive error recovery."""
        
        self.recovery_stats["total_recoveries"] += 1
        
        try:
            # Acquire resources
            await self.resource_manager.acquire_resources(context)
            
            try:
                # Try with retry if enabled
                if enable_retry:
                    result = await self.retry_manager.retry_async(
                        operation,
                        input_data,
                        context=context
                    )
                    
                    if result.success:
                        self.recovery_stats["successful_recoveries"] += 1
                        return result
                
                # If retry failed or disabled, try fallback
                if enable_fallback:
                    self.recovery_stats["fallback_activations"] += 1
                    
                    result = await self.fallback_processor.process_with_fallback(
                        input_data=input_data,
                        primary_processor=operation,
                        context=context,
                        fallback_mode=fallback_mode
                    )
                    
                    if result.success:
                        self.recovery_stats["successful_recoveries"] += 1
                    
                    return result
                
                # No recovery options available
                return RecoveryResult(
                    success=False,
                    error=DTESNProcessingError(
                        "All recovery options exhausted",
                        context=context
                    ),
                    recovery_mode="no_recovery_available"
                )
                
            finally:
                self.resource_manager.release_resources()
                
        except DTESNResourceError as e:
            # Resource acquisition failed
            return RecoveryResult(
                success=False,
                error=e,
                recovery_mode="resource_exhaustion"
            )
        
        except Exception as e:
            # Unexpected error in recovery process
            error = DTESNProcessingError(
                f"Recovery process failed: {e}",
                context=context,
                original_error=e
            )
            
            return RecoveryResult(
                success=False,
                error=error,
                recovery_mode="recovery_system_failure"
            )
    
    def get_recovery_stats(self) -> Dict[str, Any]:
        """Get recovery service statistics."""
        total = max(self.recovery_stats["total_recoveries"], 1)
        
        return {
            **self.recovery_stats,
            "success_rate": self.recovery_stats["successful_recoveries"] / total,
            "fallback_rate": self.recovery_stats["fallback_activations"] / total,
            "resource_status": self.resource_manager.get_resource_status(),
            "system_health": error_aggregator.get_system_health_status()
        }


# Global error recovery service instance
error_recovery_service = ErrorRecoveryService()