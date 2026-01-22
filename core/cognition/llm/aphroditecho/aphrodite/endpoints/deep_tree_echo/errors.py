import enum
import logging
import traceback
from datetime import datetime
from typing import Any, Dict, List, Optional, Union
from dataclasses import dataclass, field
from pydantic import BaseModel
logger = logging.getLogger(__name__)
class ErrorSeverity(enum.Enum):
    LOW = 'low'
    MEDIUM = 'medium'
    HIGH = 'high'
    CRITICAL = 'critical'
class ErrorCategory(enum.Enum):
    VALIDATION = 'validation'
    PROCESSING = 'processing'
    RESOURCE = 'resource'
    NETWORK = 'network'
    CONFIGURATION = 'configuration'
    SYSTEM = 'system'
    DTESN = 'dtesn'
    ENGINE = 'engine'
class RecoveryStrategy(enum.Enum):
    RETRY = 'retry'
    FALLBACK = 'fallback'
    DEGRADE = 'degrade'
    CIRCUIT_BREAK = 'circuit_break'
    ABORT = 'abort'
@dataclass
class ErrorContext:
    request_id: str
    endpoint: str
    user_input: Optional[str] = None
    processing_stage: Optional[str] = None
    resource_state: Optional[Dict[str, Any]] = None
    retry_count: int = 0
    timestamp: datetime = field(default_factory=datetime.now)
    trace_id: Optional[str] = None
    def to_dict(self) -> Dict[str, Any]:
        return {'request_id': self.request_id, 'endpoint': self.endpoint, 'user_input': self.user_input[:100] if self.user_input else None, 'processing_stage': self.processing_stage, 'resource_state': self.resource_state, 'retry_count': self.retry_count, 'timestamp': self.timestamp.isoformat(), 'trace_id': self.trace_id}
class DTESNError(Exception):
    def __init__(self, message: str, category: ErrorCategory=ErrorCategory.SYSTEM, severity: ErrorSeverity=ErrorSeverity.MEDIUM, recovery_strategy: RecoveryStrategy=RecoveryStrategy.RETRY, context: Optional[ErrorContext]=None, original_error: Optional[Exception]=None, error_code: Optional[str]=None):
        super().__init__(message)
        self.message = message
        self.category = category
        self.severity = severity
        self.recovery_strategy = recovery_strategy
        self.context = context or ErrorContext('', '')
        self.original_error = original_error
        self.error_code = error_code or self._generate_error_code()
        self.timestamp = datetime.now()
    def _generate_error_code(self) -> str:
        return f'DTESN_{self.category.value.upper()}_{self.severity.value.upper()}_{int(self.timestamp.timestamp())}'
    def to_dict(self) -> Dict[str, Any]:
        return {'error_code': self.error_code, 'message': self.message, 'category': self.category.value, 'severity': self.severity.value, 'recovery_strategy': self.recovery_strategy.value, 'timestamp': self.timestamp.isoformat(), 'context': self.context.to_dict() if self.context else None, 'original_error': {'type': type(self.original_error).__name__, 'message': str(self.original_error)} if self.original_error else None, 'traceback': traceback.format_exc() if self.original_error else None}
class DTESNValidationError(DTESNError):
    def __init__(self, message: str, field_name: str=None, **kwargs):
        super().__init__(message, category=ErrorCategory.VALIDATION, severity=ErrorSeverity.LOW, recovery_strategy=RecoveryStrategy.ABORT, **kwargs)
        self.field_name = field_name
class DTESNProcessingError(DTESNError):
    def __init__(self, message: str, processing_stage: str=None, **kwargs):
        super().__init__(message, category=ErrorCategory.PROCESSING, severity=ErrorSeverity.MEDIUM, recovery_strategy=RecoveryStrategy.RETRY, **kwargs)
        self.processing_stage = processing_stage
class DTESNResourceError(DTESNError):
    def __init__(self, message: str, resource_type: str=None, **kwargs):
        super().__init__(message, category=ErrorCategory.RESOURCE, severity=ErrorSeverity.HIGH, recovery_strategy=RecoveryStrategy.DEGRADE, **kwargs)
        self.resource_type = resource_type
class DTESNEngineError(DTESNError):
    def __init__(self, message: str, engine_state: str=None, **kwargs):
        super().__init__(message, category=ErrorCategory.ENGINE, severity=ErrorSeverity.HIGH, recovery_strategy=RecoveryStrategy.CIRCUIT_BREAK, **kwargs)
        self.engine_state = engine_state
class DTESNNetworkError(DTESNError):
    def __init__(self, message: str, **kwargs):
        super().__init__(message, category=ErrorCategory.NETWORK, severity=ErrorSeverity.MEDIUM, recovery_strategy=RecoveryStrategy.RETRY, **kwargs)
class DTESNSystemError(DTESNError):
    def __init__(self, message: str, **kwargs):
        super().__init__(message, category=ErrorCategory.SYSTEM, severity=ErrorSeverity.CRITICAL, recovery_strategy=RecoveryStrategy.CIRCUIT_BREAK, **kwargs)
class ErrorResponse(BaseModel):
    error: bool = True
    error_code: str
    message: str
    category: str
    severity: str
    timestamp: str
    request_id: Optional[str] = None
    recovery_suggestions: List[str] = field(default_factory=list)
    context: Optional[Dict[str, Any]] = None
    retry_after: Optional[int] = None
    degraded_mode: bool = False
    server_rendered: bool = True
    class Config:
        use_enum_values = True
class ErrorAggregator:
    def __init__(self):
        self.error_history: List[DTESNError] = []
        self.error_counts: Dict[str, int] = {}
        self.failure_rate_threshold = 0.1
    def record_error(self, error: DTESNError) -> None:
        self.error_history.append(error)
        if len(self.error_history) > 1000:
            self.error_history = self.error_history[-1000:]
        error_key = f'{error.category.value}_{error.severity.value}'
        self.error_counts[error_key] = self.error_counts.get(error_key, 0) + 1
        logger.error(f'DTESN Error recorded: {error.error_code}', extra={'error_data': error.to_dict(), 'system_health': self.get_system_health_status()})
    def get_system_health_status(self) -> Dict[str, Any]:
        recent_errors = [e for e in self.error_history if (datetime.now() - e.timestamp).total_seconds() < 3600]
        total_operations = len(self.error_history) + 1000
        error_rate = len(recent_errors) / max(total_operations, 1)
        critical_errors = [e for e in recent_errors if e.severity == ErrorSeverity.CRITICAL]
        return {'status': 'unhealthy' if error_rate > self.failure_rate_threshold else 'healthy', 'error_rate': error_rate, 'recent_errors_count': len(recent_errors), 'critical_errors_count': len(critical_errors), 'should_circuit_break': error_rate > self.failure_rate_threshold or len(critical_errors) > 0, 'degraded_mode_recommended': error_rate > 0.05}
    def get_recovery_recommendations(self, error_category: ErrorCategory) -> List[str]:
        recommendations = []
        if error_category == ErrorCategory.RESOURCE:
            recommendations.extend(['Reduce batch sizes', 'Enable degraded mode processing', 'Check system resources (CPU, memory)', 'Scale horizontally if possible'])
        elif error_category == ErrorCategory.PROCESSING:
            recommendations.extend(['Retry with exponential backoff', 'Reduce processing complexity', 'Check input data validity', 'Verify DTESN component health'])
        elif error_category == ErrorCategory.ENGINE:
            recommendations.extend(['Check Aphrodite engine status', 'Restart engine connection', 'Use fallback processing mode', 'Verify model availability'])
        elif error_category == ErrorCategory.NETWORK:
            recommendations.extend(['Retry request', 'Check network connectivity', 'Use cached responses if available', 'Enable offline mode processing'])
        return recommendations
error_aggregator = ErrorAggregator()
def create_error_response(error: Union[DTESNError, Exception], context: Optional[ErrorContext]=None, include_debug_info: bool=False) -> ErrorResponse:
    if isinstance(error, DTESNError):
        dtesn_error = error
    else:
        dtesn_error = DTESNError(message=str(error), category=ErrorCategory.SYSTEM, severity=ErrorSeverity.MEDIUM, context=context, original_error=error)
    error_aggregator.record_error(dtesn_error)
    recommendations = error_aggregator.get_recovery_recommendations(dtesn_error.category)
    health_status = error_aggregator.get_system_health_status()
    response = ErrorResponse(error_code=dtesn_error.error_code, message=dtesn_error.message, category=dtesn_error.category.value, severity=dtesn_error.severity.value, timestamp=dtesn_error.timestamp.isoformat(), request_id=dtesn_error.context.request_id if dtesn_error.context else None, recovery_suggestions=recommendations, degraded_mode=health_status.get('degraded_mode_recommended', False), retry_after=_calculate_retry_delay(dtesn_error.severity) if dtesn_error.recovery_strategy == RecoveryStrategy.RETRY else None)
    if include_debug_info and dtesn_error.context:
        response.context = dtesn_error.context.to_dict()
    return response
def _calculate_retry_delay(severity: ErrorSeverity) -> int:
    delays = {ErrorSeverity.LOW: 1, ErrorSeverity.MEDIUM: 5, ErrorSeverity.HIGH: 30, ErrorSeverity.CRITICAL: 300}
    return delays.get(severity, 5)
class CircuitBreaker:
    def __init__(self, failure_threshold: int=5, recovery_timeout: int=60):
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.state = 'CLOSED'
    def call(self, func, *args, **kwargs):
        if self.state == 'OPEN':
            if self._should_attempt_reset():
                self.state = 'HALF_OPEN'
            else:
                raise DTESNSystemError('Circuit breaker is OPEN - service temporarily unavailable', recovery_strategy=RecoveryStrategy.CIRCUIT_BREAK)
        try:
            result = func(*args, **kwargs)
            self._on_success()
            return result
        except Exception as e:
            self._on_failure()
            raise
    def _on_success(self):
        self.failure_count = 0
        self.state = 'CLOSED'
    def _on_failure(self):
        self.failure_count += 1
        self.last_failure_time = datetime.now()
        if self.failure_count >= self.failure_threshold:
            self.state = 'OPEN'
            logger.warning(f'Circuit breaker OPEN - failure threshold ({self.failure_threshold}) exceeded')
    def _should_attempt_reset(self) -> bool:
        if not self.last_failure_time:
            return True
        time_since_failure = (datetime.now() - self.last_failure_time).total_seconds()
        return time_since_failure > self.recovery_timeout
dtesn_circuit_breaker = CircuitBreaker(failure_threshold=3, recovery_timeout=30)
engine_circuit_breaker = CircuitBreaker(failure_threshold=5, recovery_timeout=60)