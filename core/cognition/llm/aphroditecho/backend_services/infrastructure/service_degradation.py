import asyncio
import time
import logging
from typing import Dict, List, Any, Optional, Callable, Union, Set
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
import json
logger = logging.getLogger(__name__)
class DegradationLevel(Enum):
    NORMAL = 'normal'
    PARTIAL = 'partial'
    MINIMAL = 'minimal'
    EMERGENCY = 'emergency'
    OFFLINE = 'offline'
class FeaturePriority(Enum):
    CRITICAL = 'critical'
    HIGH = 'high'
    MEDIUM = 'medium'
    LOW = 'low'
    OPTIONAL = 'optional'
class ResourceType(Enum):
    CPU = 'cpu'
    MEMORY = 'memory'
    DISK = 'disk'
    NETWORK = 'network'
    DATABASE = 'database'
    CACHE = 'cache'
    EXTERNAL_API = 'external_api'
@dataclass
class ResourceThreshold:
    resource_type: ResourceType
    normal_threshold: float = 0.7
    partial_threshold: float = 0.8
    minimal_threshold: float = 0.9
    emergency_threshold: float = 0.95
@dataclass
class Feature:
    name: str
    priority: FeaturePriority
    enabled: bool = True
    fallback_function: Optional[Callable] = None
    resource_dependencies: List[ResourceType] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class DegradationEvent:
    timestamp: float
    previous_level: DegradationLevel
    new_level: DegradationLevel
    trigger_resource: Optional[ResourceType]
    trigger_value: Optional[float]
    disabled_features: List[str]
    reason: str
class ServiceDegradationManager:
    def __init__(self, service_name: str, resource_thresholds: Optional[List[ResourceThreshold]]=None, check_interval: float=30.0, recovery_delay: float=60.0):
        self.service_name = service_name
        self.resource_thresholds = resource_thresholds or self._default_thresholds()
        self.check_interval = check_interval
        self.recovery_delay = recovery_delay
        self.current_level = DegradationLevel.NORMAL
        self.features: Dict[str, Feature] = {}
        self.degradation_history: List[DegradationEvent] = []
        self.resource_monitors: Dict[ResourceType, Callable] = {}
        self.current_resources: Dict[ResourceType, float] = {}
        self.monitoring_task: Optional[asyncio.Task] = None
        self.degradation_callbacks: List[Callable] = []
        self.recovery_callbacks: List[Callable] = []
        self._initialized = False
        self._lock = asyncio.Lock()
    def _default_thresholds(self) -> List[ResourceThreshold]:
        return [ResourceThreshold(ResourceType.CPU, 0.7, 0.8, 0.9, 0.95), ResourceThreshold(ResourceType.MEMORY, 0.75, 0.85, 0.9, 0.95), ResourceThreshold(ResourceType.DISK, 0.8, 0.85, 0.9, 0.95), ResourceThreshold(ResourceType.NETWORK, 0.7, 0.8, 0.9, 0.95)]
    async def initialize(self) -> None:
        if self._initialized:
            return
        async with self._lock:
            if self._initialized:
                return
            self.monitoring_task = asyncio.create_task(self._monitoring_loop())
            self._initialized = True
            logger.info(f'🚀 Service degradation manager initialized for {self.service_name}')
    async def shutdown(self) -> None:
        if not self._initialized:
            return
        if self.monitoring_task:
            self.monitoring_task.cancel()
            try:
                await self.monitoring_task
            except asyncio.CancelledError:
                pass
        self._initialized = False
        logger.info(f'🔌 Service degradation manager shutdown for {self.service_name}')
    def register_feature(self, feature: Feature) -> None:
        self.features[feature.name] = feature
        logger.debug(f'📝 Registered feature: {feature.name} (priority: {feature.priority.value})')
    def register_resource_monitor(self, resource_type: ResourceType, monitor_func: Callable) -> None:
        self.resource_monitors[resource_type] = monitor_func
        logger.debug(f'📊 Registered resource monitor: {resource_type.value}')
    def add_degradation_callback(self, callback: Callable[[DegradationLevel], None]) -> None:
        self.degradation_callbacks.append(callback)
    def add_recovery_callback(self, callback: Callable[[DegradationLevel], None]) -> None:
        self.recovery_callbacks.append(callback)
    async def is_feature_enabled(self, feature_name: str) -> bool:
        feature = self.features.get(feature_name)
        if not feature:
            return False
        return feature.enabled
    async def execute_with_fallback(self, feature_name: str, func: Callable, *args, **kwargs) -> Any:
        feature = self.features.get(feature_name)
        if not feature or not feature.enabled:
            if feature and feature.fallback_function:
                logger.info(f'🔄 Using fallback for disabled feature: {feature_name}')
                if asyncio.iscoroutinefunction(feature.fallback_function):
                    return await feature.fallback_function(*args, **kwargs)
                else:
                    return feature.fallback_function(*args, **kwargs)
            else:
                raise RuntimeError(f"Feature '{feature_name}' is disabled and no fallback available")
        if asyncio.iscoroutinefunction(func):
            return await func(*args, **kwargs)
        else:
            return func(*args, **kwargs)
    async def force_degradation(self, level: DegradationLevel, reason: str='Manual trigger') -> None:
        if level == self.current_level:
            return
        previous_level = self.current_level
        await self._apply_degradation_level(level, reason=reason)
        event = DegradationEvent(timestamp=time.time(), previous_level=previous_level, new_level=level, trigger_resource=None, trigger_value=None, disabled_features=self._get_disabled_features(), reason=reason)
        self.degradation_history.append(event)
        logger.warning(f'🔴 Forced degradation: {self.service_name} -> {level.value} ({reason})')
    async def _monitoring_loop(self) -> None:
        while True:
            try:
                await asyncio.sleep(self.check_interval)
                await self._check_resources_and_adjust()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Monitoring loop error: {e}')
    async def _check_resources_and_adjust(self) -> None:
        for resource_type, monitor_func in self.resource_monitors.items():
            try:
                if asyncio.iscoroutinefunction(monitor_func):
                    usage = await monitor_func()
                else:
                    usage = monitor_func()
                self.current_resources[resource_type] = usage
            except Exception as e:
                logger.error(f'Resource monitoring error for {resource_type.value}: {e}')
        required_level = await self._calculate_required_degradation_level()
        if required_level != self.current_level:
            await self._handle_level_change(required_level)
    async def _calculate_required_degradation_level(self) -> DegradationLevel:
        highest_level = DegradationLevel.NORMAL
        for threshold in self.resource_thresholds:
            usage = self.current_resources.get(threshold.resource_type, 0.0)
            if usage >= threshold.emergency_threshold:
                return DegradationLevel.OFFLINE
            elif usage >= threshold.minimal_threshold:
                highest_level = max(highest_level, DegradationLevel.EMERGENCY, key=lambda x: list(DegradationLevel).index(x))
            elif usage >= threshold.partial_threshold:
                highest_level = max(highest_level, DegradationLevel.MINIMAL, key=lambda x: list(DegradationLevel).index(x))
            elif usage >= threshold.normal_threshold:
                highest_level = max(highest_level, DegradationLevel.PARTIAL, key=lambda x: list(DegradationLevel).index(x))
        return highest_level
    async def _handle_level_change(self, new_level: DegradationLevel) -> None:
        previous_level = self.current_level
        if list(DegradationLevel).index(new_level) < list(DegradationLevel).index(previous_level):
            await self._check_recovery_stability(new_level)
            return
        await self._apply_degradation_level(new_level)
        trigger_resource, trigger_value = self._find_triggering_resource()
        event = DegradationEvent(timestamp=time.time(), previous_level=previous_level, new_level=new_level, trigger_resource=trigger_resource, trigger_value=trigger_value, disabled_features=self._get_disabled_features(), reason=f"Resource threshold exceeded: {(trigger_resource.value if trigger_resource else 'unknown')}")
        self.degradation_history.append(event)
    async def _check_recovery_stability(self, new_level: DegradationLevel) -> None:
        logger.info(f'⏳ Checking recovery stability for {self.service_name} -> {new_level.value}')
        await asyncio.sleep(self.recovery_delay)
        await self._check_resources_and_adjust()
        current_required = await self._calculate_required_degradation_level()
        if list(DegradationLevel).index(current_required) <= list(DegradationLevel).index(new_level):
            await self._apply_degradation_level(new_level, is_recovery=True)
            event = DegradationEvent(timestamp=time.time(), previous_level=self.current_level, new_level=new_level, trigger_resource=None, trigger_value=None, disabled_features=self._get_disabled_features(), reason='Resource conditions improved')
            self.degradation_history.append(event)
    async def _apply_degradation_level(self, level: DegradationLevel, is_recovery: bool=False, reason: str='') -> None:
        previous_level = self.current_level
        self.current_level = level
        await self._update_feature_states(level)
        if is_recovery:
            logger.info(f'✅ Service recovery: {self.service_name} -> {level.value}')
            for callback in self.recovery_callbacks:
                try:
                    if asyncio.iscoroutinefunction(callback):
                        await callback(level)
                    else:
                        callback(level)
                except Exception as e:
                    logger.error(f'Recovery callback error: {e}')
        else:
            logger.warning(f'⚠️ Service degradation: {self.service_name} -> {level.value}')
            for callback in self.degradation_callbacks:
                try:
                    if asyncio.iscoroutinefunction(callback):
                        await callback(level)
                    else:
                        callback(level)
                except Exception as e:
                    logger.error(f'Degradation callback error: {e}')
    async def _update_feature_states(self, level: DegradationLevel) -> None:
        for feature in self.features.values():
            should_be_enabled = self._should_feature_be_enabled(feature, level)
            if feature.enabled != should_be_enabled:
                feature.enabled = should_be_enabled
                status = 'enabled' if should_be_enabled else 'disabled'
                logger.info(f"🔧 Feature '{feature.name}' {status} due to {level.value} mode")
    def _should_feature_be_enabled(self, feature: Feature, level: DegradationLevel) -> bool:
        if level == DegradationLevel.NORMAL:
            return True
        elif level == DegradationLevel.PARTIAL:
            return feature.priority.value not in ['optional']
        elif level == DegradationLevel.MINIMAL:
            return feature.priority.value in ['critical', 'high']
        elif level == DegradationLevel.EMERGENCY:
            return feature.priority.value == 'critical'
        elif level == DegradationLevel.OFFLINE:
            return False
        return True
    def _find_triggering_resource(self) -> tuple[Optional[ResourceType], Optional[float]]:
        highest_usage = 0.0
        triggering_resource = None
        for resource_type, usage in self.current_resources.items():
            if usage > highest_usage:
                highest_usage = usage
                triggering_resource = resource_type
        return (triggering_resource, highest_usage)
    def _get_disabled_features(self) -> List[str]:
        return [name for name, feature in self.features.items() if not feature.enabled]
    def get_status(self) -> Dict[str, Any]:
        return {'service_name': self.service_name, 'current_level': self.current_level.value, 'enabled_features': [name for name, feature in self.features.items() if feature.enabled], 'disabled_features': self._get_disabled_features(), 'current_resources': self.current_resources, 'degradation_events_count': len(self.degradation_history), 'last_event_time': self.degradation_history[-1].timestamp if self.degradation_history else None}
    def get_degradation_history(self, limit: Optional[int]=None) -> List[Dict[str, Any]]:
        events = self.degradation_history[-limit:] if limit else self.degradation_history
        return [{'timestamp': event.timestamp, 'previous_level': event.previous_level.value, 'new_level': event.new_level.value, 'trigger_resource': event.trigger_resource.value if event.trigger_resource else None, 'trigger_value': event.trigger_value, 'disabled_features': event.disabled_features, 'reason': event.reason} for event in events]
class GlobalDegradationManager:
    def __init__(self):
        self.service_managers: Dict[str, ServiceDegradationManager] = {}
        self._lock = asyncio.Lock()
    async def register_service(self, manager: ServiceDegradationManager) -> None:
        async with self._lock:
            self.service_managers[manager.service_name] = manager
            logger.info(f'📝 Registered service degradation manager: {manager.service_name}')
    async def get_global_status(self) -> Dict[str, Any]:
        services_status = {}
        overall_level = DegradationLevel.NORMAL
        for name, manager in self.service_managers.items():
            status = manager.get_status()
            services_status[name] = status
            service_level = DegradationLevel(status['current_level'])
            if list(DegradationLevel).index(service_level) > list(DegradationLevel).index(overall_level):
                overall_level = service_level
        return {'overall_level': overall_level.value, 'services': services_status, 'total_services': len(self.service_managers), 'degraded_services': len([s for s in services_status.values() if s['current_level'] != DegradationLevel.NORMAL.value])}
    async def emergency_shutdown_all(self, reason: str='Emergency shutdown') -> None:
        logger.critical(f'🚨 Emergency shutdown triggered: {reason}')
        for manager in self.service_managers.values():
            await manager.force_degradation(DegradationLevel.OFFLINE, reason)
_global_manager: Optional[GlobalDegradationManager] = None
def get_global_degradation_manager() -> GlobalDegradationManager:
    global _global_manager
    if _global_manager is None:
        _global_manager = GlobalDegradationManager()
    return _global_manager