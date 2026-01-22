import asyncio
import json
import logging
import time
import threading
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Dict, Any, Optional, List, Callable, Union
from enum import Enum
import pydantic
from pydantic import BaseModel, Field, validator
from .config import DTESNConfig
logger = logging.getLogger(__name__)
class ConfigurationEnvironment(str, Enum):
    DEVELOPMENT = 'development'
    STAGING = 'staging'
    PRODUCTION = 'production'
    TESTING = 'testing'
@dataclass
class ConfigurationSnapshot:
    snapshot_id: str
    timestamp: float
    config_data: Dict[str, Any]
    environment: ConfigurationEnvironment
    description: str
    is_active: bool = False
    validation_errors: Optional[List[str]] = None
@dataclass
class ConfigurationUpdateRequest:
    parameter_path: str
    new_value: Any
    description: Optional[str] = None
    validate_only: bool = False
    environment: Optional[ConfigurationEnvironment] = None
class ConfigurationValidator:
    def __init__(self):
        self._validators: Dict[str, Callable[[Any], bool]] = {'max_membrane_depth': lambda x: isinstance(x, int) and 1 <= x <= 64, 'esn_reservoir_size': lambda x: isinstance(x, int) and 64 <= x <= 16384, 'bseries_max_order': lambda x: isinstance(x, int) and 1 <= x <= 32, 'cache_ttl_seconds': lambda x: isinstance(x, int) and 1 <= x <= 86400, 'enable_caching': lambda x: isinstance(x, bool), 'enable_docs': lambda x: isinstance(x, bool), 'enable_performance_monitoring': lambda x: isinstance(x, bool)}
        self._dependencies: Dict[str, List[str]] = {'enable_caching': ['cache_ttl_seconds'], 'max_membrane_depth': ['esn_reservoir_size', 'bseries_max_order']}
    def validate_parameter(self, param_path: str, value: Any) -> List[str]:
        errors = []
        if param_path not in self._validators:
            errors.append(f'Unknown parameter: {param_path}')
            return errors
        if not self._validators[param_path](value):
            errors.append(f'Invalid value for {param_path}: {value}')
        return errors
    def validate_configuration(self, config_dict: Dict[str, Any]) -> List[str]:
        errors = []
        for param_path, value in config_dict.items():
            param_errors = self.validate_parameter(param_path, value)
            errors.extend(param_errors)
        for param, deps in self._dependencies.items():
            if param in config_dict and config_dict[param]:
                for dep in deps:
                    if dep not in config_dict:
                        errors.append(f'Missing dependency {dep} for parameter {param}')
        if 'max_membrane_depth' in config_dict and 'esn_reservoir_size' in config_dict:
            depth = config_dict['max_membrane_depth']
            reservoir = config_dict['esn_reservoir_size']
            if reservoir < depth * 32:
                errors.append(f'ESN reservoir size ({reservoir}) should be at least {depth * 32} for membrane depth {depth}')
        return errors
class DynamicConfigurationManager:
    def __init__(self, initial_config: Optional[DTESNConfig]=None, max_snapshots: int=50, backup_directory: Optional[Path]=None, enable_auto_backup: bool=True):
        self.max_snapshots = max_snapshots
        self.enable_auto_backup = enable_auto_backup
        self.backup_directory = backup_directory or Path('/tmp/dtesn_config_backups')
        self.backup_directory.mkdir(exist_ok=True)
        self._current_config = initial_config or DTESNConfig()
        self._config_lock = threading.RLock()
        self._snapshots: List[ConfigurationSnapshot] = []
        self._snapshot_counter = 0
        self._validator = ConfigurationValidator()
        self._update_callbacks: List[Callable[[DTESNConfig], None]] = []
        self._environment = ConfigurationEnvironment.DEVELOPMENT
        self._create_snapshot('Initial configuration')
        logger.info('Dynamic configuration manager initialized')
    @property
    def current_config(self) -> DTESNConfig:
        with self._config_lock:
            return self._current_config
    @property
    def environment(self) -> ConfigurationEnvironment:
        return self._environment
    def set_environment(self, environment: ConfigurationEnvironment) -> None:
        self._environment = environment
        logger.info(f'Configuration environment set to: {environment}')
    def register_update_callback(self, callback: Callable[[DTESNConfig], None]) -> None:
        self._update_callbacks.append(callback)
    async def update_parameter(self, request: ConfigurationUpdateRequest) -> Dict[str, Any]:
        try:
            with self._config_lock:
                current_dict = self._current_config.dict()
                if '.' in request.parameter_path:
                    return {'success': False, 'error': 'Nested parameter updates not yet supported', 'parameter': request.parameter_path}
                else:
                    if request.parameter_path not in current_dict:
                        return {'success': False, 'error': f'Unknown parameter: {request.parameter_path}', 'parameter': request.parameter_path}
                    new_dict = current_dict.copy()
                    new_dict[request.parameter_path] = request.new_value
                validation_errors = self._validator.validate_configuration(new_dict)
                if request.validate_only:
                    return {'success': len(validation_errors) == 0, 'validation_errors': validation_errors, 'parameter': request.parameter_path, 'validate_only': True}
                if validation_errors:
                    return {'success': False, 'validation_errors': validation_errors, 'parameter': request.parameter_path}
                previous_snapshot_id = self._create_snapshot(f'Before updating {request.parameter_path}')
                try:
                    new_config = DTESNConfig(**new_dict)
                    old_config = self._current_config
                    self._current_config = new_config
                    await self._notify_update_callbacks(new_config)
                    new_snapshot_id = self._create_snapshot(request.description or f'Updated {request.parameter_path} to {request.new_value}')
                    logger.info(f'Configuration parameter {request.parameter_path} updated from {getattr(old_config, request.parameter_path)} to {request.new_value}')
                    return {'success': True, 'parameter': request.parameter_path, 'old_value': getattr(old_config, request.parameter_path), 'new_value': request.new_value, 'snapshot_id': new_snapshot_id, 'rollback_snapshot': previous_snapshot_id}
                except Exception as e:
                    logger.error(f'Failed to apply configuration update: {e}')
                    return {'success': False, 'error': f'Failed to apply update: {str(e)}', 'parameter': request.parameter_path, 'rollback_snapshot': previous_snapshot_id}
        except Exception as e:
            logger.error(f'Configuration update failed: {e}')
            return {'success': False, 'error': f'Update failed: {str(e)}', 'parameter': request.parameter_path}
    async def update_multiple_parameters(self, updates: List[ConfigurationUpdateRequest]) -> Dict[str, Any]:
        try:
            with self._config_lock:
                current_dict = self._current_config.dict()
                new_dict = current_dict.copy()
                applied_updates = []
                for update in updates:
                    if '.' in update.parameter_path:
                        return {'success': False, 'error': 'Nested parameter updates not yet supported', 'failed_parameter': update.parameter_path}
                    if update.parameter_path not in current_dict:
                        return {'success': False, 'error': f'Unknown parameter: {update.parameter_path}', 'failed_parameter': update.parameter_path}
                    new_dict[update.parameter_path] = update.new_value
                    applied_updates.append({'parameter': update.parameter_path, 'old_value': current_dict[update.parameter_path], 'new_value': update.new_value})
                validation_errors = self._validator.validate_configuration(new_dict)
                if validation_errors:
                    return {'success': False, 'validation_errors': validation_errors, 'attempted_updates': applied_updates}
                previous_snapshot_id = self._create_snapshot('Before batch update')
                try:
                    new_config = DTESNConfig(**new_dict)
                    self._current_config = new_config
                    await self._notify_update_callbacks(new_config)
                    new_snapshot_id = self._create_snapshot(f'Batch update of {len(updates)} parameters')
                    logger.info(f'Batch configuration update applied: {len(updates)} parameters')
                    return {'success': True, 'updated_parameters': applied_updates, 'snapshot_id': new_snapshot_id, 'rollback_snapshot': previous_snapshot_id}
                except Exception as e:
                    logger.error(f'Failed to apply batch configuration update: {e}')
                    return {'success': False, 'error': f'Failed to apply batch update: {str(e)}', 'attempted_updates': applied_updates, 'rollback_snapshot': previous_snapshot_id}
        except Exception as e:
            logger.error(f'Batch configuration update failed: {e}')
            return {'success': False, 'error': f'Batch update failed: {str(e)}'}
    async def rollback_to_snapshot(self, snapshot_id: str) -> Dict[str, Any]:
        try:
            with self._config_lock:
                target_snapshot = None
                for snapshot in self._snapshots:
                    if snapshot.snapshot_id == snapshot_id:
                        target_snapshot = snapshot
                        break
                if not target_snapshot:
                    return {'success': False, 'error': f'Snapshot {snapshot_id} not found'}
                pre_rollback_snapshot = self._create_snapshot(f'Before rollback to {snapshot_id}')
                try:
                    old_config = self._current_config
                    rollback_config = DTESNConfig(**target_snapshot.config_data)
                    self._current_config = rollback_config
                    await self._notify_update_callbacks(rollback_config)
                    rollback_snapshot_id = self._create_snapshot(f'Rollback to snapshot {snapshot_id}: {target_snapshot.description}')
                    logger.info(f'Configuration rolled back to snapshot {snapshot_id}')
                    return {'success': True, 'rolled_back_to': snapshot_id, 'rollback_snapshot': rollback_snapshot_id, 'undo_rollback_snapshot': pre_rollback_snapshot}
                except Exception as e:
                    logger.error(f'Failed to rollback configuration: {e}')
                    return {'success': False, 'error': f'Rollback failed: {str(e)}', 'target_snapshot': snapshot_id}
        except Exception as e:
            logger.error(f'Configuration rollback failed: {e}')
            return {'success': False, 'error': f'Rollback failed: {str(e)}'}
    def get_snapshots(self) -> List[Dict[str, Any]]:
        with self._config_lock:
            return [{'snapshot_id': snapshot.snapshot_id, 'timestamp': snapshot.timestamp, 'description': snapshot.description, 'environment': snapshot.environment.value, 'is_active': snapshot.is_active, 'validation_errors': snapshot.validation_errors} for snapshot in self._snapshots]
    def get_current_status(self) -> Dict[str, Any]:
        with self._config_lock:
            return {'current_config': self._current_config.dict(), 'environment': self._environment.value, 'total_snapshots': len(self._snapshots), 'max_snapshots': self.max_snapshots, 'backup_directory': str(self.backup_directory), 'auto_backup_enabled': self.enable_auto_backup, 'registered_callbacks': len(self._update_callbacks)}
    def _create_snapshot(self, description: str) -> str:
        self._snapshot_counter += 1
        snapshot_id = f'snapshot_{int(time.time())}_{self._snapshot_counter:04d}'
        for snapshot in self._snapshots:
            snapshot.is_active = False
        snapshot = ConfigurationSnapshot(snapshot_id=snapshot_id, timestamp=time.time(), config_data=self._current_config.dict(), environment=self._environment, description=description, is_active=True)
        self._snapshots.append(snapshot)
        if len(self._snapshots) > self.max_snapshots:
            removed = self._snapshots.pop(0)
            logger.debug(f'Removed old snapshot: {removed.snapshot_id}')
        if self.enable_auto_backup:
            self._save_snapshot_to_disk(snapshot)
        return snapshot_id
    def _save_snapshot_to_disk(self, snapshot: ConfigurationSnapshot) -> None:
        try:
            backup_file = self.backup_directory / f'{snapshot.snapshot_id}.json'
            with open(backup_file, 'w') as f:
                json.dump(asdict(snapshot), f, indent=2, default=str)
        except Exception as e:
            logger.warning(f'Failed to save snapshot backup: {e}')
    async def _notify_update_callbacks(self, new_config: DTESNConfig) -> None:
        for callback in self._update_callbacks:
            try:
                if asyncio.iscoroutinefunction(callback):
                    await callback(new_config)
                else:
                    callback(new_config)
            except Exception as e:
                logger.error(f'Configuration update callback failed: {e}')
_global_config_manager: Optional[DynamicConfigurationManager] = None
_manager_lock = threading.Lock()
def get_dynamic_config_manager() -> DynamicConfigurationManager:
    global _global_config_manager
    with _manager_lock:
        if _global_config_manager is None:
            _global_config_manager = DynamicConfigurationManager()
    return _global_config_manager
def initialize_dynamic_config_manager(initial_config: Optional[DTESNConfig]=None, **kwargs) -> DynamicConfigurationManager:
    global _global_config_manager
    with _manager_lock:
        if _global_config_manager is not None:
            logger.warning('Dynamic configuration manager already initialized')
        _global_config_manager = DynamicConfigurationManager(initial_config=initial_config, **kwargs)
        logger.info('Global dynamic configuration manager initialized')
        return _global_config_manager