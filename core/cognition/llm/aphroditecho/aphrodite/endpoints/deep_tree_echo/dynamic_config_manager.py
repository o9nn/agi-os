"""
Dynamic Configuration Manager for DTESN Parameters.

Implements server-side configuration management with dynamic updates,
validation, rollback mechanisms, and environment-specific handling.
Enables configuration changes without service restart.
"""

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
    """Configuration environments."""
    DEVELOPMENT = "development"
    STAGING = "staging"
    PRODUCTION = "production"
    TESTING = "testing"


@dataclass
class ConfigurationSnapshot:
    """Represents a configuration snapshot with metadata."""
    snapshot_id: str
    timestamp: float
    config_data: Dict[str, Any]
    environment: ConfigurationEnvironment
    description: str
    is_active: bool = False
    validation_errors: Optional[List[str]] = None


@dataclass
class ConfigurationUpdateRequest:
    """Request for configuration parameter update."""
    parameter_path: str  # e.g., "esn_reservoir_size" or "performance.enable_caching"
    new_value: Any
    description: Optional[str] = None
    validate_only: bool = False
    environment: Optional[ConfigurationEnvironment] = None


class ConfigurationValidator:
    """Validates DTESN configuration parameters."""
    
    def __init__(self):
        self._validators: Dict[str, Callable[[Any], bool]] = {
            "max_membrane_depth": lambda x: isinstance(x, int) and 1 <= x <= 64,
            "esn_reservoir_size": lambda x: isinstance(x, int) and 64 <= x <= 16384,
            "bseries_max_order": lambda x: isinstance(x, int) and 1 <= x <= 32,
            "cache_ttl_seconds": lambda x: isinstance(x, int) and 1 <= x <= 86400,
            "enable_caching": lambda x: isinstance(x, bool),
            "enable_docs": lambda x: isinstance(x, bool),
            "enable_performance_monitoring": lambda x: isinstance(x, bool),
        }
        
        self._dependencies: Dict[str, List[str]] = {
            "enable_caching": ["cache_ttl_seconds"],
            "max_membrane_depth": ["esn_reservoir_size", "bseries_max_order"],
        }
    
    def validate_parameter(self, param_path: str, value: Any) -> List[str]:
        """Validate a single parameter."""
        errors = []
        
        if param_path not in self._validators:
            errors.append(f"Unknown parameter: {param_path}")
            return errors
        
        if not self._validators[param_path](value):
            errors.append(f"Invalid value for {param_path}: {value}")
        
        return errors
    
    def validate_configuration(self, config_dict: Dict[str, Any]) -> List[str]:
        """Validate entire configuration."""
        errors = []
        
        # Validate individual parameters
        for param_path, value in config_dict.items():
            param_errors = self.validate_parameter(param_path, value)
            errors.extend(param_errors)
        
        # Validate dependencies
        for param, deps in self._dependencies.items():
            if param in config_dict and config_dict[param]:
                for dep in deps:
                    if dep not in config_dict:
                        errors.append(f"Missing dependency {dep} for parameter {param}")
        
        # Custom validation rules
        if ("max_membrane_depth" in config_dict and 
            "esn_reservoir_size" in config_dict):
            depth = config_dict["max_membrane_depth"]
            reservoir = config_dict["esn_reservoir_size"]
            if reservoir < (depth * 32):
                errors.append(
                    f"ESN reservoir size ({reservoir}) should be at least "
                    f"{depth * 32} for membrane depth {depth}"
                )
        
        return errors


class DynamicConfigurationManager:
    """
    Dynamic configuration manager for DTESN parameters.
    
    Provides server-side configuration management with:
    - Dynamic parameter updates without service restart
    - Configuration validation and rollback mechanisms
    - Environment-specific configuration handling
    - Configuration versioning and history
    """
    
    def __init__(
        self,
        initial_config: Optional[DTESNConfig] = None,
        max_snapshots: int = 50,
        backup_directory: Optional[Path] = None,
        enable_auto_backup: bool = True,
    ):
        self.max_snapshots = max_snapshots
        self.enable_auto_backup = enable_auto_backup
        self.backup_directory = backup_directory or Path("/tmp/dtesn_config_backups")
        self.backup_directory.mkdir(exist_ok=True)
        
        # Thread-safe configuration storage
        self._current_config = initial_config or DTESNConfig()
        self._config_lock = threading.RLock()
        
        # Configuration history
        self._snapshots: List[ConfigurationSnapshot] = []
        self._snapshot_counter = 0
        
        # Validation and callbacks
        self._validator = ConfigurationValidator()
        self._update_callbacks: List[Callable[[DTESNConfig], None]] = []
        self._environment = ConfigurationEnvironment.DEVELOPMENT
        
        # Create initial snapshot
        self._create_snapshot("Initial configuration")
        
        logger.info("Dynamic configuration manager initialized")
    
    @property
    def current_config(self) -> DTESNConfig:
        """Get current configuration (thread-safe)."""
        with self._config_lock:
            return self._current_config
    
    @property
    def environment(self) -> ConfigurationEnvironment:
        """Get current environment."""
        return self._environment
    
    def set_environment(self, environment: ConfigurationEnvironment) -> None:
        """Set configuration environment."""
        self._environment = environment
        logger.info(f"Configuration environment set to: {environment}")
    
    def register_update_callback(self, callback: Callable[[DTESNConfig], None]) -> None:
        """Register callback for configuration updates."""
        self._update_callbacks.append(callback)
    
    async def update_parameter(
        self, 
        request: ConfigurationUpdateRequest
    ) -> Dict[str, Any]:
        """
        Update a single configuration parameter.
        
        Returns:
            Dict with success status, validation errors, and rollback info
        """
        try:
            with self._config_lock:
                # Get current configuration as dict
                current_dict = self._current_config.dict()
                
                # Apply the parameter update
                if "." in request.parameter_path:
                    # Handle nested parameters (future extension)
                    return {
                        "success": False,
                        "error": "Nested parameter updates not yet supported",
                        "parameter": request.parameter_path
                    }
                else:
                    # Simple parameter update
                    if request.parameter_path not in current_dict:
                        return {
                            "success": False,
                            "error": f"Unknown parameter: {request.parameter_path}",
                            "parameter": request.parameter_path
                        }
                    
                    # Create new configuration dict
                    new_dict = current_dict.copy()
                    new_dict[request.parameter_path] = request.new_value
                
                # Validate the new configuration
                validation_errors = self._validator.validate_configuration(new_dict)
                
                if request.validate_only:
                    return {
                        "success": len(validation_errors) == 0,
                        "validation_errors": validation_errors,
                        "parameter": request.parameter_path,
                        "validate_only": True
                    }
                
                if validation_errors:
                    return {
                        "success": False,
                        "validation_errors": validation_errors,
                        "parameter": request.parameter_path
                    }
                
                # Create snapshot before update
                previous_snapshot_id = self._create_snapshot(
                    f"Before updating {request.parameter_path}"
                )
                
                try:
                    # Apply the configuration update
                    new_config = DTESNConfig(**new_dict)
                    old_config = self._current_config
                    self._current_config = new_config
                    
                    # Notify callbacks
                    await self._notify_update_callbacks(new_config)
                    
                    # Create success snapshot
                    new_snapshot_id = self._create_snapshot(
                        request.description or f"Updated {request.parameter_path} to {request.new_value}"
                    )
                    
                    logger.info(
                        f"Configuration parameter {request.parameter_path} updated "
                        f"from {getattr(old_config, request.parameter_path)} to {request.new_value}"
                    )
                    
                    return {
                        "success": True,
                        "parameter": request.parameter_path,
                        "old_value": getattr(old_config, request.parameter_path),
                        "new_value": request.new_value,
                        "snapshot_id": new_snapshot_id,
                        "rollback_snapshot": previous_snapshot_id
                    }
                    
                except Exception as e:
                    logger.error(f"Failed to apply configuration update: {e}")
                    return {
                        "success": False,
                        "error": f"Failed to apply update: {str(e)}",
                        "parameter": request.parameter_path,
                        "rollback_snapshot": previous_snapshot_id
                    }
                
        except Exception as e:
            logger.error(f"Configuration update failed: {e}")
            return {
                "success": False,
                "error": f"Update failed: {str(e)}",
                "parameter": request.parameter_path
            }
    
    async def update_multiple_parameters(
        self,
        updates: List[ConfigurationUpdateRequest]
    ) -> Dict[str, Any]:
        """Update multiple configuration parameters atomically."""
        try:
            with self._config_lock:
                # Get current configuration
                current_dict = self._current_config.dict()
                
                # Apply all updates to a temporary dict
                new_dict = current_dict.copy()
                applied_updates = []
                
                for update in updates:
                    if "." in update.parameter_path:
                        return {
                            "success": False,
                            "error": "Nested parameter updates not yet supported",
                            "failed_parameter": update.parameter_path
                        }
                    
                    if update.parameter_path not in current_dict:
                        return {
                            "success": False,
                            "error": f"Unknown parameter: {update.parameter_path}",
                            "failed_parameter": update.parameter_path
                        }
                    
                    new_dict[update.parameter_path] = update.new_value
                    applied_updates.append({
                        "parameter": update.parameter_path,
                        "old_value": current_dict[update.parameter_path],
                        "new_value": update.new_value
                    })
                
                # Validate the entire new configuration
                validation_errors = self._validator.validate_configuration(new_dict)
                
                if validation_errors:
                    return {
                        "success": False,
                        "validation_errors": validation_errors,
                        "attempted_updates": applied_updates
                    }
                
                # Create snapshot before update
                previous_snapshot_id = self._create_snapshot("Before batch update")
                
                try:
                    # Apply the configuration update
                    new_config = DTESNConfig(**new_dict)
                    self._current_config = new_config
                    
                    # Notify callbacks
                    await self._notify_update_callbacks(new_config)
                    
                    # Create success snapshot
                    new_snapshot_id = self._create_snapshot(
                        f"Batch update of {len(updates)} parameters"
                    )
                    
                    logger.info(f"Batch configuration update applied: {len(updates)} parameters")
                    
                    return {
                        "success": True,
                        "updated_parameters": applied_updates,
                        "snapshot_id": new_snapshot_id,
                        "rollback_snapshot": previous_snapshot_id
                    }
                    
                except Exception as e:
                    logger.error(f"Failed to apply batch configuration update: {e}")
                    return {
                        "success": False,
                        "error": f"Failed to apply batch update: {str(e)}",
                        "attempted_updates": applied_updates,
                        "rollback_snapshot": previous_snapshot_id
                    }
                
        except Exception as e:
            logger.error(f"Batch configuration update failed: {e}")
            return {
                "success": False,
                "error": f"Batch update failed: {str(e)}"
            }
    
    async def rollback_to_snapshot(self, snapshot_id: str) -> Dict[str, Any]:
        """Rollback configuration to a specific snapshot."""
        try:
            with self._config_lock:
                # Find the snapshot
                target_snapshot = None
                for snapshot in self._snapshots:
                    if snapshot.snapshot_id == snapshot_id:
                        target_snapshot = snapshot
                        break
                
                if not target_snapshot:
                    return {
                        "success": False,
                        "error": f"Snapshot {snapshot_id} not found"
                    }
                
                # Create snapshot before rollback
                pre_rollback_snapshot = self._create_snapshot(
                    f"Before rollback to {snapshot_id}"
                )
                
                try:
                    # Apply the rollback
                    old_config = self._current_config
                    rollback_config = DTESNConfig(**target_snapshot.config_data)
                    self._current_config = rollback_config
                    
                    # Notify callbacks
                    await self._notify_update_callbacks(rollback_config)
                    
                    # Create rollback success snapshot
                    rollback_snapshot_id = self._create_snapshot(
                        f"Rollback to snapshot {snapshot_id}: {target_snapshot.description}"
                    )
                    
                    logger.info(f"Configuration rolled back to snapshot {snapshot_id}")
                    
                    return {
                        "success": True,
                        "rolled_back_to": snapshot_id,
                        "rollback_snapshot": rollback_snapshot_id,
                        "undo_rollback_snapshot": pre_rollback_snapshot
                    }
                    
                except Exception as e:
                    logger.error(f"Failed to rollback configuration: {e}")
                    return {
                        "success": False,
                        "error": f"Rollback failed: {str(e)}",
                        "target_snapshot": snapshot_id
                    }
                
        except Exception as e:
            logger.error(f"Configuration rollback failed: {e}")
            return {
                "success": False,
                "error": f"Rollback failed: {str(e)}"
            }
    
    def get_snapshots(self) -> List[Dict[str, Any]]:
        """Get list of configuration snapshots."""
        with self._config_lock:
            return [
                {
                    "snapshot_id": snapshot.snapshot_id,
                    "timestamp": snapshot.timestamp,
                    "description": snapshot.description,
                    "environment": snapshot.environment.value,
                    "is_active": snapshot.is_active,
                    "validation_errors": snapshot.validation_errors
                }
                for snapshot in self._snapshots
            ]
    
    def get_current_status(self) -> Dict[str, Any]:
        """Get current configuration manager status."""
        with self._config_lock:
            return {
                "current_config": self._current_config.dict(),
                "environment": self._environment.value,
                "total_snapshots": len(self._snapshots),
                "max_snapshots": self.max_snapshots,
                "backup_directory": str(self.backup_directory),
                "auto_backup_enabled": self.enable_auto_backup,
                "registered_callbacks": len(self._update_callbacks)
            }
    
    def _create_snapshot(self, description: str) -> str:
        """Create a configuration snapshot."""
        self._snapshot_counter += 1
        snapshot_id = f"snapshot_{int(time.time())}_{self._snapshot_counter:04d}"
        
        # Deactivate previous snapshots
        for snapshot in self._snapshots:
            snapshot.is_active = False
        
        # Create new snapshot
        snapshot = ConfigurationSnapshot(
            snapshot_id=snapshot_id,
            timestamp=time.time(),
            config_data=self._current_config.dict(),
            environment=self._environment,
            description=description,
            is_active=True
        )
        
        self._snapshots.append(snapshot)
        
        # Clean up old snapshots
        if len(self._snapshots) > self.max_snapshots:
            removed = self._snapshots.pop(0)
            logger.debug(f"Removed old snapshot: {removed.snapshot_id}")
        
        # Auto-backup if enabled
        if self.enable_auto_backup:
            self._save_snapshot_to_disk(snapshot)
        
        return snapshot_id
    
    def _save_snapshot_to_disk(self, snapshot: ConfigurationSnapshot) -> None:
        """Save snapshot to disk for backup."""
        try:
            backup_file = self.backup_directory / f"{snapshot.snapshot_id}.json"
            with open(backup_file, 'w') as f:
                json.dump(asdict(snapshot), f, indent=2, default=str)
        except Exception as e:
            logger.warning(f"Failed to save snapshot backup: {e}")
    
    async def _notify_update_callbacks(self, new_config: DTESNConfig) -> None:
        """Notify registered callbacks of configuration updates."""
        for callback in self._update_callbacks:
            try:
                if asyncio.iscoroutinefunction(callback):
                    await callback(new_config)
                else:
                    callback(new_config)
            except Exception as e:
                logger.error(f"Configuration update callback failed: {e}")


# Global configuration manager instance (thread-safe singleton)
_global_config_manager: Optional[DynamicConfigurationManager] = None
_manager_lock = threading.Lock()


def get_dynamic_config_manager() -> DynamicConfigurationManager:
    """Get the global dynamic configuration manager instance."""
    global _global_config_manager
    
    with _manager_lock:
        if _global_config_manager is None:
            _global_config_manager = DynamicConfigurationManager()
    
    return _global_config_manager


def initialize_dynamic_config_manager(
    initial_config: Optional[DTESNConfig] = None,
    **kwargs
) -> DynamicConfigurationManager:
    """Initialize the global dynamic configuration manager."""
    global _global_config_manager
    
    with _manager_lock:
        if _global_config_manager is not None:
            logger.warning("Dynamic configuration manager already initialized")
        
        _global_config_manager = DynamicConfigurationManager(
            initial_config=initial_config,
            **kwargs
        )
        
        logger.info("Global dynamic configuration manager initialized")
        return _global_config_manager