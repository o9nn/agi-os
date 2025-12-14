#!/usr/bin/env python3
"""
Configuration Management for Backend Services

Provides centralized configuration management for backend service integration
with dynamic updates, validation, and rollback capabilities.

Features:
- Dynamic configuration updates without service restart
- Configuration validation and rollback mechanisms
- Environment-specific configuration management
- Integration with service discovery and degradation systems
"""

import asyncio
import json
import logging
import os
from typing import Dict, Any, Optional, List, Callable, Union
from dataclasses import dataclass, field, asdict
from datetime import datetime
from enum import Enum
import hashlib

try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None

try:
    import yaml
    YAML_AVAILABLE = True
except ImportError:
    YAML_AVAILABLE = False
    yaml = None

logger = logging.getLogger(__name__)


class ConfigSource(Enum):
    """Configuration sources"""
    FILE = "file"
    REDIS = "redis"
    ENVIRONMENT = "environment"
    API = "api"


@dataclass
class ConfigEntry:
    """Configuration entry with metadata"""
    key: str
    value: Any
    source: ConfigSource
    timestamp: float
    version: int = 1
    description: str = ""
    validation_schema: Optional[Dict] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def config_hash(self) -> str:
        """Generate hash for configuration value"""
        content = json.dumps(self.value, sort_keys=True, default=str)
        return hashlib.sha256(content.encode()).hexdigest()[:16]


@dataclass
class ConfigVersion:
    """Configuration version snapshot"""
    version_id: str
    timestamp: float
    config_entries: Dict[str, ConfigEntry]
    description: str = ""
    created_by: str = "system"
    
    @property
    def version_hash(self) -> str:
        """Generate hash for entire configuration version"""
        content = json.dumps(
            {k: v.config_hash for k, v in self.config_entries.items()},
            sort_keys=True
        )
        return hashlib.sha256(content.encode()).hexdigest()[:16]


class ConfigurationManager:
    """
    Centralized configuration management system
    """
    
    def __init__(self,
                 service_name: str,
                 redis_url: Optional[str] = None,
                 config_file: Optional[str] = None,
                 auto_reload: bool = True,
                 validation_enabled: bool = True):
        self.service_name = service_name
        self.redis_url = redis_url
        self.config_file = config_file
        self.auto_reload = auto_reload
        self.validation_enabled = validation_enabled
        
        # Configuration storage
        self.config_entries: Dict[str, ConfigEntry] = {}
        self.config_versions: List[ConfigVersion] = []
        self.current_version: Optional[ConfigVersion] = None
        
        # Redis connection
        self.redis: Optional[aioredis.Redis] = None
        
        # Change callbacks
        self.change_callbacks: Dict[str, List[Callable]] = {}
        self.global_change_callbacks: List[Callable] = []
        
        # Validation functions
        self.validators: Dict[str, Callable] = {}
        
        # Background tasks
        self.reload_task: Optional[asyncio.Task] = None
        
        self._initialized = False
        self._lock = asyncio.Lock()

    async def initialize(self) -> None:
        """Initialize configuration manager"""
        if self._initialized:
            return
            
        async with self._lock:
            if self._initialized:
                return
                
            # Connect to Redis if available
            if self.redis_url and REDIS_AVAILABLE and aioredis:
                try:
                    self.redis = aioredis.from_url(
                        self.redis_url,
                        decode_responses=True,
                        retry_on_error=[ConnectionError, OSError]
                    )
                    await self.redis.ping()
                    logger.info("✅ Configuration manager connected to Redis")
                except Exception as e:
                    logger.warning(f"Redis connection failed: {e}")
                    self.redis = None
            
            # Load initial configuration
            await self._load_initial_configuration()
            
            # Start auto-reload if enabled
            if self.auto_reload:
                self.reload_task = asyncio.create_task(self._auto_reload_loop())
            
            self._initialized = True
            logger.info(f"🚀 Configuration manager initialized for {self.service_name}")

    async def shutdown(self) -> None:
        """Shutdown configuration manager"""
        if not self._initialized:
            return
            
        # Cancel auto-reload task
        if self.reload_task:
            self.reload_task.cancel()
            try:
                await self.reload_task
            except asyncio.CancelledError:
                pass
        
        # Close Redis connection
        if self.redis:
            await self.redis.aclose()
            
        self._initialized = False
        logger.info("🔌 Configuration manager shutdown")

    async def set_config(self,
                        key: str,
                        value: Any,
                        source: ConfigSource = ConfigSource.API,
                        description: str = "",
                        validate: bool = True) -> bool:
        """
        Set configuration value
        
        Args:
            key: Configuration key
            value: Configuration value
            source: Source of configuration
            description: Description of the change
            validate: Whether to validate the value
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Validate if enabled and validator exists
            if validate and self.validation_enabled and key in self.validators:
                validator = self.validators[key]
                if not await self._validate_value(validator, value):
                    logger.error(f"Validation failed for config key: {key}")
                    return False
            
            # Create configuration entry
            entry = ConfigEntry(
                key=key,
                value=value,
                source=source,
                timestamp=datetime.now().timestamp(),
                description=description
            )
            
            # Store old value for rollback
            old_entry = self.config_entries.get(key)
            
            # Update configuration
            self.config_entries[key] = entry
            
            # Save to persistent storage
            await self._save_config_entry(entry)
            
            # Trigger change callbacks
            await self._trigger_change_callbacks(key, value, old_entry.value if old_entry else None)
            
            logger.info(f"📝 Configuration updated: {key} = {value} (source: {source.value})")
            return True
            
        except Exception as e:
            logger.error(f"Failed to set config {key}: {e}")
            return False

    async def get_config(self, key: str, default: Any = None) -> Any:
        """Get configuration value"""
        entry = self.config_entries.get(key)
        if entry:
            return entry.value
        return default

    async def get_config_entry(self, key: str) -> Optional[ConfigEntry]:
        """Get full configuration entry with metadata"""
        return self.config_entries.get(key)

    async def delete_config(self, key: str) -> bool:
        """Delete configuration entry"""
        try:
            if key in self.config_entries:
                old_entry = self.config_entries[key]
                del self.config_entries[key]
                
                # Remove from persistent storage
                await self._delete_config_entry(key)
                
                # Trigger change callbacks
                await self._trigger_change_callbacks(key, None, old_entry.value)
                
                logger.info(f"🗑️ Configuration deleted: {key}")
                return True
            return False
            
        except Exception as e:
            logger.error(f"Failed to delete config {key}: {e}")
            return False

    async def bulk_update(self, config_dict: Dict[str, Any], 
                         source: ConfigSource = ConfigSource.API,
                         description: str = "Bulk update") -> Dict[str, bool]:
        """
        Update multiple configuration values
        
        Args:
            config_dict: Dictionary of key-value pairs
            source: Source of configuration
            description: Description of the bulk update
            
        Returns:
            Dictionary of key -> success status
        """
        results = {}
        
        # Create version snapshot before bulk update
        await self._create_version_snapshot(f"Before {description}")
        
        for key, value in config_dict.items():
            results[key] = await self.set_config(key, value, source, description)
        
        # Create version snapshot after bulk update
        if any(results.values()):
            await self._create_version_snapshot(f"After {description}")
        
        return results

    def add_change_callback(self, key: str, callback: Callable[[str, Any, Any], None]) -> None:
        """Add callback for specific configuration key changes"""
        if key not in self.change_callbacks:
            self.change_callbacks[key] = []
        self.change_callbacks[key].append(callback)

    def add_global_change_callback(self, callback: Callable[[str, Any, Any], None]) -> None:
        """Add callback for any configuration change"""
        self.global_change_callbacks.append(callback)

    def add_validator(self, key: str, validator: Callable[[Any], bool]) -> None:
        """Add validation function for configuration key"""
        self.validators[key] = validator

    async def create_version_snapshot(self, description: str = "") -> str:
        """Create a named version snapshot of current configuration"""
        return await self._create_version_snapshot(description)

    async def list_versions(self) -> List[Dict[str, Any]]:
        """List all configuration versions"""
        return [
            {
                "version_id": version.version_id,
                "timestamp": version.timestamp,
                "description": version.description,
                "created_by": version.created_by,
                "version_hash": version.version_hash,
                "config_count": len(version.config_entries)
            }
            for version in self.config_versions
        ]

    async def rollback_to_version(self, version_id: str) -> bool:
        """
        Rollback configuration to a specific version
        
        Args:
            version_id: Version ID to rollback to
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Find version
            target_version = None
            for version in self.config_versions:
                if version.version_id == version_id:
                    target_version = version
                    break
            
            if not target_version:
                logger.error(f"Version not found: {version_id}")
                return False
            
            # Create snapshot before rollback
            await self._create_version_snapshot(f"Before rollback to {version_id}")
            
            # Apply version configuration
            old_config = dict(self.config_entries)
            self.config_entries = dict(target_version.config_entries)
            
            # Save all entries to persistent storage
            for entry in self.config_entries.values():
                await self._save_config_entry(entry)
            
            # Trigger change callbacks for all changed values
            all_keys = set(old_config.keys()) | set(self.config_entries.keys())
            for key in all_keys:
                old_value = old_config.get(key, {}).value if key in old_config else None
                new_value = self.config_entries.get(key, {}).value if key in self.config_entries else None
                
                if old_value != new_value:
                    await self._trigger_change_callbacks(key, new_value, old_value)
            
            logger.info(f"🔄 Configuration rolled back to version: {version_id}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to rollback to version {version_id}: {e}")
            return False

    async def reload_configuration(self) -> bool:
        """Manually reload configuration from all sources"""
        try:
            await self._load_initial_configuration()
            logger.info("🔄 Configuration reloaded")
            return True
        except Exception as e:
            logger.error(f"Failed to reload configuration: {e}")
            return False

    async def get_all_config(self) -> Dict[str, Any]:
        """Get all configuration as key-value dictionary"""
        return {key: entry.value for key, entry in self.config_entries.items()}

    async def get_config_summary(self) -> Dict[str, Any]:
        """Get configuration summary with metadata"""
        return {
            "service_name": self.service_name,
            "total_entries": len(self.config_entries),
            "sources": list({entry.source.value for entry in self.config_entries.values()}),
            "versions": len(self.config_versions),
            "current_version": self.current_version.version_id if self.current_version else None,
            "last_updated": max(
                (entry.timestamp for entry in self.config_entries.values()),
                default=0
            )
        }

    async def export_configuration(self, format: str = "json") -> str:
        """Export configuration in specified format"""
        config_data = {
            "service_name": self.service_name,
            "export_timestamp": datetime.now().isoformat(),
            "configuration": {
                key: {
                    "value": entry.value,
                    "source": entry.source.value,
                    "timestamp": entry.timestamp,
                    "description": entry.description
                }
                for key, entry in self.config_entries.items()
            }
        }
        
        if format.lower() == "json":
            return json.dumps(config_data, indent=2, default=str)
        elif format.lower() == "yaml" and YAML_AVAILABLE:
            return yaml.dump(config_data, default_flow_style=False)
        else:
            raise ValueError(f"Unsupported export format: {format}")

    async def import_configuration(self, config_data: str, format: str = "json") -> bool:
        """Import configuration from specified format"""
        try:
            if format.lower() == "json":
                data = json.loads(config_data)
            elif format.lower() == "yaml" and YAML_AVAILABLE:
                data = yaml.safe_load(config_data)
            else:
                raise ValueError(f"Unsupported import format: {format}")
            
            # Create snapshot before import
            await self._create_version_snapshot("Before configuration import")
            
            # Import configuration entries
            config_dict = {}
            for key, entry_data in data.get("configuration", {}).items():
                config_dict[key] = entry_data["value"]
            
            # Bulk update
            results = await self.bulk_update(config_dict, ConfigSource.API, "Configuration import")
            
            logger.info(f"📥 Configuration imported: {len(config_dict)} entries")
            return all(results.values())
            
        except Exception as e:
            logger.error(f"Failed to import configuration: {e}")
            return False

    async def _load_initial_configuration(self) -> None:
        """Load configuration from all configured sources"""
        # Load from file if specified
        if self.config_file and os.path.exists(self.config_file):
            await self._load_from_file()
        
        # Load from Redis if available
        if self.redis:
            await self._load_from_redis()
        
        # Load from environment variables
        await self._load_from_environment()
        
        # Create initial version snapshot
        if self.config_entries:
            await self._create_version_snapshot("Initial configuration")

    async def _load_from_file(self) -> None:
        """Load configuration from file"""
        try:
            with open(self.config_file, 'r') as f:
                if self.config_file.endswith(('.yml', '.yaml')) and YAML_AVAILABLE:
                    data = yaml.safe_load(f)
                else:
                    data = json.load(f)
            
            for key, value in data.items():
                if key not in self.config_entries:  # Don't override existing
                    entry = ConfigEntry(
                        key=key,
                        value=value,
                        source=ConfigSource.FILE,
                        timestamp=datetime.now().timestamp(),
                        description=f"Loaded from {self.config_file}"
                    )
                    self.config_entries[key] = entry
            
            logger.info(f"📁 Loaded configuration from file: {self.config_file}")
            
        except Exception as e:
            logger.error(f"Failed to load configuration from file: {e}")

    async def _load_from_redis(self) -> None:
        """Load configuration from Redis"""
        try:
            config_key = f"config:{self.service_name}"
            config_data = await self.redis.hgetall(config_key)
            
            for key, value_json in config_data.items():
                if key not in self.config_entries:  # Don't override existing
                    entry_data = json.loads(value_json)
                    entry = ConfigEntry(
                        key=key,
                        value=entry_data["value"],
                        source=ConfigSource.REDIS,
                        timestamp=entry_data.get("timestamp", datetime.now().timestamp()),
                        description=entry_data.get("description", "Loaded from Redis")
                    )
                    self.config_entries[key] = entry
            
            logger.info(f"📦 Loaded configuration from Redis: {len(config_data)} entries")
            
        except Exception as e:
            logger.error(f"Failed to load configuration from Redis: {e}")

    async def _load_from_environment(self) -> None:
        """Load configuration from environment variables"""
        prefix = f"{self.service_name.upper().replace('-', '_')}_"
        
        for key, value in os.environ.items():
            if key.startswith(prefix):
                config_key = key[len(prefix):].lower()
                
                if config_key not in self.config_entries:  # Don't override existing
                    # Try to parse as JSON, fallback to string
                    try:
                        parsed_value = json.loads(value)
                    except (json.JSONDecodeError, ValueError):
                        parsed_value = value
                    
                    entry = ConfigEntry(
                        key=config_key,
                        value=parsed_value,
                        source=ConfigSource.ENVIRONMENT,
                        timestamp=datetime.now().timestamp(),
                        description=f"Loaded from environment variable {key}"
                    )
                    self.config_entries[config_key] = entry
        
        env_count = sum(1 for k in os.environ if k.startswith(prefix))
        if env_count > 0:
            logger.info(f"🌍 Loaded configuration from environment: {env_count} entries")

    async def _save_config_entry(self, entry: ConfigEntry) -> None:
        """Save configuration entry to persistent storage"""
        # Save to Redis if available
        if self.redis:
            try:
                config_key = f"config:{self.service_name}"
                entry_data = {
                    "value": entry.value,
                    "source": entry.source.value,
                    "timestamp": entry.timestamp,
                    "description": entry.description
                }
                await self.redis.hset(config_key, entry.key, json.dumps(entry_data, default=str))
                
            except Exception as e:
                logger.error(f"Failed to save config entry to Redis: {e}")

    async def _delete_config_entry(self, key: str) -> None:
        """Delete configuration entry from persistent storage"""
        # Delete from Redis if available
        if self.redis:
            try:
                config_key = f"config:{self.service_name}"
                await self.redis.hdel(config_key, key)
                
            except Exception as e:
                logger.error(f"Failed to delete config entry from Redis: {e}")

    async def _create_version_snapshot(self, description: str) -> str:
        """Create a version snapshot"""
        version_id = f"v{len(self.config_versions) + 1}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        version = ConfigVersion(
            version_id=version_id,
            timestamp=datetime.now().timestamp(),
            config_entries=dict(self.config_entries),
            description=description
        )
        
        self.config_versions.append(version)
        self.current_version = version
        
        # Keep only last 10 versions
        if len(self.config_versions) > 10:
            self.config_versions = self.config_versions[-10:]
        
        logger.debug(f"📸 Created configuration snapshot: {version_id}")
        return version_id

    async def _trigger_change_callbacks(self, key: str, new_value: Any, old_value: Any) -> None:
        """Trigger change callbacks"""
        # Key-specific callbacks
        if key in self.change_callbacks:
            for callback in self.change_callbacks[key]:
                try:
                    if asyncio.iscoroutinefunction(callback):
                        await callback(key, new_value, old_value)
                    else:
                        callback(key, new_value, old_value)
                except Exception as e:
                    logger.error(f"Change callback error for {key}: {e}")
        
        # Global callbacks
        for callback in self.global_change_callbacks:
            try:
                if asyncio.iscoroutinefunction(callback):
                    await callback(key, new_value, old_value)
                else:
                    callback(key, new_value, old_value)
            except Exception as e:
                logger.error(f"Global change callback error: {e}")

    async def _validate_value(self, validator: Callable, value: Any) -> bool:
        """Validate configuration value"""
        try:
            if asyncio.iscoroutinefunction(validator):
                return await validator(value)
            else:
                return validator(value)
        except Exception as e:
            logger.error(f"Validation error: {e}")
            return False

    async def _auto_reload_loop(self) -> None:
        """Auto-reload configuration loop"""
        while True:
            try:
                await asyncio.sleep(60)  # Check every minute
                
                # Check if file has been modified
                if self.config_file and os.path.exists(self.config_file):
                    file_mtime = os.path.getmtime(self.config_file)
                    last_load_time = max(
                        (entry.timestamp for entry in self.config_entries.values()
                         if entry.source == ConfigSource.FILE),
                        default=0
                    )
                    
                    if file_mtime > last_load_time:
                        await self._load_from_file()
                        logger.info("🔄 Configuration file reloaded")
                
                # Check Redis for updates
                if self.redis:
                    await self._check_redis_updates()
                    
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Auto-reload error: {e}")

    async def _check_redis_updates(self) -> None:
        """Check for configuration updates in Redis"""
        try:
            config_key = f"config:{self.service_name}:updates"
            update_timestamp = await self.redis.get(config_key)
            
            if update_timestamp:
                last_update = float(update_timestamp)
                last_local_update = max(
                    (entry.timestamp for entry in self.config_entries.values()),
                    default=0
                )
                
                if last_update > last_local_update:
                    await self._load_from_redis()
                    logger.info("🔄 Configuration updated from Redis")
                    
        except Exception as e:
            logger.error(f"Redis update check error: {e}")


# Global configuration manager instance
_global_config_manager: Optional[ConfigurationManager] = None


def get_config_manager(service_name: str = "backend-services", **kwargs) -> ConfigurationManager:
    """Get or create global configuration manager"""
    global _global_config_manager
    
    if _global_config_manager is None:
        _global_config_manager = ConfigurationManager(service_name, **kwargs)
    
    return _global_config_manager