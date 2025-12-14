"""
Production Data Pipeline for Deep Tree Echo System Network.

Implements Phase 8.2.3 requirements:
- Robust data collection from server-side operations
- Data quality validation and anomaly detection
- Data retention and privacy compliance mechanisms

This module provides enterprise-grade data collection, validation, and compliance
for production MLOps environments with high-quality data feeds for model improvements.
"""

import asyncio
import hashlib
import json
import logging
import os
import time
import uuid
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone, timedelta
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Union, Callable, AsyncGenerator
from collections import defaultdict, deque
import sqlite3

# Performance monitoring imports
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False

# Statistical analysis imports  
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False

from aphrodite.endpoints.deep_tree_echo.data_pipeline import (
    DataProcessingPipeline, PipelineConfiguration
)

logger = logging.getLogger(__name__)


class DataClassification(Enum):
    """Data classification levels for privacy and compliance."""
    PUBLIC = "public"
    INTERNAL = "internal"
    CONFIDENTIAL = "confidential"
    RESTRICTED = "restricted"


class RetentionPolicy(Enum):
    """Data retention policy options."""
    SHORT_TERM = "short_term"  # 7 days
    MEDIUM_TERM = "medium_term"  # 30 days
    LONG_TERM = "long_term"  # 365 days
    PERMANENT = "permanent"  # Indefinite
    COMPLIANCE_REQUIRED = "compliance_required"  # Based on regulations


@dataclass
class DataCollectionEvent:
    """Represents a single data collection event from server operations."""
    
    # Event identification
    event_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    session_id: str = ""
    request_id: str = ""
    
    # Event metadata
    event_type: str = ""  # e.g., "inference_request", "model_update", "error_event"
    source_component: str = ""  # e.g., "aphrodite_engine", "dtesn_processor", "aar_core"
    operation: str = ""  # Specific operation being performed
    
    # Data payload
    input_data: Dict[str, Any] = field(default_factory=dict)
    output_data: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    # Performance metrics
    processing_time_ms: float = 0.0
    memory_usage_mb: float = 0.0
    cpu_utilization: float = 0.0
    
    # Quality indicators
    success: bool = True
    error_message: Optional[str] = None
    quality_score: float = 1.0  # 0.0 to 1.0
    
    # Privacy and compliance
    classification: DataClassification = DataClassification.INTERNAL
    retention_policy: RetentionPolicy = RetentionPolicy.MEDIUM_TERM
    contains_pii: bool = False
    privacy_tags: List[str] = field(default_factory=list)
    
    # Data lineage
    parent_event_ids: List[str] = field(default_factory=list)
    child_event_ids: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert event to dictionary for storage and transmission."""
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        data['classification'] = self.classification.value
        data['retention_policy'] = self.retention_policy.value
        return data
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'DataCollectionEvent':
        """Create event from dictionary."""
        # Convert timestamp from ISO string
        if isinstance(data.get('timestamp'), str):
            data['timestamp'] = datetime.fromisoformat(data['timestamp'].replace('Z', '+00:00'))
        
        # Convert enums from strings
        if isinstance(data.get('classification'), str):
            data['classification'] = DataClassification(data['classification'])
        if isinstance(data.get('retention_policy'), str):
            data['retention_policy'] = RetentionPolicy(data['retention_policy'])
        
        return cls(**data)


@dataclass
class QualityValidationRule:
    """Defines a data quality validation rule."""
    
    rule_id: str
    name: str
    description: str
    rule_type: str  # e.g., "range_check", "null_check", "format_check", "anomaly_detection"
    target_field: str
    parameters: Dict[str, Any] = field(default_factory=dict)
    severity: str = "warning"  # "info", "warning", "error", "critical"
    enabled: bool = True
    
    def validate(self, event: DataCollectionEvent) -> Dict[str, Any]:
        """Validate event against this rule."""
        result = {
            "rule_id": self.rule_id,
            "rule_name": self.name,
            "passed": True,
            "message": "",
            "severity": self.severity,
            "details": {}
        }
        
        try:
            # Get the target field value
            target_value = self._get_field_value(event, self.target_field)
            
            if self.rule_type == "null_check":
                if target_value is None:
                    result["passed"] = False
                    result["message"] = f"Field '{self.target_field}' cannot be null"
            
            elif self.rule_type == "range_check":
                min_val = self.parameters.get("min")
                max_val = self.parameters.get("max")
                if min_val is not None and target_value < min_val:
                    result["passed"] = False
                    result["message"] = f"Field '{self.target_field}' value {target_value} below minimum {min_val}"
                elif max_val is not None and target_value > max_val:
                    result["passed"] = False
                    result["message"] = f"Field '{self.target_field}' value {target_value} above maximum {max_val}"
            
            elif self.rule_type == "format_check":
                pattern = self.parameters.get("pattern")
                if pattern and isinstance(target_value, str):
                    import re
                    if not re.match(pattern, target_value):
                        result["passed"] = False
                        result["message"] = f"Field '{self.target_field}' does not match required format"
            
            elif self.rule_type == "anomaly_detection":
                # Simple statistical anomaly detection
                historical_values = self.parameters.get("historical_values", [])
                if len(historical_values) > 10 and NUMPY_AVAILABLE:
                    values_array = np.array(historical_values)
                    mean = np.mean(values_array)
                    std = np.std(values_array)
                    z_score = abs((target_value - mean) / std) if std > 0 else 0
                    
                    threshold = self.parameters.get("z_score_threshold", 3.0)
                    if z_score > threshold:
                        result["passed"] = False
                        result["message"] = f"Field '{self.target_field}' shows anomaly (z-score: {z_score:.2f})"
                        result["details"]["z_score"] = z_score
                        result["details"]["mean"] = mean
                        result["details"]["std"] = std
            
        except Exception as e:
            result["passed"] = False
            result["message"] = f"Validation error: {str(e)}"
            result["severity"] = "error"
        
        return result
    
    def _get_field_value(self, event: DataCollectionEvent, field_path: str) -> Any:
        """Extract field value from event using dot notation."""
        parts = field_path.split('.')
        value = event
        
        for part in parts:
            if hasattr(value, part):
                value = getattr(value, part)
            elif isinstance(value, dict) and part in value:
                value = value[part]
            else:
                return None
        
        return value


class ProductionDataCollector:
    """
    Robust data collection system for server-side operations.
    
    Collects comprehensive data from all Deep Tree Echo components
    with high performance and reliability guarantees.
    """
    
    def __init__(
        self,
        storage_path: str = "/tmp/production_data",
        batch_size: int = 1000,
        flush_interval: float = 30.0,
        enable_compression: bool = True
    ):
        """Initialize production data collector."""
        self.storage_path = Path(storage_path)
        self.storage_path.mkdir(parents=True, exist_ok=True)
        
        self.batch_size = batch_size
        self.flush_interval = flush_interval
        self.enable_compression = enable_compression
        
        # In-memory buffer for high-performance collection
        self._event_buffer = deque(maxlen=batch_size * 2)
        self._buffer_lock = asyncio.Lock()
        
        # Statistics tracking
        self.stats = {
            "events_collected": 0,
            "events_flushed": 0,
            "errors": 0,
            "last_flush_time": None,
            "avg_processing_time_ms": 0.0
        }
        
        # Background tasks
        self._flush_task = None
        self._is_running = False
        
        logger.info(f"Initialized production data collector with storage: {self.storage_path}")
    
    async def start(self):
        """Start the data collector background tasks."""
        if self._is_running:
            return
        
        self._is_running = True
        self._flush_task = asyncio.create_task(self._periodic_flush())
        logger.info("Production data collector started")
    
    async def stop(self):
        """Stop the data collector and flush remaining data."""
        if not self._is_running:
            return
        
        self._is_running = False
        
        if self._flush_task:
            self._flush_task.cancel()
            try:
                await self._flush_task
            except asyncio.CancelledError:
                pass
        
        # Final flush
        await self._flush_buffer()
        logger.info("Production data collector stopped")
    
    async def collect_event(
        self, 
        event_type: str,
        source_component: str,
        operation: str,
        input_data: Optional[Dict[str, Any]] = None,
        output_data: Optional[Dict[str, Any]] = None,
        metadata: Optional[Dict[str, Any]] = None,
        session_id: str = "",
        request_id: str = "",
        **kwargs
    ) -> str:
        """
        Collect a data event from server operations.
        
        Args:
            event_type: Type of event (e.g., "inference_request")
            source_component: Component generating the event
            operation: Specific operation being performed
            input_data: Input data for the operation
            output_data: Output data from the operation
            metadata: Additional metadata
            session_id: Session identifier
            request_id: Request identifier
            **kwargs: Additional event properties
        
        Returns:
            Event ID for tracking
        """
        start_time = time.time()
        
        try:
            # Create event
            event = DataCollectionEvent(
                event_type=event_type,
                source_component=source_component,
                operation=operation,
                input_data=input_data or {},
                output_data=output_data or {},
                metadata=metadata or {},
                session_id=session_id,
                request_id=request_id,
                **kwargs
            )
            
            # Add performance metrics
            if PSUTIL_AVAILABLE:
                process = psutil.Process()
                memory_info = process.memory_info()
                event.memory_usage_mb = memory_info.rss / (1024 * 1024)
                event.cpu_utilization = process.cpu_percent()
            
            # Add to buffer
            async with self._buffer_lock:
                self._event_buffer.append(event)
                self.stats["events_collected"] += 1
                
                # Flush if buffer is full
                if len(self._event_buffer) >= self.batch_size:
                    await self._flush_buffer()
            
            processing_time = (time.time() - start_time) * 1000
            event.processing_time_ms = processing_time
            
            # Update average processing time
            current_avg = self.stats["avg_processing_time_ms"]
            total_events = self.stats["events_collected"]
            self.stats["avg_processing_time_ms"] = (
                (current_avg * (total_events - 1) + processing_time) / total_events
            )
            
            return event.event_id
            
        except Exception as e:
            self.stats["errors"] += 1
            logger.error(f"Failed to collect event: {e}")
            raise
    
    async def _periodic_flush(self):
        """Periodically flush buffered events to storage."""
        while self._is_running:
            try:
                await asyncio.sleep(self.flush_interval)
                await self._flush_buffer()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Periodic flush error: {e}")
    
    async def _flush_buffer(self):
        """Flush buffered events to persistent storage."""
        if not self._event_buffer:
            return
        
        async with self._buffer_lock:
            events_to_flush = list(self._event_buffer)
            self._event_buffer.clear()
        
        if not events_to_flush:
            return
        
        try:
            # Create timestamped filename
            timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
            filename = f"events_{timestamp}_{uuid.uuid4().hex[:8]}.json"
            filepath = self.storage_path / filename
            
            # Prepare data for storage
            events_data = {
                "metadata": {
                    "timestamp": datetime.now(timezone.utc).isoformat(),
                    "event_count": len(events_to_flush),
                    "collector_stats": self.stats.copy()
                },
                "events": [event.to_dict() for event in events_to_flush]
            }
            
            # Write to file
            with open(filepath, 'w') as f:
                json.dump(events_data, f, indent=None, separators=(',', ':'))
            
            # Update statistics
            self.stats["events_flushed"] += len(events_to_flush)
            self.stats["last_flush_time"] = datetime.now(timezone.utc).isoformat()
            
            logger.debug(f"Flushed {len(events_to_flush)} events to {filepath}")
            
        except Exception as e:
            self.stats["errors"] += 1
            logger.error(f"Failed to flush events: {e}")
            # Put events back in buffer for retry
            async with self._buffer_lock:
                self._event_buffer.extendleft(reversed(events_to_flush))
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get collector statistics."""
        return {
            **self.stats,
            "buffer_size": len(self._event_buffer),
            "is_running": self._is_running,
            "storage_path": str(self.storage_path)
        }


class DataQualityValidator:
    """
    Data quality validation and anomaly detection system.
    
    Provides real-time validation of collected data with configurable
    rules and statistical anomaly detection.
    """
    
    def __init__(self, rules_config_path: Optional[str] = None):
        """Initialize data quality validator."""
        self.rules: Dict[str, QualityValidationRule] = {}
        self.validation_history = defaultdict(deque)  # Rule ID -> validation results
        self.anomaly_baselines = defaultdict(list)  # Field -> historical values
        
        # Statistics
        self.stats = {
            "validations_performed": 0,
            "validation_failures": 0,
            "anomalies_detected": 0,
            "rules_active": 0
        }
        
        # Load default rules
        self._load_default_rules()
        
        # Load custom rules if provided
        if rules_config_path and Path(rules_config_path).exists():
            self._load_rules_from_config(rules_config_path)
        
        logger.info(f"Initialized data quality validator with {len(self.rules)} rules")
    
    def _load_default_rules(self):
        """Load default validation rules."""
        default_rules = [
            QualityValidationRule(
                rule_id="processing_time_range",
                name="Processing Time Range Check",
                description="Validates processing time is within acceptable range",
                rule_type="range_check",
                target_field="processing_time_ms",
                parameters={"min": 0.0, "max": 10000.0},
                severity="warning"
            ),
            QualityValidationRule(
                rule_id="memory_usage_check",
                name="Memory Usage Check",
                description="Validates memory usage is reasonable",
                rule_type="range_check",
                target_field="memory_usage_mb",
                parameters={"min": 0.0, "max": 16384.0},
                severity="warning"
            ),
            QualityValidationRule(
                rule_id="quality_score_range",
                name="Quality Score Range",
                description="Validates quality score is between 0 and 1",
                rule_type="range_check",
                target_field="quality_score",
                parameters={"min": 0.0, "max": 1.0},
                severity="error"
            ),
            QualityValidationRule(
                rule_id="event_type_format",
                name="Event Type Format",
                description="Validates event type follows naming convention",
                rule_type="format_check",
                target_field="event_type",
                parameters={"pattern": r"^[a-z][a-z0-9_]*$"},
                severity="info"
            ),
            QualityValidationRule(
                rule_id="processing_time_anomaly",
                name="Processing Time Anomaly Detection",
                description="Detects anomalous processing times",
                rule_type="anomaly_detection",
                target_field="processing_time_ms",
                parameters={"z_score_threshold": 3.0},
                severity="warning"
            )
        ]
        
        for rule in default_rules:
            self.rules[rule.rule_id] = rule
        
        self.stats["rules_active"] = len(self.rules)
    
    def _load_rules_from_config(self, config_path: str):
        """Load validation rules from configuration file."""
        try:
            with open(config_path, 'r') as f:
                config = json.load(f)
            
            for rule_data in config.get("validation_rules", []):
                rule = QualityValidationRule(**rule_data)
                self.rules[rule.rule_id] = rule
            
            self.stats["rules_active"] = len(self.rules)
            logger.info(f"Loaded {len(config.get('validation_rules', []))} rules from config")
            
        except Exception as e:
            logger.error(f"Failed to load rules config: {e}")
    
    async def validate_event(self, event: DataCollectionEvent) -> Dict[str, Any]:
        """
        Validate a data collection event against all active rules.
        
        Args:
            event: Event to validate
        
        Returns:
            Validation results dictionary
        """
        validation_results = {
            "event_id": event.event_id,
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "overall_passed": True,
            "rules_checked": 0,
            "rules_passed": 0,
            "rules_failed": 0,
            "validation_details": [],
            "anomalies_detected": [],
            "severity_counts": {"info": 0, "warning": 0, "error": 0, "critical": 0}
        }
        
        try:
            # Update anomaly baselines first
            self._update_anomaly_baselines(event)
            
            # Run all validation rules
            for rule_id, rule in self.rules.items():
                if not rule.enabled:
                    continue
                
                validation_results["rules_checked"] += 1
                
                # Get rule parameters with updated baselines for anomaly detection
                if rule.rule_type == "anomaly_detection":
                    rule.parameters["historical_values"] = self.anomaly_baselines.get(
                        rule.target_field, []
                    )
                
                # Validate against rule
                rule_result = rule.validate(event)
                validation_results["validation_details"].append(rule_result)
                
                # Update counters
                if rule_result["passed"]:
                    validation_results["rules_passed"] += 1
                else:
                    validation_results["rules_failed"] += 1
                    validation_results["overall_passed"] = False
                    
                    # Track anomalies separately
                    if rule.rule_type == "anomaly_detection":
                        validation_results["anomalies_detected"].append(rule_result)
                
                # Count by severity
                severity = rule_result["severity"]
                if severity in validation_results["severity_counts"]:
                    validation_results["severity_counts"][severity] += 1
                
                # Store validation history for this rule
                self.validation_history[rule_id].append({
                    "timestamp": datetime.now(timezone.utc).isoformat(),
                    "passed": rule_result["passed"],
                    "event_id": event.event_id
                })
                
                # Limit history size
                if len(self.validation_history[rule_id]) > 1000:
                    self.validation_history[rule_id].popleft()
            
            # Update statistics
            self.stats["validations_performed"] += 1
            if not validation_results["overall_passed"]:
                self.stats["validation_failures"] += 1
            if validation_results["anomalies_detected"]:
                self.stats["anomalies_detected"] += len(validation_results["anomalies_detected"])
            
        except Exception as e:
            logger.error(f"Validation error for event {event.event_id}: {e}")
            validation_results["overall_passed"] = False
            validation_results["validation_details"].append({
                "rule_id": "validation_system",
                "rule_name": "System Validation",
                "passed": False,
                "message": f"Validation system error: {str(e)}",
                "severity": "critical"
            })
        
        return validation_results
    
    def _update_anomaly_baselines(self, event: DataCollectionEvent):
        """Update historical baselines for anomaly detection."""
        # Extract numeric fields that could be used for anomaly detection
        numeric_fields = {
            "processing_time_ms": event.processing_time_ms,
            "memory_usage_mb": event.memory_usage_mb,
            "cpu_utilization": event.cpu_utilization,
            "quality_score": event.quality_score
        }
        
        for field, value in numeric_fields.items():
            if value is not None and isinstance(value, (int, float)):
                baseline = self.anomaly_baselines[field]
                baseline.append(value)
                
                # Keep only recent values for baseline (sliding window)
                max_baseline_size = 1000
                if len(baseline) > max_baseline_size:
                    baseline[:] = baseline[-max_baseline_size:]
    
    def get_validation_statistics(self) -> Dict[str, Any]:
        """Get validation statistics and rule performance."""
        rule_stats = {}
        for rule_id, history in self.validation_history.items():
            if history:
                passed_count = sum(1 for h in history if h["passed"])
                rule_stats[rule_id] = {
                    "total_validations": len(history),
                    "passed": passed_count,
                    "failed": len(history) - passed_count,
                    "success_rate": passed_count / len(history) if history else 0.0
                }
        
        return {
            "overall_stats": self.stats,
            "rule_performance": rule_stats,
            "active_rules": len([r for r in self.rules.values() if r.enabled]),
            "anomaly_baseline_sizes": {
                field: len(values) for field, values in self.anomaly_baselines.items()
            }
        }


class PrivacyComplianceManager:
    """
    Data retention and privacy compliance management system.
    
    Implements data governance policies, retention schedules,
    and privacy compliance mechanisms.
    """
    
    def __init__(
        self, 
        compliance_db_path: str = "/tmp/compliance.db",
        default_retention_days: int = 30
    ):
        """Initialize privacy compliance manager."""
        self.db_path = compliance_db_path
        self.default_retention_days = default_retention_days
        
        # Initialize database
        self._init_database()
        
        # Privacy policies
        self.retention_policies = {
            RetentionPolicy.SHORT_TERM: 7,
            RetentionPolicy.MEDIUM_TERM: 30,
            RetentionPolicy.LONG_TERM: 365,
            RetentionPolicy.PERMANENT: -1,  # Never delete
            RetentionPolicy.COMPLIANCE_REQUIRED: 2555  # 7 years
        }
        
        # Data classification handling
        self.classification_policies = {
            DataClassification.PUBLIC: {"encryption": False, "access_logging": False},
            DataClassification.INTERNAL: {"encryption": True, "access_logging": True},
            DataClassification.CONFIDENTIAL: {"encryption": True, "access_logging": True, "audit_trail": True},
            DataClassification.RESTRICTED: {"encryption": True, "access_logging": True, "audit_trail": True, "approval_required": True}
        }
        
        self.stats = {
            "events_tracked": 0,
            "retention_actions": 0,
            "privacy_violations": 0,
            "data_purged": 0
        }
        
        logger.info(f"Initialized privacy compliance manager with database: {self.db_path}")
    
    def _init_database(self):
        """Initialize compliance tracking database."""
        conn = sqlite3.connect(self.db_path)
        try:
            cursor = conn.cursor()
            
            # Create tables for compliance tracking
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS event_compliance (
                    event_id TEXT PRIMARY KEY,
                    timestamp TEXT NOT NULL,
                    classification TEXT NOT NULL,
                    retention_policy TEXT NOT NULL,
                    expiry_date TEXT,
                    contains_pii INTEGER DEFAULT 0,
                    privacy_tags TEXT,
                    data_location TEXT,
                    access_count INTEGER DEFAULT 0,
                    last_access TEXT,
                    purge_scheduled INTEGER DEFAULT 0,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS retention_actions (
                    action_id INTEGER PRIMARY KEY AUTOINCREMENT,
                    event_id TEXT NOT NULL,
                    action_type TEXT NOT NULL,
                    action_timestamp TEXT NOT NULL,
                    details TEXT,
                    FOREIGN KEY (event_id) REFERENCES event_compliance (event_id)
                )
            """)
            
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS privacy_audit_log (
                    audit_id INTEGER PRIMARY KEY AUTOINCREMENT,
                    event_id TEXT,
                    action TEXT NOT NULL,
                    user_id TEXT,
                    timestamp TEXT NOT NULL,
                    details TEXT,
                    ip_address TEXT
                )
            """)
            
            # Create indexes for performance
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_expiry_date ON event_compliance(expiry_date)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_classification ON event_compliance(classification)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_purge_scheduled ON event_compliance(purge_scheduled)")
            
            conn.commit()
            
        finally:
            conn.close()
    
    async def register_event(self, event: DataCollectionEvent, data_location: str = "") -> bool:
        """
        Register an event for privacy and compliance tracking.
        
        Args:
            event: Data collection event to register
            data_location: Storage location of the event data
        
        Returns:
            True if registration successful
        """
        try:
            # Calculate expiry date based on retention policy
            expiry_date = None
            retention_days = self.retention_policies.get(event.retention_policy)
            
            if retention_days and retention_days > 0:
                expiry_date = (event.timestamp + timedelta(days=retention_days)).isoformat()
            
            # Store compliance record
            conn = sqlite3.connect(self.db_path)
            try:
                cursor = conn.cursor()
                cursor.execute("""
                    INSERT OR REPLACE INTO event_compliance
                    (event_id, timestamp, classification, retention_policy, expiry_date,
                     contains_pii, privacy_tags, data_location, created_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    event.event_id,
                    event.timestamp.isoformat(),
                    event.classification.value,
                    event.retention_policy.value,
                    expiry_date,
                    1 if event.contains_pii else 0,
                    json.dumps(event.privacy_tags),
                    data_location,
                    datetime.now(timezone.utc).isoformat()
                ))
                
                conn.commit()
                self.stats["events_tracked"] += 1
                
            finally:
                conn.close()
            
            # Log audit entry
            await self._log_audit_action(
                event.event_id, 
                "event_registered", 
                details=f"Event registered with {event.classification.value} classification"
            )
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to register event for compliance: {e}")
            return False
    
    async def check_retention_compliance(self) -> Dict[str, Any]:
        """
        Check and enforce data retention compliance.
        
        Returns:
            Summary of retention actions taken
        """
        actions_taken = {
            "expired_events": [],
            "purge_scheduled": [],
            "compliance_violations": []
        }
        
        try:
            conn = sqlite3.connect(self.db_path)
            try:
                cursor = conn.cursor()
                
                # Find expired events
                current_time = datetime.now(timezone.utc).isoformat()
                cursor.execute("""
                    SELECT event_id, expiry_date, classification, data_location
                    FROM event_compliance
                    WHERE expiry_date IS NOT NULL 
                    AND expiry_date < ? 
                    AND purge_scheduled = 0
                """, (current_time,))
                
                expired_events = cursor.fetchall()
                
                for event_id, expiry_date, classification, data_location in expired_events:
                    # Schedule for purging
                    cursor.execute("""
                        UPDATE event_compliance 
                        SET purge_scheduled = 1 
                        WHERE event_id = ?
                    """, (event_id,))
                    
                    # Log retention action
                    cursor.execute("""
                        INSERT INTO retention_actions 
                        (event_id, action_type, action_timestamp, details)
                        VALUES (?, ?, ?, ?)
                    """, (
                        event_id,
                        "purge_scheduled",
                        current_time,
                        json.dumps({
                            "expiry_date": expiry_date,
                            "classification": classification,
                            "data_location": data_location
                        })
                    ))
                    
                    actions_taken["expired_events"].append({
                        "event_id": event_id,
                        "expiry_date": expiry_date,
                        "classification": classification
                    })
                    
                    self.stats["retention_actions"] += 1
                
                conn.commit()
                
            finally:
                conn.close()
            
            # Log audit for retention check
            await self._log_audit_action(
                None, 
                "retention_check",
                details=f"Processed {len(expired_events)} expired events"
            )
            
        except Exception as e:
            logger.error(f"Retention compliance check failed: {e}")
            actions_taken["compliance_violations"].append({
                "error": str(e),
                "timestamp": datetime.now(timezone.utc).isoformat()
            })
        
        return actions_taken
    
    async def _log_audit_action(
        self, 
        event_id: Optional[str], 
        action: str, 
        user_id: str = "system",
        details: str = "",
        ip_address: str = ""
    ):
        """Log privacy audit action."""
        try:
            conn = sqlite3.connect(self.db_path)
            try:
                cursor = conn.cursor()
                cursor.execute("""
                    INSERT INTO privacy_audit_log
                    (event_id, action, user_id, timestamp, details, ip_address)
                    VALUES (?, ?, ?, ?, ?, ?)
                """, (
                    event_id,
                    action,
                    user_id,
                    datetime.now(timezone.utc).isoformat(),
                    details,
                    ip_address
                ))
                conn.commit()
            finally:
                conn.close()
        except Exception as e:
            logger.error(f"Failed to log audit action: {e}")
    
    def get_compliance_report(self) -> Dict[str, Any]:
        """Generate comprehensive compliance report."""
        try:
            conn = sqlite3.connect(self.db_path)
            try:
                cursor = conn.cursor()
                
                # Event counts by classification
                cursor.execute("""
                    SELECT classification, COUNT(*) 
                    FROM event_compliance 
                    GROUP BY classification
                """)
                classification_counts = dict(cursor.fetchall())
                
                # Events requiring purge
                cursor.execute("""
                    SELECT COUNT(*) FROM event_compliance 
                    WHERE purge_scheduled = 1
                """)
                pending_purge = cursor.fetchone()[0]
                
                # PII data counts
                cursor.execute("""
                    SELECT COUNT(*) FROM event_compliance 
                    WHERE contains_pii = 1
                """)
                pii_events = cursor.fetchone()[0]
                
                # Recent audit actions
                cursor.execute("""
                    SELECT action, COUNT(*) FROM privacy_audit_log
                    WHERE timestamp > datetime('now', '-7 days')
                    GROUP BY action
                """)
                recent_audit_actions = dict(cursor.fetchall())
                
                return {
                    "overall_stats": self.stats,
                    "classification_distribution": classification_counts,
                    "pending_purge_count": pending_purge,
                    "pii_events_count": pii_events,
                    "recent_audit_actions": recent_audit_actions,
                    "retention_policies": {
                        policy.value: days for policy, days in self.retention_policies.items()
                    }
                }
                
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"Failed to generate compliance report: {e}")
            return {"error": str(e)}


class ProductionDataPipelineOrchestrator:
    """
    Main orchestrator for the production data pipeline.
    
    Coordinates data collection, quality validation, and privacy compliance
    for enterprise-grade MLOps data processing.
    """
    
    def __init__(
        self,
        storage_path: str = "/tmp/production_data_pipeline",
        config: Optional[Dict[str, Any]] = None
    ):
        """Initialize production data pipeline orchestrator."""
        self.storage_path = Path(storage_path)
        self.storage_path.mkdir(parents=True, exist_ok=True)
        
        self.config = config or {}
        
        # Initialize components
        collector_config = self.config.get("collector", {})
        self.data_collector = ProductionDataCollector(
            storage_path=str(self.storage_path / "events"),
            **collector_config
        )
        
        validator_config = self.config.get("validator", {})
        self.quality_validator = DataQualityValidator(
            rules_config_path=validator_config.get("rules_config_path")
        )
        
        compliance_config = self.config.get("compliance", {})
        self.compliance_manager = PrivacyComplianceManager(
            compliance_db_path=str(self.storage_path / "compliance.db"),
            **compliance_config
        )
        
        # Pipeline statistics
        self.pipeline_stats = {
            "started_at": None,
            "events_processed": 0,
            "validation_failures": 0,
            "compliance_violations": 0,
            "is_healthy": True,
            "last_health_check": None
        }
        
        # Background tasks
        self._monitoring_task = None
        self._compliance_task = None
        self._is_running = False
        
        logger.info(f"Initialized production data pipeline with storage: {self.storage_path}")
    
    async def start(self):
        """Start the production data pipeline."""
        if self._is_running:
            return
        
        self._is_running = True
        self.pipeline_stats["started_at"] = datetime.now(timezone.utc).isoformat()
        
        # Start components
        await self.data_collector.start()
        
        # Start background tasks
        self._monitoring_task = asyncio.create_task(self._health_monitoring())
        self._compliance_task = asyncio.create_task(self._compliance_monitoring())
        
        logger.info("Production data pipeline started successfully")
    
    async def stop(self):
        """Stop the production data pipeline."""
        if not self._is_running:
            return
        
        self._is_running = False
        
        # Stop background tasks
        for task in [self._monitoring_task, self._compliance_task]:
            if task:
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        
        # Stop components
        await self.data_collector.stop()
        
        logger.info("Production data pipeline stopped")
    
    async def process_server_operation(
        self,
        operation_type: str,
        component: str,
        operation_name: str,
        input_data: Optional[Dict[str, Any]] = None,
        output_data: Optional[Dict[str, Any]] = None,
        metadata: Optional[Dict[str, Any]] = None,
        session_id: str = "",
        request_id: str = "",
        classification: DataClassification = DataClassification.INTERNAL,
        retention_policy: RetentionPolicy = RetentionPolicy.MEDIUM_TERM,
        contains_pii: bool = False
    ) -> Dict[str, Any]:
        """
        Process a server operation through the complete data pipeline.
        
        Args:
            operation_type: Type of operation (e.g., "inference", "training")
            component: Source component name
            operation_name: Specific operation name
            input_data: Input data for the operation
            output_data: Output data from the operation
            metadata: Additional metadata
            session_id: Session identifier
            request_id: Request identifier
            classification: Data classification level
            retention_policy: Data retention policy
            contains_pii: Whether data contains PII
        
        Returns:
            Processing result with event ID and validation status
        """
        start_time = time.time()
        result = {
            "success": True,
            "event_id": None,
            "validation_passed": True,
            "compliance_registered": True,
            "processing_time_ms": 0.0,
            "errors": []
        }
        
        try:
            # 1. Collect the event
            event_id = await self.data_collector.collect_event(
                event_type=operation_type,
                source_component=component,
                operation=operation_name,
                input_data=input_data,
                output_data=output_data,
                metadata=metadata,
                session_id=session_id,
                request_id=request_id,
                classification=classification,
                retention_policy=retention_policy,
                contains_pii=contains_pii
            )
            result["event_id"] = event_id
            
            # 2. Create full event for validation (reconstruct from collected data)
            event = DataCollectionEvent(
                event_id=event_id,
                event_type=operation_type,
                source_component=component,
                operation=operation_name,
                input_data=input_data or {},
                output_data=output_data or {},
                metadata=metadata or {},
                session_id=session_id,
                request_id=request_id,
                classification=classification,
                retention_policy=retention_policy,
                contains_pii=contains_pii
            )
            
            # 3. Validate data quality
            validation_result = await self.quality_validator.validate_event(event)
            result["validation_passed"] = validation_result["overall_passed"]
            result["validation_details"] = validation_result
            
            if not validation_result["overall_passed"]:
                self.pipeline_stats["validation_failures"] += 1
            
            # 4. Register for compliance tracking
            data_location = str(self.storage_path / "events")
            compliance_registered = await self.compliance_manager.register_event(
                event, data_location
            )
            result["compliance_registered"] = compliance_registered
            
            if not compliance_registered:
                self.pipeline_stats["compliance_violations"] += 1
                result["errors"].append("Failed to register for compliance tracking")
            
            # 5. Update pipeline statistics
            self.pipeline_stats["events_processed"] += 1
            
        except Exception as e:
            result["success"] = False
            result["errors"].append(str(e))
            logger.error(f"Pipeline processing error: {e}")
        
        finally:
            result["processing_time_ms"] = (time.time() - start_time) * 1000
        
        return result
    
    async def _health_monitoring(self):
        """Background task for pipeline health monitoring."""
        while self._is_running:
            try:
                # Check component health
                collector_stats = self.data_collector.get_statistics()
                validator_stats = self.quality_validator.get_validation_statistics()
                
                # Simple health checks
                is_healthy = True
                health_issues = []
                
                # Check if collector is collecting events
                if collector_stats["events_collected"] == 0 and self.pipeline_stats["events_processed"] > 0:
                    is_healthy = False
                    health_issues.append("Data collector not receiving events")
                
                # Check validation failure rate
                validation_failure_rate = (
                    self.pipeline_stats["validation_failures"] / 
                    max(self.pipeline_stats["events_processed"], 1)
                )
                if validation_failure_rate > 0.1:  # More than 10% failures
                    is_healthy = False
                    health_issues.append(f"High validation failure rate: {validation_failure_rate:.2%}")
                
                # Update health status
                self.pipeline_stats["is_healthy"] = is_healthy
                self.pipeline_stats["last_health_check"] = datetime.now(timezone.utc).isoformat()
                
                if health_issues:
                    logger.warning(f"Pipeline health issues detected: {health_issues}")
                
                await asyncio.sleep(60)  # Check every minute
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Health monitoring error: {e}")
                await asyncio.sleep(60)
    
    async def _compliance_monitoring(self):
        """Background task for compliance monitoring."""
        while self._is_running:
            try:
                # Run retention compliance check
                retention_actions = await self.compliance_manager.check_retention_compliance()
                
                if retention_actions["expired_events"]:
                    logger.info(f"Processed {len(retention_actions['expired_events'])} expired events")
                
                if retention_actions["compliance_violations"]:
                    logger.error(f"Compliance violations detected: {retention_actions['compliance_violations']}")
                
                await asyncio.sleep(3600)  # Check every hour
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Compliance monitoring error: {e}")
                await asyncio.sleep(3600)
    
    def get_comprehensive_status(self) -> Dict[str, Any]:
        """Get comprehensive status of the production data pipeline."""
        return {
            "pipeline_stats": self.pipeline_stats,
            "data_collector": self.data_collector.get_statistics(),
            "quality_validator": self.quality_validator.get_validation_statistics(),
            "compliance_manager": self.compliance_manager.get_compliance_report(),
            "storage_info": {
                "storage_path": str(self.storage_path),
                "storage_exists": self.storage_path.exists()
            },
            "is_running": self._is_running
        }


# Factory function for easy pipeline creation
async def create_production_data_pipeline(
    storage_path: str = "/tmp/production_data_pipeline",
    config: Optional[Dict[str, Any]] = None
) -> ProductionDataPipelineOrchestrator:
    """
    Factory function to create and start a production data pipeline.
    
    Args:
        storage_path: Base storage path for pipeline data
        config: Configuration dictionary for pipeline components
    
    Returns:
        Started ProductionDataPipelineOrchestrator instance
    """
    pipeline = ProductionDataPipelineOrchestrator(storage_path, config)
    await pipeline.start()
    return pipeline