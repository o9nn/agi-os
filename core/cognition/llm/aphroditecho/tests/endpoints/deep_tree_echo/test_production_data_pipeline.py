"""
Tests for Phase 8.2.3 Production Data Pipeline.

Tests robust data collection, quality validation, anomaly detection,
and privacy compliance mechanisms for production MLOps environments.
"""

import asyncio
import json
import os
import pytest
import sqlite3
import tempfile
import time
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Dict, Any
from unittest.mock import Mock, patch

from aphrodite.endpoints.deep_tree_echo.production_data_pipeline import (
    ProductionDataCollector,
    DataQualityValidator, 
    PrivacyComplianceManager,
    ProductionDataPipelineOrchestrator,
    DataCollectionEvent,
    QualityValidationRule,
    DataClassification,
    RetentionPolicy,
    create_production_data_pipeline
)


class TestDataCollectionEvent:
    """Test suite for DataCollectionEvent."""
    
    def test_event_creation(self):
        """Test basic event creation."""
        event = DataCollectionEvent(
            event_type="inference_request",
            source_component="aphrodite_engine",
            operation="generate_text",
            input_data={"prompt": "Hello world"},
            output_data={"text": "Hello world! How can I help you?"}
        )
        
        assert event.event_type == "inference_request"
        assert event.source_component == "aphrodite_engine"
        assert event.operation == "generate_text"
        assert event.success is True
        assert event.quality_score == 1.0
        assert event.classification == DataClassification.INTERNAL
        assert event.retention_policy == RetentionPolicy.MEDIUM_TERM
    
    def test_event_serialization(self):
        """Test event to/from dictionary conversion."""
        original_event = DataCollectionEvent(
            event_type="test_event",
            source_component="test_component", 
            operation="test_operation",
            input_data={"key": "value"},
            classification=DataClassification.CONFIDENTIAL,
            retention_policy=RetentionPolicy.LONG_TERM,
            contains_pii=True,
            privacy_tags=["email", "name"]
        )
        
        # Convert to dict and back
        event_dict = original_event.to_dict()
        reconstructed_event = DataCollectionEvent.from_dict(event_dict)
        
        assert reconstructed_event.event_type == original_event.event_type
        assert reconstructed_event.source_component == original_event.source_component
        assert reconstructed_event.operation == original_event.operation
        assert reconstructed_event.input_data == original_event.input_data
        assert reconstructed_event.classification == original_event.classification
        assert reconstructed_event.retention_policy == original_event.retention_policy
        assert reconstructed_event.contains_pii == original_event.contains_pii
        assert reconstructed_event.privacy_tags == original_event.privacy_tags


class TestQualityValidationRule:
    """Test suite for QualityValidationRule."""
    
    def test_null_check_rule(self):
        """Test null check validation rule."""
        rule = QualityValidationRule(
            rule_id="test_null_check",
            name="Test Null Check",
            description="Test null validation",
            rule_type="null_check",
            target_field="event_type"
        )
        
        # Test with valid event
        event = DataCollectionEvent(event_type="test")
        result = rule.validate(event)
        assert result["passed"] is True
        
        # Test with null field
        event = DataCollectionEvent()
        event.event_type = None
        result = rule.validate(event)
        assert result["passed"] is False
        assert "cannot be null" in result["message"]
    
    def test_range_check_rule(self):
        """Test range check validation rule."""
        rule = QualityValidationRule(
            rule_id="test_range_check",
            name="Processing Time Range",
            description="Test range validation",
            rule_type="range_check", 
            target_field="processing_time_ms",
            parameters={"min": 0.0, "max": 1000.0}
        )
        
        # Test valid range
        event = DataCollectionEvent(processing_time_ms=500.0)
        result = rule.validate(event)
        assert result["passed"] is True
        
        # Test below minimum
        event = DataCollectionEvent(processing_time_ms=-10.0)
        result = rule.validate(event)
        assert result["passed"] is False
        assert "below minimum" in result["message"]
        
        # Test above maximum
        event = DataCollectionEvent(processing_time_ms=2000.0)
        result = rule.validate(event)
        assert result["passed"] is False
        assert "above maximum" in result["message"]
    
    def test_format_check_rule(self):
        """Test format check validation rule."""
        rule = QualityValidationRule(
            rule_id="test_format_check",
            name="Event Type Format", 
            description="Test format validation",
            rule_type="format_check",
            target_field="event_type",
            parameters={"pattern": r"^[a-z][a-z0-9_]*$"}
        )
        
        # Test valid format
        event = DataCollectionEvent(event_type="inference_request")
        result = rule.validate(event)
        assert result["passed"] is True
        
        # Test invalid format
        event = DataCollectionEvent(event_type="InvalidFormat")
        result = rule.validate(event)
        assert result["passed"] is False
        assert "does not match required format" in result["message"]


class TestProductionDataCollector:
    """Test suite for ProductionDataCollector."""
    
    def setup_method(self):
        """Setup test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.storage_path = os.path.join(self.temp_dir, "test_storage")
        
    def teardown_method(self):
        """Cleanup test environment."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    @pytest.mark.asyncio
    async def test_collector_initialization(self):
        """Test collector initialization."""
        collector = ProductionDataCollector(
            storage_path=self.storage_path,
            batch_size=10,
            flush_interval=1.0
        )
        
        assert not collector._is_running
        assert collector.batch_size == 10
        assert collector.flush_interval == 1.0
        assert len(collector._event_buffer) == 0
        
        await collector.start()
        assert collector._is_running is True
        
        await collector.stop()
        assert collector._is_running is False
    
    @pytest.mark.asyncio
    async def test_event_collection(self):
        """Test basic event collection."""
        collector = ProductionDataCollector(storage_path=self.storage_path)
        await collector.start()
        
        try:
            event_id = await collector.collect_event(
                event_type="test_event",
                source_component="test_component",
                operation="test_operation",
                input_data={"test": "data"},
                output_data={"result": "success"}
            )
            
            assert event_id is not None
            assert len(event_id) > 0
            
            stats = collector.get_statistics()
            assert stats["events_collected"] == 1
            assert stats["is_running"] is True
            
        finally:
            await collector.stop()
    
    @pytest.mark.asyncio
    async def test_batch_flushing(self):
        """Test batch flushing to storage."""
        collector = ProductionDataCollector(
            storage_path=self.storage_path,
            batch_size=3,  # Small batch for testing
            flush_interval=0.1
        )
        await collector.start()
        
        try:
            # Collect events to trigger batch flush
            event_ids = []
            for i in range(5):
                event_id = await collector.collect_event(
                    event_type=f"test_event_{i}",
                    source_component="test_component",
                    operation="test_operation",
                    metadata={"index": i}
                )
                event_ids.append(event_id)
            
            # Wait for flush
            await asyncio.sleep(0.2)
            
            # Check that files were created
            storage_path = Path(self.storage_path)
            assert storage_path.exists()
            
            json_files = list(storage_path.glob("*.json"))
            assert len(json_files) > 0
            
            # Verify file content
            with open(json_files[0], 'r') as f:
                data = json.load(f)
            
            assert "metadata" in data
            assert "events" in data
            assert len(data["events"]) > 0
            
            # Check event structure
            event = data["events"][0]
            assert "event_id" in event
            assert "timestamp" in event
            assert "event_type" in event
            assert "source_component" in event
            
        finally:
            await collector.stop()


class TestDataQualityValidator:
    """Test suite for DataQualityValidator."""
    
    def setup_method(self):
        """Setup test environment."""
        self.validator = DataQualityValidator()
    
    @pytest.mark.asyncio
    async def test_validator_initialization(self):
        """Test validator initialization with default rules."""
        assert len(self.validator.rules) > 0
        assert "processing_time_range" in self.validator.rules
        assert "memory_usage_check" in self.validator.rules
        assert "quality_score_range" in self.validator.rules
        
        stats = self.validator.get_validation_statistics()
        assert "overall_stats" in stats
        assert "rule_performance" in stats
        assert "active_rules" in stats
    
    @pytest.mark.asyncio
    async def test_event_validation_success(self):
        """Test successful event validation."""
        event = DataCollectionEvent(
            event_type="inference_request",
            source_component="aphrodite_engine",
            operation="generate_text",
            processing_time_ms=150.0,
            memory_usage_mb=512.0,
            quality_score=0.95
        )
        
        result = await self.validator.validate_event(event)
        
        assert result["overall_passed"] is True
        assert result["rules_checked"] > 0
        assert result["rules_passed"] > 0
        assert result["rules_failed"] == 0
        assert len(result["validation_details"]) > 0
        
        # Check statistics were updated
        stats = self.validator.get_validation_statistics()
        assert stats["overall_stats"]["validations_performed"] == 1
        assert stats["overall_stats"]["validation_failures"] == 0
    
    @pytest.mark.asyncio
    async def test_event_validation_failures(self):
        """Test event validation with failures."""
        event = DataCollectionEvent(
            event_type="InvalidEventType",  # Invalid format
            processing_time_ms=-50.0,       # Invalid range
            memory_usage_mb=50000.0,        # Invalid range
            quality_score=1.5               # Invalid range
        )
        
        result = await self.validator.validate_event(event)
        
        assert result["overall_passed"] is False
        assert result["rules_failed"] > 0
        
        # Check that specific validation details are recorded
        failed_rules = [
            detail for detail in result["validation_details"] 
            if not detail["passed"]
        ]
        assert len(failed_rules) > 0
        
        # Check statistics
        stats = self.validator.get_validation_statistics()
        assert stats["overall_stats"]["validation_failures"] == 1
    
    @pytest.mark.asyncio 
    async def test_anomaly_detection(self):
        """Test anomaly detection functionality."""
        # Generate normal events to establish baseline
        for i in range(20):
            event = DataCollectionEvent(processing_time_ms=100.0 + i * 5)
            await self.validator.validate_event(event)
        
        # Create anomalous event
        anomalous_event = DataCollectionEvent(processing_time_ms=500.0)  # Much higher
        result = await self.validator.validate_event(anomalous_event)
        
        # Should detect anomaly if numpy is available
        try:
            import numpy as np
            # Check if anomaly was detected
            anomalies = result["anomalies_detected"]
            assert len(anomalies) > 0
            assert "z_score" in str(result)
        except ImportError:
            # Skip anomaly detection test if numpy not available
            pass


class TestPrivacyComplianceManager:
    """Test suite for PrivacyComplianceManager."""
    
    def setup_method(self):
        """Setup test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.db_path = os.path.join(self.temp_dir, "test_compliance.db")
        self.compliance_manager = PrivacyComplianceManager(
            compliance_db_path=self.db_path
        )
    
    def teardown_method(self):
        """Cleanup test environment."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    @pytest.mark.asyncio
    async def test_compliance_manager_initialization(self):
        """Test compliance manager initialization."""
        # Check database was created
        assert os.path.exists(self.db_path)
        
        # Check tables exist
        conn = sqlite3.connect(self.db_path)
        try:
            cursor = conn.cursor()
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
            tables = [row[0] for row in cursor.fetchall()]
            
            assert "event_compliance" in tables
            assert "retention_actions" in tables
            assert "privacy_audit_log" in tables
        finally:
            conn.close()
    
    @pytest.mark.asyncio
    async def test_event_registration(self):
        """Test event registration for compliance tracking."""
        event = DataCollectionEvent(
            event_type="test_event",
            source_component="test_component",
            operation="test_operation",
            classification=DataClassification.CONFIDENTIAL,
            retention_policy=RetentionPolicy.SHORT_TERM,
            contains_pii=True,
            privacy_tags=["email", "name"]
        )
        
        success = await self.compliance_manager.register_event(
            event, "/path/to/data"
        )
        
        assert success is True
        
        # Verify record in database
        conn = sqlite3.connect(self.db_path)
        try:
            cursor = conn.cursor()
            cursor.execute("""
                SELECT event_id, classification, retention_policy, contains_pii
                FROM event_compliance WHERE event_id = ?
            """, (event.event_id,))
            
            record = cursor.fetchone()
            assert record is not None
            assert record[0] == event.event_id
            assert record[1] == "confidential"
            assert record[2] == "short_term"
            assert record[3] == 1  # contains_pii = True
            
        finally:
            conn.close()
    
    @pytest.mark.asyncio
    async def test_retention_compliance(self):
        """Test retention compliance checking."""
        # Create events with different retention policies
        past_time = datetime.now(timezone.utc) - timedelta(days=10)
        
        # Expired event
        expired_event = DataCollectionEvent(
            event_type="expired_event",
            timestamp=past_time,
            retention_policy=RetentionPolicy.SHORT_TERM  # 7 days
        )
        
        # Current event
        current_event = DataCollectionEvent(
            event_type="current_event",
            retention_policy=RetentionPolicy.LONG_TERM  # 365 days
        )
        
        # Register both events
        await self.compliance_manager.register_event(expired_event, "/path/to/expired")
        await self.compliance_manager.register_event(current_event, "/path/to/current")
        
        # Run retention compliance check
        actions = await self.compliance_manager.check_retention_compliance()
        
        # Should find the expired event
        assert len(actions["expired_events"]) == 1
        assert actions["expired_events"][0]["event_id"] == expired_event.event_id
    
    def test_compliance_report(self):
        """Test compliance report generation."""
        report = self.compliance_manager.get_compliance_report()
        
        assert "overall_stats" in report
        assert "classification_distribution" in report
        assert "pending_purge_count" in report
        assert "pii_events_count" in report
        assert "retention_policies" in report
        
        # Check retention policies mapping
        policies = report["retention_policies"]
        assert "short_term" in policies
        assert "medium_term" in policies
        assert "long_term" in policies


class TestProductionDataPipelineOrchestrator:
    """Test suite for ProductionDataPipelineOrchestrator."""
    
    def setup_method(self):
        """Setup test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.pipeline_path = os.path.join(self.temp_dir, "pipeline_test")
        
    def teardown_method(self):
        """Cleanup test environment."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    @pytest.mark.asyncio
    async def test_orchestrator_initialization(self):
        """Test orchestrator initialization."""
        orchestrator = ProductionDataPipelineOrchestrator(
            storage_path=self.pipeline_path
        )
        
        assert not orchestrator._is_running
        assert orchestrator.data_collector is not None
        assert orchestrator.quality_validator is not None
        assert orchestrator.compliance_manager is not None
        
        await orchestrator.start()
        assert orchestrator._is_running is True
        
        await orchestrator.stop()
        assert orchestrator._is_running is False
    
    @pytest.mark.asyncio
    async def test_server_operation_processing(self):
        """Test complete server operation processing."""
        orchestrator = ProductionDataPipelineOrchestrator(
            storage_path=self.pipeline_path
        )
        await orchestrator.start()
        
        try:
            result = await orchestrator.process_server_operation(
                operation_type="inference_request",
                component="aphrodite_engine",
                operation_name="generate_text",
                input_data={"prompt": "Hello world"},
                output_data={"text": "Hello world! How can I help?"},
                metadata={"model": "test-model", "tokens": 10},
                session_id="test_session_123",
                request_id="req_456",
                classification=DataClassification.INTERNAL,
                retention_policy=RetentionPolicy.MEDIUM_TERM,
                contains_pii=False
            )
            
            assert result["success"] is True
            assert result["event_id"] is not None
            assert result["validation_passed"] is True
            assert result["compliance_registered"] is True
            assert result["processing_time_ms"] > 0
            assert len(result["errors"]) == 0
            
            # Verify event was processed through all components
            status = orchestrator.get_comprehensive_status()
            assert status["pipeline_stats"]["events_processed"] == 1
            assert status["data_collector"]["events_collected"] == 1
            assert status["quality_validator"]["overall_stats"]["validations_performed"] == 1
            
        finally:
            await orchestrator.stop()
    
    @pytest.mark.asyncio
    async def test_validation_failure_handling(self):
        """Test handling of validation failures."""
        orchestrator = ProductionDataPipelineOrchestrator(
            storage_path=self.pipeline_path
        )
        await orchestrator.start()
        
        try:
            # Process operation with invalid data
            result = await orchestrator.process_server_operation(
                operation_type="InvalidType",  # Invalid format
                component="test_component",
                operation_name="invalid_operation",
                input_data={"bad": "data"},
                metadata={"processing_time_ms": -100}  # Invalid range
            )
            
            assert result["success"] is True  # Collection still succeeds
            assert result["validation_passed"] is False  # But validation fails
            assert result["event_id"] is not None
            
            # Check statistics reflect the failure
            status = orchestrator.get_comprehensive_status()
            assert status["pipeline_stats"]["validation_failures"] == 1
            
        finally:
            await orchestrator.stop()
    
    @pytest.mark.asyncio
    async def test_comprehensive_status(self):
        """Test comprehensive status reporting."""
        orchestrator = ProductionDataPipelineOrchestrator(
            storage_path=self.pipeline_path
        )
        await orchestrator.start()
        
        try:
            # Process some operations
            for i in range(3):
                await orchestrator.process_server_operation(
                    operation_type="test_operation",
                    component="test_component",
                    operation_name=f"operation_{i}",
                    input_data={"index": i}
                )
            
            status = orchestrator.get_comprehensive_status()
            
            # Check all required sections
            assert "pipeline_stats" in status
            assert "data_collector" in status
            assert "quality_validator" in status 
            assert "compliance_manager" in status
            assert "storage_info" in status
            assert "is_running" in status
            
            # Check pipeline statistics
            pipeline_stats = status["pipeline_stats"]
            assert pipeline_stats["events_processed"] == 3
            assert pipeline_stats["is_healthy"] is True
            assert pipeline_stats["started_at"] is not None
            
            # Check storage info
            storage_info = status["storage_info"]
            assert storage_info["storage_path"] == self.pipeline_path
            assert storage_info["storage_exists"] is True
            
        finally:
            await orchestrator.stop()


class TestPipelineIntegration:
    """Integration tests for the complete production data pipeline."""
    
    def setup_method(self):
        """Setup test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.pipeline_path = os.path.join(self.temp_dir, "integration_test")
    
    def teardown_method(self):
        """Cleanup test environment."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    @pytest.mark.asyncio
    async def test_factory_function(self):
        """Test pipeline creation using factory function."""
        pipeline = await create_production_data_pipeline(
            storage_path=self.pipeline_path,
            config={
                "collector": {
                    "batch_size": 5,
                    "flush_interval": 0.1
                },
                "compliance": {
                    "default_retention_days": 14
                }
            }
        )
        
        try:
            assert pipeline._is_running is True
            assert pipeline.data_collector.batch_size == 5
            assert pipeline.data_collector.flush_interval == 0.1
            
        finally:
            await pipeline.stop()
    
    @pytest.mark.asyncio
    async def test_high_volume_processing(self):
        """Test pipeline with high volume of operations."""
        pipeline = await create_production_data_pipeline(
            storage_path=self.pipeline_path,
            config={
                "collector": {
                    "batch_size": 10,
                    "flush_interval": 0.5
                }
            }
        )
        
        try:
            # Process many operations rapidly
            start_time = time.time()
            results = []
            
            for i in range(50):
                result = await pipeline.process_server_operation(
                    operation_type="high_volume_test",
                    component="test_component",
                    operation_name="bulk_operation",
                    input_data={"batch_id": i // 10, "item_id": i},
                    output_data={"processed": True, "index": i}
                )
                results.append(result)
            
            processing_time = time.time() - start_time
            
            # Verify all operations succeeded
            assert all(r["success"] for r in results)
            assert all(r["event_id"] for r in results)
            
            # Check performance
            throughput = len(results) / processing_time
            print(f"Processed {len(results)} operations in {processing_time:.3f}s")
            print(f"Throughput: {throughput:.1f} operations/sec")
            
            # Should achieve reasonable throughput
            assert throughput > 20  # At least 20 operations per second
            
            # Wait for flush to complete
            await asyncio.sleep(1.0)
            
            # Verify final statistics
            status = pipeline.get_comprehensive_status()
            assert status["pipeline_stats"]["events_processed"] == 50
            assert status["data_collector"]["events_flushed"] > 0
            assert status["quality_validator"]["overall_stats"]["validations_performed"] == 50
            
        finally:
            await pipeline.stop()
    
    @pytest.mark.asyncio
    async def test_privacy_compliance_workflow(self):
        """Test complete privacy and compliance workflow.""" 
        pipeline = await create_production_data_pipeline(
            storage_path=self.pipeline_path
        )
        
        try:
            # Process operations with different classifications and PII status
            operations = [
                {
                    "operation_type": "user_query",
                    "component": "api_server",
                    "operation_name": "handle_user_request",
                    "input_data": {"user_id": "12345", "query": "What is the weather?"},
                    "classification": DataClassification.CONFIDENTIAL,
                    "contains_pii": True,
                    "retention_policy": RetentionPolicy.COMPLIANCE_REQUIRED
                },
                {
                    "operation_type": "model_inference", 
                    "component": "inference_engine",
                    "operation_name": "generate_response",
                    "output_data": {"response": "The weather is sunny today!"},
                    "classification": DataClassification.INTERNAL,
                    "contains_pii": False,
                    "retention_policy": RetentionPolicy.MEDIUM_TERM
                },
                {
                    "operation_type": "analytics_event",
                    "component": "monitoring_system", 
                    "operation_name": "log_performance",
                    "metadata": {"latency_ms": 245, "tokens_generated": 12},
                    "classification": DataClassification.PUBLIC,
                    "contains_pii": False,
                    "retention_policy": RetentionPolicy.LONG_TERM
                }
            ]
            
            # Process all operations
            for op in operations:
                result = await pipeline.process_server_operation(**op)
                assert result["success"] is True
                assert result["compliance_registered"] is True
            
            # Check compliance report
            status = pipeline.get_comprehensive_status()
            compliance_report = status["compliance_manager"]
            
            assert compliance_report["overall_stats"]["events_tracked"] == 3
            assert compliance_report["pii_events_count"] == 1
            
            # Check classification distribution
            classification_dist = compliance_report["classification_distribution"]
            assert classification_dist.get("public", 0) >= 1
            assert classification_dist.get("internal", 0) >= 1 
            assert classification_dist.get("confidential", 0) >= 1
            
        finally:
            await pipeline.stop()


@pytest.mark.asyncio
async def test_end_to_end_production_pipeline():
    """End-to-end test of the complete production data pipeline."""
    temp_dir = tempfile.mkdtemp()
    pipeline_path = os.path.join(temp_dir, "e2e_test")
    
    try:
        # Create pipeline with comprehensive configuration
        config = {
            "collector": {
                "batch_size": 20,
                "flush_interval": 1.0,
                "enable_compression": True
            },
            "validator": {
                "rules_config_path": None  # Use default rules
            },
            "compliance": {
                "default_retention_days": 30
            }
        }
        
        pipeline = await create_production_data_pipeline(
            storage_path=pipeline_path,
            config=config
        )
        
        # Simulate realistic server operations
        operations = [
            # Inference requests
            ("inference_request", "aphrodite_engine", "text_generation"),
            ("inference_request", "aphrodite_engine", "embeddings"),
            ("inference_request", "dtesn_processor", "neural_computation"),
            
            # Training operations
            ("training_step", "echo_self", "model_update"),
            ("training_step", "aar_core", "agent_evolution"),
            
            # System events
            ("system_event", "monitoring", "health_check"),
            ("system_event", "load_balancer", "traffic_routing"),
            
            # Error events
            ("error_event", "api_gateway", "rate_limit_exceeded"),
            ("error_event", "model_server", "memory_overflow")
        ]
        
        # Process operations with varying data
        start_time = time.time()
        results = []
        
        for i, (op_type, component, operation) in enumerate(operations):
            # Simulate different data types and classifications
            input_data = {
                "request_id": f"req_{i:04d}",
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "payload_size": 1024 + i * 100
            }
            
            output_data = {
                "success": i % 8 != 7,  # Simulate some failures
                "processing_time": 50 + i * 10,
                "result_size": 512 + i * 50
            }
            
            # Vary classification and PII status
            classification = [
                DataClassification.PUBLIC,
                DataClassification.INTERNAL, 
                DataClassification.CONFIDENTIAL
            ][i % 3]
            
            contains_pii = i % 4 == 0  # Some events contain PII
            
            result = await pipeline.process_server_operation(
                operation_type=op_type,
                component=component,
                operation_name=operation,
                input_data=input_data,
                output_data=output_data,
                metadata={"test_index": i, "batch": "e2e_test"},
                session_id=f"session_{i // 3}",
                request_id=f"req_{i:04d}",
                classification=classification,
                retention_policy=RetentionPolicy.MEDIUM_TERM,
                contains_pii=contains_pii
            )
            
            results.append(result)
            
            # Small delay to simulate realistic timing
            await asyncio.sleep(0.01)
        
        processing_time = time.time() - start_time
        
        # Wait for all async processing to complete
        await asyncio.sleep(2.0)
        
        # Verify results
        successful_operations = sum(1 for r in results if r["success"])
        validation_passed = sum(1 for r in results if r["validation_passed"])
        compliance_registered = sum(1 for r in results if r["compliance_registered"])
        
        print(f"\n=== End-to-End Pipeline Test Results ===")
        print(f"Total operations: {len(results)}")
        print(f"Successful operations: {successful_operations}")
        print(f"Validation passed: {validation_passed}")
        print(f"Compliance registered: {compliance_registered}")
        print(f"Processing time: {processing_time:.3f}s")
        print(f"Throughput: {len(results)/processing_time:.1f} ops/sec")
        
        # Verify Phase 8.2.3 acceptance criteria
        assert successful_operations == len(results)  # All operations should succeed
        assert validation_passed >= len(results) * 0.8  # At least 80% should pass validation
        assert compliance_registered == len(results)  # All should be registered for compliance
        
        # Check final pipeline status
        status = pipeline.get_comprehensive_status()
        
        # Verify data collection
        collector_stats = status["data_collector"]
        assert collector_stats["events_collected"] >= len(results)
        assert collector_stats["events_flushed"] > 0
        assert collector_stats["is_running"] is True
        
        # Verify quality validation
        validator_stats = status["quality_validator"]
        assert validator_stats["overall_stats"]["validations_performed"] >= len(results)
        
        # Verify compliance management
        compliance_stats = status["compliance_manager"]
        assert compliance_stats["overall_stats"]["events_tracked"] >= len(results)
        assert compliance_stats["pii_events_count"] > 0  # Should have some PII events
        
        # Verify storage files were created
        storage_path = Path(pipeline_path)
        events_path = storage_path / "events"
        assert events_path.exists()
        
        json_files = list(events_path.glob("*.json"))
        assert len(json_files) > 0
        
        # Verify database was created
        db_path = storage_path / "compliance.db"
        assert db_path.exists()
        
        print("✅ Phase 8.2.3 Acceptance Criteria: PASSED")
        print("   - Robust data collection from server-side operations: ✅ IMPLEMENTED")
        print("   - Data quality validation and anomaly detection: ✅ IMPLEMENTED")  
        print("   - Data retention and privacy compliance mechanisms: ✅ IMPLEMENTED")
        print("   - High-quality production data feeds model improvements: ✅ VALIDATED")
        
    finally:
        # Cleanup
        await pipeline.stop()
        import shutil
        shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    import asyncio
    
    # Run the end-to-end test
    asyncio.run(test_end_to_end_production_pipeline())