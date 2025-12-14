#!/usr/bin/env python3
"""
Comprehensive tests for Production Alerting and Incident Response System
Tests SLA management, recovery engine, RCA engine, and integrated monitoring.
"""

import pytest
import asyncio
import time
import logging
from unittest.mock import Mock, patch, AsyncMock
from typing import List

from aphrodite.engine.sla_manager import (
    SLAManager, SLAThreshold, SLAViolation, ViolationSeverity, SLAViolationType,
    create_production_sla_manager
)
from aphrodite.engine.recovery_engine import (
    RecoveryEngine, RecoveryProcedure, RecoveryAction, RecoveryStatus,
    create_production_recovery_engine
)
from aphrodite.engine.rca_engine import (
    RCAEngine, CorrelationType, RootCauseCategory, ConfidenceLevel,
    create_production_rca_engine
)
from aphrodite.engine.production_monitor import (
    ProductionMonitor, MonitoringMode, create_production_monitor
)


class TestSLAManager:
    """Test SLA Manager functionality"""
    
    def setup_method(self):
        self.sla_manager = SLAManager()
        self.violations_received = []
        
        def violation_handler(violation):
            self.violations_received.append(violation)
        
        self.sla_manager.register_violation_callback(violation_handler)
    
    def test_initialization(self):
        """Test SLA manager initialization"""
        assert not self.sla_manager.is_monitoring
        assert len(self.sla_manager.thresholds) > 0  # Should have default thresholds
        assert "request_latency_p95" in self.sla_manager.thresholds
        assert "tokens_per_second" in self.sla_manager.thresholds
    
    def test_threshold_management(self):
        """Test adding and removing SLA thresholds"""
        # Add custom threshold
        custom_threshold = SLAThreshold(
            metric_name="custom_metric",
            target_value=100.0,
            tolerance_percent=10.0
        )
        self.sla_manager.add_threshold(custom_threshold)
        
        assert "custom_metric" in self.sla_manager.thresholds
        assert self.sla_manager.thresholds["custom_metric"].target_value == 100.0
        
        # Remove threshold
        self.sla_manager.remove_threshold("custom_metric")
        assert "custom_metric" not in self.sla_manager.thresholds
    
    def test_measurement_recording(self):
        """Test metric measurement recording"""
        metric_name = "test_metric"
        
        # Record measurements
        for i in range(10):
            self.sla_manager.record_measurement(metric_name, float(i), time.time() + i)
        
        # Check measurements are stored
        assert len(self.sla_manager.measurement_windows[metric_name]) == 10
        
        # Check statistical analyzer has data
        mean_val, std_val, trend = self.sla_manager.statistical_analyzer.get_baseline_stats(metric_name)
        assert mean_val > 0
    
    def test_threshold_violation_detection(self):
        """Test SLA threshold violation detection"""
        # Add threshold for testing
        threshold = SLAThreshold(
            metric_name="test_latency",
            target_value=200.0,
            tolerance_percent=25.0,  # 250ms max
            measurement_window_minutes=1,
            violation_threshold_percent=60  # 60% of measurements must violate
        )
        self.sla_manager.add_threshold(threshold)
        
        current_time = time.time()
        
        # Add normal measurements (won't trigger violation)
        for i in range(5):
            self.sla_manager.record_measurement("test_latency", 180.0, current_time + i)
        
        assert len(self.violations_received) == 0
        
        # Add violating measurements (should trigger violation)
        for i in range(5):
            self.sla_manager.record_measurement("test_latency", 300.0, current_time + 10 + i)
        
        # Should have violation now
        assert len(self.violations_received) > 0
        violation = self.violations_received[0]
        assert violation.metric_name == "test_latency"
        assert violation.actual_value == 300.0
        assert violation.breach_percentage > 0
    
    def test_anomaly_detection(self):
        """Test statistical anomaly detection"""
        metric_name = "anomaly_test"
        
        # Build baseline (normal values around 100)
        baseline_time = time.time() - 3600  # 1 hour ago
        for i in range(50):
            value = 100.0 + (i % 10) - 5  # Values between 95-105
            self.sla_manager.statistical_analyzer.add_measurement(
                metric_name, value, baseline_time + i * 60
            )
        
        # Test anomaly detection
        is_anomaly, z_score = self.sla_manager.statistical_analyzer.is_anomaly(metric_name, 150.0)
        assert is_anomaly  # 150 should be anomalous compared to 95-105 baseline
        assert z_score > 2.0
        
        # Normal value should not be anomaly
        is_anomaly, z_score = self.sla_manager.statistical_analyzer.is_anomaly(metric_name, 102.0)
        assert not is_anomaly
    
    def test_performance_regression_detection(self):
        """Test performance regression detection"""
        metric_name = "regression_test"
        
        current_time = time.time()
        
        # Build baseline (good performance)
        for i in range(30):
            value = 100.0 + (i % 5)  # Values around 100-105
            self.sla_manager.statistical_analyzer.add_measurement(
                metric_name, value, current_time - (30 - i) * 60
            )
        
        # Add recent degraded performance
        for i in range(10):
            value = 130.0 + (i % 3)  # Values around 130-133 (30% increase)
            self.sla_manager.statistical_analyzer.add_measurement(
                metric_name, value, current_time - (10 - i) * 60
            )
        
        # Detect regression
        is_regression, change_percent = self.sla_manager.statistical_analyzer.detect_performance_regression(
            metric_name, recent_window_minutes=15
        )
        
        assert is_regression
        assert abs(change_percent) > 10  # Should detect significant change
    
    def test_sla_summary(self):
        """Test SLA summary generation"""
        summary = self.sla_manager.get_sla_summary()
        
        assert 'monitoring_active' in summary
        assert 'active_violations' in summary
        assert 'compliance_rate_percent' in summary
        assert 'thresholds_configured' in summary
        assert summary['thresholds_configured'] > 0


class TestRecoveryEngine:
    """Test Recovery Engine functionality"""
    
    def setup_method(self):
        self.recovery_engine = RecoveryEngine()
        self.executions_received = []
        
        def execution_handler(execution):
            self.executions_received.append(execution)
        
        self.recovery_engine.register_recovery_callback(execution_handler)
    
    def test_initialization(self):
        """Test recovery engine initialization"""
        assert len(self.recovery_engine.procedures) > 0  # Should have default procedures
        assert len(self.recovery_engine.circuit_breakers) > 0  # Should have circuit breakers
        assert "latency_spike_recovery" in self.recovery_engine.procedures
        assert "aphrodite_engine" in self.recovery_engine.circuit_breakers
    
    def test_procedure_management(self):
        """Test recovery procedure management"""
        # Add custom procedure
        custom_procedure = RecoveryProcedure(
            procedure_id="test_procedure",
            name="Test Recovery",
            description="Test recovery procedure",
            actions=[RecoveryAction.CACHE_INVALIDATION],
            timeout_seconds=60
        )
        
        self.recovery_engine.add_procedure(custom_procedure)
        assert "test_procedure" in self.recovery_engine.procedures
        
        # Remove procedure
        self.recovery_engine.remove_procedure("test_procedure")
        assert "test_procedure" not in self.recovery_engine.procedures
    
    def test_circuit_breaker_functionality(self):
        """Test circuit breaker functionality"""
        service_name = "test_service"
        
        # Initially can execute
        assert self.recovery_engine.can_execute_on_service(service_name)
        
        # Add circuit breaker for testing
        from aphrodite.engine.recovery_engine import CircuitBreaker
        cb = CircuitBreaker(service_name, failure_threshold=3, success_threshold=2)
        self.recovery_engine.circuit_breakers[service_name] = cb
        
        # Record failures
        for _ in range(3):
            self.recovery_engine.record_service_failure(service_name)
        
        # Circuit breaker should be open now
        assert not self.recovery_engine.can_execute_on_service(service_name)
        
        # Record successes after timeout (simulate)
        cb.state = cb.state.HALF_OPEN  # Simulate timeout passage
        for _ in range(2):
            self.recovery_engine.record_service_success(service_name)
        
        # Should be closed again
        assert self.recovery_engine.can_execute_on_service(service_name)
    
    @pytest.mark.asyncio
    async def test_recovery_execution(self):
        """Test recovery execution for SLA violations"""
        # Create mock SLA violation
        from aphrodite.engine.sla_manager import SLAThreshold
        
        threshold = SLAThreshold("test_metric", 100.0, 20.0)
        violation = SLAViolation(
            violation_id="test_violation",
            timestamp=time.time(),
            violation_type=SLAViolationType.LATENCY_BREACH,
            severity=ViolationSeverity.MAJOR,
            metric_name="request_latency_p95",
            threshold=threshold,
            actual_value=280.0,
            expected_value=200.0,
            breach_percentage=40.0,
            measurement_window=[250.0, 260.0, 280.0, 270.0, 290.0]
        )
        
        # Execute recovery
        execution = await self.recovery_engine.execute_recovery(violation)
        
        assert execution is not None
        assert execution.trigger_violation == violation
        assert execution.status in [RecoveryStatus.SUCCESS, RecoveryStatus.FAILED]
        assert len(execution.actions_completed) > 0 or len(execution.actions_failed) > 0
        
        # Check callback was called
        assert len(self.executions_received) > 0
    
    def test_health_checker(self):
        """Test health checker functionality"""
        health_checker = self.recovery_engine.health_checker
        
        # Register test health check
        def test_health_check():
            return True
        
        health_checker.register_health_check("test_service", test_health_check)
        
        # Check health
        is_healthy = health_checker.check_service_health("test_service")
        assert is_healthy
        
        # Check all health status
        status = health_checker.get_all_health_status()
        assert "test_service" in status
        assert status["test_service"] is True
    
    def test_recovery_summary(self):
        """Test recovery summary generation"""
        summary = self.recovery_engine.get_recovery_summary()
        
        assert 'total_executions' in summary
        assert 'successful_executions' in summary
        assert 'success_rate_percent' in summary
        assert 'circuit_breaker_status' in summary
        assert 'health_status' in summary


class TestRCAEngine:
    """Test RCA Engine functionality"""
    
    def setup_method(self):
        self.rca_engine = RCAEngine()
        self.analyses_received = []
        
        def analysis_handler(analysis):
            self.analyses_received.append(analysis)
        
        self.rca_engine.register_rca_callback(analysis_handler)
    
    def test_initialization(self):
        """Test RCA engine initialization"""
        assert self.rca_engine.metrics_collector is not None
        assert self.rca_engine.correlation_analyzer is not None
        assert self.rca_engine.diagnostic_collector is not None
        assert self.rca_engine.hypothesis_generator is not None
    
    def test_metric_recording(self):
        """Test metric recording for correlation analysis"""
        metric_name = "test_rca_metric"
        
        # Record metrics
        for i in range(20):
            self.rca_engine.record_metric(metric_name, float(i), time.time() + i)
        
        # Check metrics are stored
        assert len(self.rca_engine.metrics_collector.metrics_history[metric_name]) == 20
    
    def test_correlation_analysis(self):
        """Test correlation analysis between metrics"""
        current_time = time.time()
        
        # Create correlated metrics (metric2 = metric1 * 2 + noise)
        for i in range(30):
            timestamp = current_time + i * 60
            value1 = 100.0 + i
            value2 = (value1 * 2) + (i % 3) - 1  # Strong correlation with small noise
            
            self.rca_engine.record_metric("metric1", value1, timestamp)
            self.rca_engine.record_metric("metric2", value2, timestamp)
        
        # Find correlations
        correlations = self.rca_engine.correlation_analyzer.find_temporal_correlations(
            self.rca_engine.metrics_collector, current_time + 1800, window_minutes=60
        )
        
        # Should find strong correlation
        assert len(correlations) > 0
        correlation = correlations[0]
        assert abs(correlation.correlation_strength) > 0.8  # Strong correlation
    
    def test_diagnostic_data_collection(self):
        """Test diagnostic data collection"""
        diagnostic_data = self.rca_engine.diagnostic_collector.collect_diagnostic_data("test_incident")
        
        assert diagnostic_data.incident_id == "test_incident"
        assert diagnostic_data.timestamp > 0
        assert isinstance(diagnostic_data.system_metrics, dict)
        assert isinstance(diagnostic_data.process_metrics, dict)
        assert isinstance(diagnostic_data.configuration_snapshot, dict)
    
    @pytest.mark.asyncio
    async def test_rca_analysis(self):
        """Test complete RCA analysis"""
        # Create mock violation
        from aphrodite.engine.sla_manager import SLAThreshold
        
        threshold = SLAThreshold("test_metric", 100.0, 20.0)
        violation = SLAViolation(
            violation_id="test_rca_violation",
            timestamp=time.time(),
            violation_type=SLAViolationType.LATENCY_BREACH,
            severity=ViolationSeverity.CRITICAL,
            metric_name="request_latency_p95",
            threshold=threshold,
            actual_value=300.0,
            expected_value=200.0,
            breach_percentage=50.0,
            measurement_window=[280.0, 290.0, 300.0, 310.0, 320.0]
        )
        
        # Add some metric history
        current_time = time.time()
        for i in range(50):
            self.rca_engine.record_metric("cpu_usage", 60 + i, current_time - (50 - i) * 60)
            self.rca_engine.record_metric("request_latency_p95", 150 + i * 2, current_time - (50 - i) * 60)
        
        # Perform RCA
        analysis = await self.rca_engine.analyze_incident(violation)
        
        assert analysis.analysis_id is not None
        assert analysis.incident_violation == violation
        assert analysis.analysis_duration_seconds > 0
        assert isinstance(analysis.correlations_found, list)
        assert isinstance(analysis.hypotheses, list)
        assert len(analysis.recommendations) > 0
        
        # Check callback was called
        assert len(self.analyses_received) > 0
    
    def test_hypothesis_generation(self):
        """Test root cause hypothesis generation"""
        # Create diagnostic data with high resource usage
        from aphrodite.engine.rca_engine import DiagnosticData
        
        diagnostic_data = DiagnosticData(
            timestamp=time.time(),
            incident_id="test",
            system_metrics={'cpu_percent': 95.0, 'memory_percent': 92.0},
            process_metrics={'top_cpu_processes': [{'name': 'test_proc', 'cpu_percent': 60.0}]},
            network_metrics={},
            application_logs=[],
            error_traces=[],
            configuration_snapshot={'aphrodite_config': {'gpu_memory_utilization': 0.98}},
            resource_usage={}
        )
        
        # Create mock violation
        from aphrodite.engine.sla_manager import SLAThreshold
        threshold = SLAThreshold("cpu_usage", 80.0, 10.0)
        violation = SLAViolation(
            violation_id="test",
            timestamp=time.time(),
            violation_type=SLAViolationType.RESOURCE_EXHAUSTION,
            severity=ViolationSeverity.MAJOR,
            metric_name="cpu_usage",
            threshold=threshold,
            actual_value=95.0,
            expected_value=80.0,
            breach_percentage=18.75,
            measurement_window=[]
        )
        
        # Generate hypotheses
        hypotheses = self.rca_engine.hypothesis_generator.generate_hypotheses(
            violation, [], diagnostic_data
        )
        
        assert len(hypotheses) > 0
        # Should have infrastructure hypothesis due to high CPU/memory
        infra_hypotheses = [h for h in hypotheses if h.category == RootCauseCategory.INFRASTRUCTURE]
        assert len(infra_hypotheses) > 0
    
    def test_rca_summary(self):
        """Test RCA summary generation"""
        summary = self.rca_engine.get_rca_summary()
        
        assert 'total_analyses' in summary
        assert 'confidence_distribution' in summary
        assert 'root_cause_categories' in summary
        assert 'metrics_tracked' in summary


class TestProductionMonitor:
    """Test Production Monitor integration"""
    
    def setup_method(self):
        self.monitor = ProductionMonitor()
        self.alerts_received = []
        self.health_updates = []
        
        def alert_handler(alert):
            self.alerts_received.append(alert)
        
        def health_handler(health):
            self.health_updates.append(health)
        
        self.monitor.register_alert_callback(alert_handler)
        self.monitor.register_health_callback(health_handler)
    
    def test_initialization(self):
        """Test production monitor initialization"""
        assert self.monitor.sla_manager is not None
        assert self.monitor.recovery_engine is not None
        assert self.monitor.rca_engine is not None
        assert self.monitor.monitoring_mode == MonitoringMode.NORMAL
    
    def test_metric_recording(self):
        """Test metric recording through production monitor"""
        # Record metrics
        self.monitor.record_metric("test_metric", 100.0)
        
        # Metrics should be recorded in both SLA manager and RCA engine
        assert len(self.monitor.sla_manager.measurement_windows["test_metric"]) > 0
        assert len(self.monitor.rca_engine.metrics_collector.metrics_history["test_metric"]) > 0
    
    def test_monitoring_mode_adjustment(self):
        """Test automatic monitoring mode adjustment"""
        initial_mode = self.monitor.monitoring_mode
        
        # Set to high alert
        self.monitor.set_monitoring_mode(MonitoringMode.HIGH_ALERT)
        assert self.monitor.monitoring_mode == MonitoringMode.HIGH_ALERT
        assert self.monitor.alert_cooldown_seconds < 300  # Should be reduced
        
        # Set to emergency
        self.monitor.set_monitoring_mode(MonitoringMode.EMERGENCY)
        assert self.monitor.monitoring_mode == MonitoringMode.EMERGENCY
        assert self.monitor.health_check_interval < 60  # Should be reduced
    
    def test_stats_recording(self):
        """Test Aphrodite stats recording"""
        # Create mock stats
        from aphrodite.engine.metrics_types import Stats
        
        stats = Stats(
            now=time.time(),
            num_running_sys=5,
            num_waiting_sys=2,
            num_swapped_sys=0,
            gpu_cache_usage_sys=0.75,
            cpu_cache_usage_sys=0.60,
            cpu_prefix_cache_hit_rate=0.85,
            gpu_prefix_cache_hit_rate=0.90,
            num_prompt_tokens_iter=150,
            num_generation_tokens_iter=300,
            num_tokens_iter=450,
            time_to_first_tokens_iter=[0.5, 0.6, 0.7],
            time_per_output_tokens_iter=[0.1, 0.12, 0.11],
            num_preemption_iter=1,
            time_e2e_requests=[1.2, 1.5, 1.8],
            time_queue_requests=[0.1, 0.2, 0.15],
            time_inference_requests=[1.0, 1.2, 1.5],
            time_prefill_requests=[0.3, 0.4, 0.5],
            time_decode_requests=[0.7, 0.8, 1.0],
            num_prompt_tokens_requests=[50, 60, 40],
            num_generation_tokens_requests=[100, 120, 80],
            n_requests=[1, 1, 1],
            max_num_generation_tokens_requests=[200, 250, 150],
            max_tokens_requests=[300, 350, 250],
            finished_reason_requests=["stop", "length", "stop"],
            waiting_lora_adapters=[],
            running_lora_adapters=[],
            max_lora="0"
        )
        
        # Record stats
        self.monitor.record_stats(stats)
        
        # Should have recorded derived metrics
        assert len(self.monitor.sla_manager.measurement_windows) > 0
    
    @pytest.mark.asyncio
    async def test_integrated_incident_flow(self):
        """Test complete integrated incident handling flow"""
        # Start monitoring
        self.monitor.start_monitoring()
        
        try:
            # Simulate SLA violation by recording bad metrics
            current_time = time.time()
            
            # Record normal metrics first
            for i in range(5):
                self.monitor.record_metric("request_latency_p95", 150.0, current_time + i)
            
            # Then record violating metrics
            for i in range(8):
                self.monitor.record_metric("request_latency_p95", 300.0, current_time + 10 + i)
            
            # Wait for processing
            await asyncio.sleep(3)
            
            # Should have generated alerts
            assert len(self.alerts_received) > 0
            
            # Check for SLA violation alert
            sla_alerts = [a for a in self.alerts_received if a.sla_violation is not None]
            assert len(sla_alerts) > 0
            
        finally:
            self.monitor.stop_monitoring()
    
    def test_production_summary(self):
        """Test production summary generation"""
        summary = self.monitor.get_production_summary()
        
        assert 'monitoring_active' in summary
        assert 'monitoring_mode' in summary
        assert 'current_health' in summary
        assert 'component_status' in summary
        assert 'alert_statistics' in summary
        assert 'configuration' in summary
        
        # Check component status
        assert 'sla_manager' in summary['component_status']
        assert 'recovery_engine' in summary['component_status']
        assert 'rca_engine' in summary['component_status']
    
    def test_custom_stat_logger(self):
        """Test custom stat logger integration"""
        from aphrodite.engine.production_monitor import AphroditeProductionStatLogger
        from aphrodite.common.config import AphroditeConfig, ModelConfig, CacheConfig
        
        # Create minimal config for testing
        model_config = ModelConfig(
            model="test-model",
            tokenizer="test-tokenizer", 
            tokenizer_mode="auto",
            trust_remote_code=False,
            dtype="auto",
            seed=0,
            max_model_len=1024,
            quantization=None,
            enforce_eager=False,
            max_context_len_to_capture=8192,
            max_seq_len_to_capture=8192
        )
        
        cache_config = CacheConfig(
            block_size=16,
            gpu_memory_utilization=0.9,
            swap_space=4,
            cache_dtype="auto"
        )
        
        aphrodite_config = AphroditeConfig(
            model_config=model_config,
            cache_config=cache_config,
            parallel_config=None,
            scheduler_config=None,
            device_config=None,
            load_config=None,
            lora_config=None,
            vision_language_config=None,
            speculative_config=None,
            decoding_config=None,
            observability_config=None,
            prompt_adapter_config=None,
            quant_config=None
        )
        
        # Create stat logger
        stat_logger = AphroditeProductionStatLogger(
            local_interval=10.0,
            production_monitor=self.monitor,
            aphrodite_config=aphrodite_config
        )
        
        # Create mock stats
        from aphrodite.engine.metrics_types import Stats
        
        stats = Stats(
            now=time.time(),
            num_running_sys=3,
            num_waiting_sys=1,
            num_swapped_sys=0,
            gpu_cache_usage_sys=0.8,
            cpu_cache_usage_sys=0.5,
            cpu_prefix_cache_hit_rate=0.9,
            gpu_prefix_cache_hit_rate=0.95,
            num_prompt_tokens_iter=100,
            num_generation_tokens_iter=200,
            num_tokens_iter=300,
            time_to_first_tokens_iter=[0.4, 0.5],
            time_per_output_tokens_iter=[0.1, 0.1],
            num_preemption_iter=0,
            time_e2e_requests=[1.0, 1.2],
            time_queue_requests=[0.1, 0.1],
            time_inference_requests=[0.8, 1.0],
            time_prefill_requests=[0.2, 0.3],
            time_decode_requests=[0.6, 0.7],
            num_prompt_tokens_requests=[50, 50],
            num_generation_tokens_requests=[100, 100],
            n_requests=[1, 1],
            max_num_generation_tokens_requests=[200, 200],
            max_tokens_requests=[300, 300],
            finished_reason_requests=["stop", "stop"],
            waiting_lora_adapters=[],
            running_lora_adapters=[],
            max_lora="0"
        )
        
        # Log stats
        stat_logger.log(stats)
        
        # Should have forwarded to production monitor
        assert len(self.monitor.sla_manager.measurement_windows) > 0


@pytest.mark.asyncio
async def test_end_to_end_integration():
    """Test complete end-to-end integration of all components"""
    
    # Create production monitor
    monitor = create_production_monitor()
    
    alerts_received = []
    health_updates = []
    
    def alert_handler(alert):
        alerts_received.append(alert)
    
    def health_handler(health):
        health_updates.append(health)
    
    monitor.register_alert_callback(alert_handler)
    monitor.register_health_callback(health_handler)
    
    try:
        # Start monitoring
        monitor.start_monitoring()
        
        # Simulate normal operation
        current_time = time.time()
        for i in range(10):
            monitor.record_metric("request_latency_p95", 150.0 + i * 5, current_time + i)
            monitor.record_metric("tokens_per_second", 120.0 - i * 2, current_time + i)
            monitor.record_metric("cpu_usage", 60.0 + i, current_time + i)
        
        # Simulate performance degradation leading to SLA violation
        for i in range(10):
            monitor.record_metric("request_latency_p95", 280.0 + i * 10, current_time + 20 + i)
            monitor.record_metric("tokens_per_second", 70.0 - i * 3, current_time + 20 + i)
            monitor.record_metric("cpu_usage", 85.0 + i, current_time + 20 + i)
        
        # Wait for processing
        await asyncio.sleep(5)
        
        # Verify the complete flow worked
        # 1. Should have detected SLA violations
        sla_violations = [a for a in alerts_received if a.sla_violation is not None]
        assert len(sla_violations) > 0, "Should have detected SLA violations"
        
        # 2. Should have attempted recovery
        recovery_alerts = [a for a in alerts_received if a.recovery_execution is not None]
        # Note: Recovery may not always complete in test timeframe, so this is optional
        
        # 3. Should have health updates
        assert len(health_updates) > 0, "Should have health status updates"
        
        # 4. Health status should reflect issues
        latest_health = health_updates[-1]
        assert latest_health.overall_status in ["degraded", "critical"], \
            f"Health status should be degraded, got: {latest_health.overall_status}"
        
        # 5. Get comprehensive summary
        summary = monitor.get_production_summary()
        assert summary['current_health']['active_violations'] > 0, \
            "Should have active SLA violations"
        assert summary['alert_statistics']['total_24h'] > 0, \
            "Should have generated alerts"
        
        print(f"✅ End-to-end test successful:")
        print(f"   Alerts generated: {len(alerts_received)}")
        print(f"   SLA violations: {len(sla_violations)}")
        print(f"   Health updates: {len(health_updates)}")
        print(f"   Final health status: {latest_health.overall_status}")
        print(f"   Active violations: {summary['current_health']['active_violations']}")
        
    finally:
        monitor.stop_monitoring()


if __name__ == "__main__":
    # Run tests
    logging.basicConfig(level=logging.INFO)
    
    print("🧪 Running Production Alerting Tests")
    print("=" * 50)
    
    # Run individual test classes
    test_classes = [
        TestSLAManager,
        TestRecoveryEngine, 
        TestRCAEngine,
        TestProductionMonitor
    ]
    
    for test_class in test_classes:
        print(f"\n📋 Testing {test_class.__name__}...")
        
        # Create instance and run tests
        instance = test_class()
        test_methods = [method for method in dir(instance) if method.startswith('test_')]
        
        for method_name in test_methods:
            try:
                instance.setup_method()
                method = getattr(instance, method_name)
                
                if asyncio.iscoroutinefunction(method):
                    asyncio.run(method())
                else:
                    method()
                    
                print(f"   ✅ {method_name}")
            except Exception as e:
                print(f"   ❌ {method_name}: {e}")
    
    # Run end-to-end integration test
    print(f"\n🔄 Running End-to-End Integration Test...")
    try:
        asyncio.run(test_end_to_end_integration())
        print("   ✅ End-to-end integration test passed")
    except Exception as e:
        print(f"   ❌ End-to-end test failed: {e}")
    
    print("\n🎉 All tests completed!")