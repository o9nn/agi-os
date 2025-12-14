#!/usr/bin/env python3
"""
Integration tests for Backend Service Integration.

Tests the complete integration of service discovery, circuit breakers,
and service degradation working together in realistic DTESN scenarios.
"""

import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock, patch

import sys
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')

from backend_services.infrastructure.service_discovery import (
    ServiceDiscovery, ServiceEndpoint, ServiceType, ServiceStatus
)
from backend_services.infrastructure.circuit_breaker import (
    CircuitBreaker, CircuitBreakerConfig, CircuitState, CircuitBreakerException
)
from backend_services.infrastructure.service_degradation import (
    ServiceDegradationManager, DegradationLevel, FeaturePriority, 
    ResourceType, Feature
)


class TestBackendServiceIntegration:
    """Integration tests for complete backend service system"""
    
    @pytest.fixture
    async def service_discovery_system(self):
        """Setup service discovery system"""
        discovery = ServiceDiscovery(
            redis_url="redis://localhost:6379",
            health_check_interval=0.5,
            health_check_timeout=1.0,
            max_consecutive_failures=2
        )
        await discovery.initialize()
        return discovery
    
    @pytest.fixture
    def dtesn_services(self):
        """Create sample DTESN service endpoints"""
        return [
            ServiceEndpoint(
                service_id="dtesn-memory-1",
                service_type=ServiceType.DTESN_MEMBRANE,
                host="localhost",
                port=8081,
                metadata={"membrane_type": "memory", "capacity": 1000}
            ),
            ServiceEndpoint(
                service_id="dtesn-reasoning-1",
                service_type=ServiceType.DTESN_MEMBRANE,
                host="localhost",
                port=8082,
                metadata={"membrane_type": "reasoning", "complexity": "high"}
            ),
            ServiceEndpoint(
                service_id="cognitive-service-1",
                service_type=ServiceType.COGNITIVE_SERVICE,
                host="localhost",
                port=8083,
                metadata={"version": "2.0", "capabilities": ["nlp", "vision"]}
            )
        ]
    
    @pytest.fixture
    async def degradation_manager(self):
        """Setup service degradation manager"""
        manager = ServiceDegradationManager(
            "integrated-dtesn-system",
            check_interval=0.2,
            recovery_delay=0.5
        )
        await manager.initialize()
        
        # Register DTESN-specific features
        features = [
            Feature("membrane_processing", FeaturePriority.CRITICAL),
            Feature("advanced_reasoning", FeaturePriority.HIGH),
            Feature("real_time_adaptation", FeaturePriority.MEDIUM),
            Feature("performance_monitoring", FeaturePriority.LOW),
            Feature("debug_visualization", FeaturePriority.OPTIONAL)
        ]
        
        for feature in features:
            manager.register_feature(feature)
        
        return manager

    @pytest.mark.asyncio
    async def test_complete_dtesn_service_lifecycle(self, service_discovery_system, dtesn_services):
        """Test complete service lifecycle with discovery and health monitoring"""
        discovery = service_discovery_system
        
        # Register all DTESN services
        for service in dtesn_services:
            result = await discovery.register_service(service)
            assert result is True
        
        # Simulate services becoming healthy
        for service in dtesn_services:
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Discover all services
        all_services = await discovery.discover_services()
        assert len(all_services) == 3
        
        # Discover DTESN services specifically
        dtesn_services_discovered = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(dtesn_services_discovered) == 2
        
        # Verify service metadata is preserved
        memory_service = next((s for s in dtesn_services_discovered if s.service_id == "dtesn-memory-1"), None)
        assert memory_service is not None
        assert memory_service.metadata["membrane_type"] == "memory"
        
        # Simulate service failure
        discovery.service_health["dtesn-memory-1"].status = ServiceStatus.UNHEALTHY
        
        # Should now discover one less DTESN service
        healthy_dtesn = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(healthy_dtesn) == 1
        assert healthy_dtesn[0].service_id == "dtesn-reasoning-1"
        
        await discovery.shutdown()

    @pytest.mark.asyncio
    async def test_circuit_breaker_with_service_discovery(self, service_discovery_system, dtesn_services):
        """Test circuit breaker integration with service discovery"""
        discovery = service_discovery_system
        
        # Register services
        for service in dtesn_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Create circuit breakers for each service
        circuit_breakers = {}
        for service in dtesn_services:
            cb_config = CircuitBreakerConfig(
                failure_threshold=2,
                timeout=0.5,
                request_timeout=1.0
            )
            circuit_breakers[service.service_id] = CircuitBreaker(
                f"cb-{service.service_id}",
                config=cb_config
            )
            await circuit_breakers[service.service_id].initialize()
        
        # Simulate service calls through circuit breakers
        async def simulate_membrane_call(service_id, input_data):
            if service_id == "dtesn-memory-1":
                # This service will fail
                raise Exception(f"Service {service_id} unavailable")
            return {"service": service_id, "processed": input_data, "status": "success"}
        
        # Make successful calls to reasoning service
        cb_reasoning = circuit_breakers["dtesn-reasoning-1"]
        result = await cb_reasoning.call(simulate_membrane_call, "dtesn-reasoning-1", "test_input")
        assert result["status"] == "success"
        
        # Make failing calls to memory service
        cb_memory = circuit_breakers["dtesn-memory-1"]
        for _ in range(2):  # failure_threshold = 2
            with pytest.raises(Exception):
                await cb_memory.call(simulate_membrane_call, "dtesn-memory-1", "test_input")
        
        # Circuit should be open
        assert cb_memory.state == CircuitState.OPEN
        
        # Update service discovery to reflect service failure
        discovery.service_health["dtesn-memory-1"].status = ServiceStatus.UNHEALTHY
        
        # Discover available services (should exclude failed service)
        available_services = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        available_ids = {s.service_id for s in available_services}
        assert "dtesn-reasoning-1" in available_ids
        assert "dtesn-memory-1" not in available_ids  # Should be excluded due to unhealthy status
        
        await discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_degradation_with_discovery_integration(self, service_discovery_system, degradation_manager, dtesn_services):
        """Test service degradation responding to service discovery events"""
        discovery = service_discovery_system
        manager = degradation_manager
        
        # Register services
        for service in dtesn_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Set up degradation callbacks to respond to service failures
        service_availability = len(dtesn_services)
        
        async def on_service_down(service_id):
            nonlocal service_availability
            service_availability -= 1
            
            # Trigger degradation based on service availability
            if service_availability <= 1:
                await manager.force_degradation(
                    DegradationLevel.EMERGENCY, 
                    f"Critical service unavailable: {service_id}"
                )
            elif service_availability <= 2:
                await manager.force_degradation(
                    DegradationLevel.MINIMAL,
                    f"Service degraded due to failure: {service_id}"
                )
        
        async def on_service_up(service_id):
            nonlocal service_availability
            service_availability += 1
            
            # Attempt recovery when services come back online
            if service_availability >= 3:
                await manager.force_degradation(
                    DegradationLevel.NORMAL,
                    f"All services available, recovering from: {service_id}"
                )
        
        discovery.add_service_down_callback(on_service_down)
        discovery.add_service_up_callback(on_service_up)
        
        # Initially all services healthy, system should be normal
        assert manager.current_level == DegradationLevel.NORMAL
        
        # Simulate service failures
        discovery.service_health["dtesn-memory-1"].status = ServiceStatus.UNHEALTHY
        await on_service_down("dtesn-memory-1")
        
        # Should trigger minimal degradation (2 services remaining)
        assert manager.current_level == DegradationLevel.MINIMAL
        assert await manager.is_feature_enabled("membrane_processing")  # Critical
        assert not await manager.is_feature_enabled("real_time_adaptation")  # Medium
        
        # Another service fails
        discovery.service_health["cognitive-service-1"].status = ServiceStatus.UNHEALTHY
        await on_service_down("cognitive-service-1")
        
        # Should trigger emergency degradation (1 service remaining)
        assert manager.current_level == DegradationLevel.EMERGENCY
        assert await manager.is_feature_enabled("membrane_processing")  # Critical
        assert not await manager.is_feature_enabled("advanced_reasoning")  # High
        
        # Service recovery
        discovery.service_health["dtesn-memory-1"].status = ServiceStatus.HEALTHY
        await on_service_up("dtesn-memory-1")
        
        discovery.service_health["cognitive-service-1"].status = ServiceStatus.HEALTHY
        await on_service_up("cognitive-service-1")
        
        # Should recover to normal (all services available)
        assert manager.current_level == DegradationLevel.NORMAL
        assert await manager.is_feature_enabled("debug_visualization")  # All features enabled
        
        await discovery.shutdown()
        await manager.shutdown()

    @pytest.mark.asyncio
    async def test_load_balanced_circuit_breaker_scenario(self, service_discovery_system):
        """Test load balancing with circuit breaker protection"""
        discovery = service_discovery_system
        
        # Create multiple instances of the same service type
        service_instances = [
            ServiceEndpoint(f"cognitive-{i}", ServiceType.COGNITIVE_SERVICE, 
                          "localhost", 8090 + i, weight=1.0 if i != 1 else 2.0)
            for i in range(3)
        ]
        
        # Register all instances
        for service in service_instances:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Create circuit breakers for each instance
        circuit_breakers = {}
        for service in service_instances:
            cb = CircuitBreaker(f"cb-{service.service_id}", 
                              config=CircuitBreakerConfig(failure_threshold=2, timeout=0.5))
            await cb.initialize()
            circuit_breakers[service.service_id] = cb
        
        # Simulate load balancer selecting services and making calls
        async def make_service_call(service_id, should_fail=False):
            cb = circuit_breakers[service_id]
            
            async def service_operation():
                if should_fail:
                    raise Exception(f"Service {service_id} failed")
                return {"service": service_id, "result": "success"}
            
            try:
                return await cb.call(service_operation)
            except CircuitBreakerException:
                return {"service": service_id, "result": "circuit_open"}
            except Exception:
                return {"service": service_id, "result": "failed"}
        
        # All services working normally
        available_services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(available_services) == 3
        
        # Make successful calls to all services
        for service in available_services:
            result = await make_service_call(service.service_id)
            assert result["result"] == "success"
        
        # Simulate one service starting to fail
        failing_service_id = "cognitive-1"
        for _ in range(2):  # Trigger circuit breaker
            result = await make_service_call(failing_service_id, should_fail=True)
            assert result["result"] == "failed"
        
        # Circuit should be open for failing service
        assert circuit_breakers[failing_service_id].state == CircuitState.OPEN
        
        # Next call should be rejected by circuit breaker
        result = await make_service_call(failing_service_id)
        assert result["result"] == "circuit_open"
        
        # Update service discovery to mark service as unhealthy
        discovery.service_health[failing_service_id].status = ServiceStatus.UNHEALTHY
        
        # Load balancer should now only see healthy services
        healthy_services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        healthy_ids = {s.service_id for s in healthy_services}
        assert len(healthy_services) == 2
        assert failing_service_id not in healthy_ids
        
        # Remaining services should still work
        for service in healthy_services:
            result = await make_service_call(service.service_id)
            assert result["result"] == "success"
        
        await discovery.shutdown()

    @pytest.mark.asyncio
    async def test_dtesn_membrane_processing_resilience(self, service_discovery_system, degradation_manager):
        """Test DTESN membrane processing resilience under various failure conditions"""
        discovery = service_discovery_system
        manager = degradation_manager
        
        # Create DTESN membrane services for different processing types
        membrane_services = [
            ServiceEndpoint("memory-membrane-1", ServiceType.DTESN_MEMBRANE, 
                          "localhost", 8100, metadata={"type": "memory", "priority": "high"}),
            ServiceEndpoint("reasoning-membrane-1", ServiceType.DTESN_MEMBRANE,
                          "localhost", 8101, metadata={"type": "reasoning", "priority": "high"}), 
            ServiceEndpoint("grammar-membrane-1", ServiceType.DTESN_MEMBRANE,
                          "localhost", 8102, metadata={"type": "grammar", "priority": "medium"}),
            ServiceEndpoint("cache-service-1", ServiceType.CACHE_SERVICE,
                          "localhost", 8103, metadata={"type": "redis", "priority": "low"})
        ]
        
        # Register all services
        for service in membrane_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Create circuit breakers for membrane operations
        membrane_circuits = {}
        for service in membrane_services:
            cb = CircuitBreaker(f"membrane-{service.service_id}",
                              config=CircuitBreakerConfig(failure_threshold=3, timeout=1.0))
            await cb.initialize()
            membrane_circuits[service.service_id] = cb
        
        # Set up resource monitoring for degradation
        system_resources = {
            ResourceType.CPU: 0.3,
            ResourceType.MEMORY: 0.4
        }
        
        def create_resource_monitor(resource_type):
            def monitor():
                return system_resources[resource_type]
            return monitor
        
        for resource_type in system_resources:
            manager.register_resource_monitor(resource_type, create_resource_monitor(resource_type))
        
        # Simulate complex DTESN processing workflow
        async def dtesn_membrane_workflow():
            results = {}
            
            # Get available membrane services
            available_membranes = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
            
            # Process through each membrane type
            for service in available_membranes:
                membrane_type = service.metadata.get("type", "unknown")
                circuit = membrane_circuits[service.service_id]
                
                try:
                    async def membrane_operation():
                        # Simulate membrane processing time based on system load
                        processing_time = 0.1 * (1 + system_resources[ResourceType.CPU])
                        await asyncio.sleep(processing_time)
                        
                        # Some membranes might fail under high load
                        if system_resources[ResourceType.CPU] > 0.8 and membrane_type == "grammar":
                            raise Exception(f"High load failure in {membrane_type} membrane")
                        
                        return {
                            "membrane": service.service_id,
                            "type": membrane_type,
                            "processed": True,
                            "load": system_resources[ResourceType.CPU]
                        }
                    
                    result = await circuit.call(membrane_operation)
                    results[membrane_type] = result
                    
                except CircuitBreakerException:
                    # Use degraded processing if available
                    if await manager.is_feature_enabled("membrane_processing"):
                        results[membrane_type] = {
                            "membrane": service.service_id,
                            "type": membrane_type,
                            "processed": False,
                            "fallback": True
                        }
                except Exception:
                    # Skip failed membranes
                    continue
            
            return results
        
        # Test normal processing
        result = await dtesn_membrane_workflow()
        assert len(result) == 3  # Should process all membrane types
        assert all(r["processed"] for r in result.values())
        
        # Increase system load to trigger degradation
        system_resources[ResourceType.CPU] = 0.75
        await manager._check_resources_and_adjust()
        
        assert manager.current_level == DegradationLevel.PARTIAL
        
        # Continue processing under partial degradation
        result = await dtesn_membrane_workflow()
        assert len(result) >= 2  # Should still process core membranes
        
        # Further increase load to trigger failures
        system_resources[ResourceType.CPU] = 0.9
        await manager._check_resources_and_adjust()
        
        assert manager.current_level == DegradationLevel.EMERGENCY
        
        # Process with high failure rate
        for _ in range(5):  # Multiple attempts to trigger circuit breakers
            try:
                result = await dtesn_membrane_workflow()
            except Exception:
                continue  # Expected failures
        
        # Check that some circuits are open due to failures
        grammar_circuit = membrane_circuits["grammar-membrane-1"]
        # Grammar membrane should fail under high load
        
        # System should still provide basic functionality
        assert await manager.is_feature_enabled("membrane_processing")  # Critical feature
        assert not await manager.is_feature_enabled("performance_monitoring")  # Low priority
        
        await discovery.shutdown()
        await manager.shutdown()

    @pytest.mark.asyncio
    async def test_graceful_system_recovery(self, service_discovery_system, degradation_manager):
        """Test graceful recovery of the entire system after failures"""
        discovery = service_discovery_system
        manager = degradation_manager
        
        # Set up comprehensive service ecosystem
        all_services = [
            ServiceEndpoint("api-gateway", ServiceType.API_GATEWAY, "localhost", 8200),
            ServiceEndpoint("load-balancer", ServiceType.LOAD_BALANCER, "localhost", 8201),
            ServiceEndpoint("dtesn-core", ServiceType.DTESN_MEMBRANE, "localhost", 8202),
            ServiceEndpoint("cognitive-main", ServiceType.COGNITIVE_SERVICE, "localhost", 8203),
            ServiceEndpoint("cache-primary", ServiceType.CACHE_SERVICE, "localhost", 8204),
            ServiceEndpoint("monitoring", ServiceType.MONITORING, "localhost", 8205)
        ]
        
        # Register all services as healthy
        for service in all_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Set up system resource monitoring
        system_state = {
            "cpu": 0.3, 
            "memory": 0.4,
            "network": 0.2,
            "services_failing": 0
        }
        
        def cpu_monitor():
            return system_state["cpu"]
        def memory_monitor():
            return system_state["memory"]
        def network_monitor():
            return system_state["network"]
        
        manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        manager.register_resource_monitor(ResourceType.MEMORY, memory_monitor)
        manager.register_resource_monitor(ResourceType.NETWORK, network_monitor)
        
        # Track system health over time
        health_timeline = []
        
        async def record_system_health():
            available_services = await discovery.discover_services()
            degradation_status = manager.get_status()
            
            health_timeline.append({
                "timestamp": time.time(),
                "available_services": len(available_services),
                "degradation_level": degradation_status["current_level"],
                "enabled_features": len(degradation_status["enabled_features"]),
                "system_resources": dict(system_state)
            })
        
        # Record initial healthy state
        await record_system_health()
        
        # Simulate cascading failure scenario
        failure_timeline = [
            # Phase 1: Increased load
            {"cpu": 0.7, "memory": 0.6, "services_failing": 0},
            # Phase 2: Some services fail
            {"cpu": 0.8, "memory": 0.7, "services_failing": 1},  # Cache fails
            # Phase 3: Critical failure
            {"cpu": 0.9, "memory": 0.9, "services_failing": 3},  # Multiple failures
            # Phase 4: Recovery begins
            {"cpu": 0.7, "memory": 0.8, "services_failing": 2},  # Load reduces
            # Phase 5: Full recovery
            {"cpu": 0.4, "memory": 0.5, "services_failing": 0}   # All healthy
        ]
        
        for i, state in enumerate(failure_timeline):
            # Update system state
            system_state.update(state)
            
            # Update service health based on failures
            failed_services = ["cache-primary", "monitoring", "cognitive-main"][:state["services_failing"]]
            for service in all_services:
                if service.service_id in failed_services:
                    discovery.service_health[service.service_id].status = ServiceStatus.UNHEALTHY
                else:
                    discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
            
            # Check resource-based degradation
            await manager._check_resources_and_adjust()
            
            # Record system state
            await record_system_health()
            
            # Wait for system to stabilize
            await asyncio.sleep(0.1)
        
        # Analyze health timeline
        assert len(health_timeline) == 6  # Initial + 5 phases
        
        # Verify degradation progression
        initial_state = health_timeline[0]
        worst_state = health_timeline[3]  # Should be the worst point
        final_state = health_timeline[-1]
        
        # Initial state should be optimal
        assert initial_state["available_services"] == 6
        assert initial_state["degradation_level"] == DegradationLevel.NORMAL.value
        
        # Worst state should show significant degradation
        assert worst_state["available_services"] < initial_state["available_services"]
        assert worst_state["degradation_level"] in [
            DegradationLevel.EMERGENCY.value, DegradationLevel.MINIMAL.value
        ]
        
        # Final state should show recovery
        assert final_state["available_services"] == initial_state["available_services"]
        assert final_state["degradation_level"] == DegradationLevel.NORMAL.value
        assert final_state["enabled_features"] >= worst_state["enabled_features"]
        
        # Verify recovery timeline makes sense
        service_counts = [h["available_services"] for h in health_timeline]
        degradation_severity = [
            list(DegradationLevel).index(DegradationLevel(h["degradation_level"])) 
            for h in health_timeline
        ]
        
        # Should show degradation then recovery pattern
        max_degradation_point = degradation_severity.index(max(degradation_severity))
        assert max_degradation_point > 0  # Degradation occurred
        assert max_degradation_point < len(degradation_severity) - 1  # Recovery occurred
        
        await discovery.shutdown()
        await manager.shutdown()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])