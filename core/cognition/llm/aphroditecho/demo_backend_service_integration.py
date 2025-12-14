#!/usr/bin/env python3
"""
Demonstration of Backend Service Integration

Shows the complete backend service integration system working together:
- Service discovery for DTESN components
- Circuit breaker patterns for fault tolerance
- Graceful service degradation
- Dynamic configuration management

This demonstrates Task 7.3.2 implementation.
"""

import asyncio
import time
import logging
from typing import Dict, Any

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

from backend_services.infrastructure.service_discovery import (
    ServiceDiscovery, ServiceEndpoint, ServiceType, ServiceStatus
)
from backend_services.infrastructure.circuit_breaker import (
    CircuitBreaker, CircuitBreakerConfig, CircuitState
)
from backend_services.infrastructure.service_degradation import (
    ServiceDegradationManager, DegradationLevel, FeaturePriority, 
    ResourceType, Feature
)
from backend_services.infrastructure.config_manager import (
    ConfigurationManager, ConfigSource
)


async def demo_service_discovery():
    """Demonstrate service discovery functionality"""
    print("\n" + "="*60)
    print("🔍 SERVICE DISCOVERY DEMONSTRATION")
    print("="*60)
    
    # Initialize service discovery
    discovery = ServiceDiscovery(health_check_interval=1.0, service_ttl=60)
    await discovery.initialize()
    
    # Register DTESN services
    dtesn_services = [
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
    
    print("📝 Registering DTESN services...")
    for service in dtesn_services:
        result = await discovery.register_service(service)
        print(f"   ✅ Registered {service.service_id}: {service.service_type.value}")
        # Mark as healthy for demo
        discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
    
    print("\n🔍 Discovering services...")
    all_services = await discovery.discover_services()
    print(f"   📊 Total services discovered: {len(all_services)}")
    
    dtesn_services_found = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
    print(f"   🧠 DTESN membrane services: {len(dtesn_services_found)}")
    for service in dtesn_services_found:
        print(f"      - {service.service_id} ({service.metadata.get('membrane_type', 'unknown')})")
    
    # Simulate service failure
    print("\n⚠️ Simulating service failure...")
    discovery.service_health["dtesn-memory-1"].status = ServiceStatus.UNHEALTHY
    
    healthy_services = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
    print(f"   💚 Healthy DTESN services after failure: {len(healthy_services)}")
    
    await discovery.shutdown()
    return dtesn_services


async def demo_circuit_breaker():
    """Demonstrate circuit breaker functionality"""
    print("\n" + "="*60)
    print("🔒 CIRCUIT BREAKER DEMONSTRATION")
    print("="*60)
    
    # Configure circuit breaker
    config = CircuitBreakerConfig(
        failure_threshold=3,
        timeout=2.0,
        request_timeout=1.0
    )
    
    circuit = CircuitBreaker("dtesn-membrane-service", config=config)
    await circuit.initialize()
    
    # Simulate service operations
    async def membrane_processing(membrane_type: str, should_fail: bool = False):
        if should_fail:
            raise Exception(f"Membrane processing failed: {membrane_type}")
        await asyncio.sleep(0.1)  # Simulate processing time
        return {"membrane": membrane_type, "processed": True, "timestamp": time.time()}
    
    print("✅ Testing successful operations...")
    for i in range(2):
        result = await circuit.call(membrane_processing, "memory", False)
        print(f"   🧠 Operation {i+1}: {result['membrane']} processed successfully")
    
    print(f"\n⚡ Circuit state: {circuit.state.value}")
    print(f"📊 Metrics: {circuit.metrics.successful_calls} successes, {circuit.metrics.failed_calls} failures")
    
    print("\n❌ Testing failed operations...")
    for i in range(config.failure_threshold):
        try:
            await circuit.call(membrane_processing, "memory", True)
        except Exception as e:
            print(f"   💥 Failure {i+1}: {e}")
    
    print(f"\n⚡ Circuit state after failures: {circuit.state.value}")
    
    # Test circuit open behavior
    print("\n🚫 Testing circuit breaker protection...")
    try:
        await circuit.call(membrane_processing, "memory", False)
    except Exception as e:
        print(f"   🛡️ Circuit breaker protected: {e}")
    
    return circuit


async def demo_service_degradation():
    """Demonstrate service degradation functionality"""
    print("\n" + "="*60)
    print("📉 SERVICE DEGRADATION DEMONSTRATION")  
    print("="*60)
    
    # Initialize degradation manager
    manager = ServiceDegradationManager(
        "dtesn-processing-service",
        check_interval=1.0,
        recovery_delay=2.0
    )
    await manager.initialize()
    
    # Register DTESN features
    features = [
        Feature("membrane_computation", FeaturePriority.CRITICAL, 
               metadata={"description": "Core DTESN membrane processing"}),
        Feature("advanced_reasoning", FeaturePriority.HIGH,
               metadata={"description": "Advanced cognitive reasoning"}),
        Feature("real_time_visualization", FeaturePriority.MEDIUM,
               metadata={"description": "Real-time processing visualization"}),
        Feature("performance_metrics", FeaturePriority.LOW,
               metadata={"description": "Detailed performance monitoring"}),
        Feature("debug_tracing", FeaturePriority.OPTIONAL,
               metadata={"description": "Debug trace logging"})
    ]
    
    for feature in features:
        manager.register_feature(feature)
        print(f"   📝 Registered feature: {feature.name} ({feature.priority.value})")
    
    # Show normal operation
    print(f"\n🟢 Normal operation - Level: {manager.current_level.value}")
    for feature in features:
        enabled = await manager.is_feature_enabled(feature.name)
        status = "✅" if enabled else "❌"
        print(f"   {status} {feature.name}")
    
    # Force partial degradation
    print(f"\n🟡 Forcing partial degradation...")
    await manager.force_degradation(DegradationLevel.PARTIAL, "Demo: Increased system load")
    
    print(f"   📊 Degradation level: {manager.current_level.value}")
    for feature in features:
        enabled = await manager.is_feature_enabled(feature.name)
        status = "✅" if enabled else "❌"
        print(f"   {status} {feature.name}")
    
    # Force minimal operation
    print(f"\n🔴 Forcing minimal operation...")
    await manager.force_degradation(DegradationLevel.MINIMAL, "Demo: Critical resource shortage")
    
    print(f"   📊 Degradation level: {manager.current_level.value}")
    for feature in features:
        enabled = await manager.is_feature_enabled(feature.name)
        status = "✅" if enabled else "❌"
        print(f"   {status} {feature.name}")
    
    # Demonstrate recovery
    print(f"\n💚 Demonstrating recovery...")
    await manager.force_degradation(DegradationLevel.NORMAL, "Demo: Resources restored")
    
    print(f"   📊 Degradation level: {manager.current_level.value}")
    for feature in features:
        enabled = await manager.is_feature_enabled(feature.name)
        status = "✅" if enabled else "❌"
        print(f"   {status} {feature.name}")
    
    # Show degradation history
    print(f"\n📊 Degradation History:")
    history = manager.get_degradation_history(limit=5)
    for i, event in enumerate(history):
        print(f"   {i+1}. {event['previous_level']} → {event['new_level']}: {event['reason']}")
    
    await manager.shutdown()
    return manager


async def demo_configuration_management():
    """Demonstrate configuration management functionality"""
    print("\n" + "="*60)
    print("⚙️ CONFIGURATION MANAGEMENT DEMONSTRATION")
    print("="*60)
    
    # Initialize configuration manager
    config_manager = ConfigurationManager("dtesn-backend-service")
    await config_manager.initialize()
    
    print("📝 Setting initial configuration...")
    config_updates = {
        "membrane_depth": 6,
        "esn_reservoir_size": 1024,
        "processing_timeout": 30.0,
        "enable_caching": True,
        "log_level": "INFO"
    }
    
    for key, value in config_updates.items():
        await config_manager.set_config(key, value, ConfigSource.API, f"Initial setup: {key}")
        print(f"   ✅ Set {key} = {value}")
    
    print(f"\n📊 Configuration summary:")
    summary = await config_manager.get_config_summary()
    print(f"   📦 Total entries: {summary['total_entries']}")
    print(f"   🔄 Versions: {summary['versions']}")
    print(f"   📅 Last updated: {time.ctime(summary['last_updated'])}")
    
    # Create version snapshot
    print(f"\n📸 Creating configuration snapshot...")
    version_id = await config_manager.create_version_snapshot("Demo configuration v1")
    print(f"   📋 Created version: {version_id}")
    
    # Update configuration
    print(f"\n🔄 Updating configuration...")
    await config_manager.set_config("membrane_depth", 8, ConfigSource.API, "Increased processing depth")
    await config_manager.set_config("processing_timeout", 45.0, ConfigSource.API, "Extended timeout")
    
    print(f"   📊 Updated membrane_depth: {await config_manager.get_config('membrane_depth')}")
    print(f"   📊 Updated processing_timeout: {await config_manager.get_config('processing_timeout')}")
    
    # Demonstrate rollback
    print(f"\n🔙 Demonstrating configuration rollback...")
    versions = await config_manager.list_versions()
    if versions:
        rollback_version = versions[0]["version_id"]
        success = await config_manager.rollback_to_version(rollback_version)
        print(f"   {'✅' if success else '❌'} Rollback to {rollback_version}: {'Success' if success else 'Failed'}")
        
        print(f"   📊 After rollback membrane_depth: {await config_manager.get_config('membrane_depth')}")
        print(f"   📊 After rollback processing_timeout: {await config_manager.get_config('processing_timeout')}")
    
    await config_manager.shutdown()
    return config_manager


async def demo_integrated_scenario():
    """Demonstrate all components working together"""
    print("\n" + "="*60)
    print("🚀 INTEGRATED DTESN BACKEND SERVICE SCENARIO")
    print("="*60)
    
    # Initialize all components
    discovery = ServiceDiscovery(health_check_interval=2.0)
    await discovery.initialize()
    
    degradation_manager = ServiceDegradationManager("integrated-dtesn-system", check_interval=1.0)
    await degradation_manager.initialize()
    
    config_manager = ConfigurationManager("dtesn-integration")
    await config_manager.initialize()
    
    # Register features
    features = [
        Feature("membrane_processing", FeaturePriority.CRITICAL),
        Feature("service_discovery", FeaturePriority.HIGH),
        Feature("performance_monitoring", FeaturePriority.MEDIUM)
    ]
    
    for feature in features:
        degradation_manager.register_feature(feature)
    
    # Set up configuration
    await config_manager.set_config("max_services", 5, ConfigSource.API)
    await config_manager.set_config("circuit_breaker_threshold", 3, ConfigSource.API)
    
    print("🏗️ System initialized with all components")
    
    # Register services with discovery
    services = [
        ServiceEndpoint("dtesn-core", ServiceType.DTESN_MEMBRANE, "localhost", 8100),
        ServiceEndpoint("cognitive-1", ServiceType.COGNITIVE_SERVICE, "localhost", 8101),
        ServiceEndpoint("cache-1", ServiceType.CACHE_SERVICE, "localhost", 8102)
    ]
    
    service_circuits = {}
    
    print("\n📝 Registering services with circuit breaker protection...")
    for service in services:
        await discovery.register_service(service)
        discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Create circuit breaker for each service
        threshold = await config_manager.get_config("circuit_breaker_threshold", 3)
        circuit = CircuitBreaker(f"cb-{service.service_id}", 
                               config=CircuitBreakerConfig(failure_threshold=threshold))
        await circuit.initialize()
        service_circuits[service.service_id] = circuit
        
        print(f"   ✅ {service.service_id} registered with circuit breaker")
    
    # Simulate system operation
    print(f"\n🔄 Simulating system operations...")
    
    async def simulate_service_call(service_id: str, should_fail: bool = False):
        circuit = service_circuits[service_id]
        
        async def service_operation():
            if should_fail:
                raise Exception(f"Service {service_id} failed")
            await asyncio.sleep(0.05)
            return {"service": service_id, "status": "success", "timestamp": time.time()}
        
        try:
            result = await circuit.call(service_operation)
            return result
        except Exception as e:
            return {"service": service_id, "status": "failed", "error": str(e)}
    
    # Normal operations
    print(f"\n✅ Normal operations phase...")
    for service in services:
        result = await simulate_service_call(service.service_id)
        print(f"   📞 {service.service_id}: {result['status']}")
    
    available_services = await discovery.discover_services()
    print(f"   📊 Available services: {len(available_services)}")
    print(f"   📊 System degradation level: {degradation_manager.current_level.value}")
    
    # Simulate service failures
    print(f"\n⚠️ Simulating service failures...")
    failing_service = "cognitive-1"
    
    # Trigger circuit breaker
    for i in range(3):
        result = await simulate_service_call(failing_service, should_fail=True)
        print(f"   💥 Failure {i+1} for {failing_service}: {result['status']}")
    
    # Check circuit breaker state
    circuit_state = service_circuits[failing_service].state
    print(f"   🔴 Circuit breaker for {failing_service}: {circuit_state.value}")
    
    # Update service discovery
    discovery.service_health[failing_service].status = ServiceStatus.UNHEALTHY
    
    # Trigger degradation based on service availability
    healthy_services = await discovery.discover_services()
    if len(healthy_services) < await config_manager.get_config("max_services", 5):
        await degradation_manager.force_degradation(
            DegradationLevel.PARTIAL,
            f"Service failure: {failing_service}"
        )
    
    print(f"   📊 Healthy services after failure: {len(healthy_services)}")
    print(f"   📊 System degradation level: {degradation_manager.current_level.value}")
    
    # Show feature states
    print(f"\n🎛️ Feature states after degradation:")
    for feature in features:
        enabled = await degradation_manager.is_feature_enabled(feature.name)
        status = "✅" if enabled else "❌"
        print(f"   {status} {feature.name}")
    
    # Recovery simulation
    print(f"\n💚 Simulating service recovery...")
    discovery.service_health[failing_service].status = ServiceStatus.HEALTHY
    
    # Reset circuit breaker
    await service_circuits[failing_service].reset()
    
    # Restore normal operation
    await degradation_manager.force_degradation(
        DegradationLevel.NORMAL,
        "Service recovered"
    )
    
    healthy_services_recovered = await discovery.discover_services()
    print(f"   📊 Services available after recovery: {len(healthy_services_recovered)}")
    print(f"   📊 System degradation level: {degradation_manager.current_level.value}")
    
    # Cleanup
    await discovery.shutdown()
    await degradation_manager.shutdown()
    await config_manager.shutdown()
    
    print(f"\n🎉 Integrated scenario completed successfully!")


async def main():
    """Main demonstration function"""
    print("🚀 Backend Service Integration Demonstration")
    print("Task 7.3.2: Enhance Backend Service Integration")
    print("=" * 60)
    
    try:
        # Run individual component demonstrations
        await demo_service_discovery()
        await demo_circuit_breaker()
        await demo_service_degradation()
        await demo_configuration_management()
        
        # Run integrated scenario
        await demo_integrated_scenario()
        
        print(f"\n" + "="*60)
        print("✅ ALL DEMONSTRATIONS COMPLETED SUCCESSFULLY!")
        print("🎯 Backend Service Integration Implementation Complete")
        print("   - ✅ Service Discovery for distributed DTESN components")
        print("   - ✅ Circuit Breaker patterns for fault tolerance") 
        print("   - ✅ Graceful Service Degradation mechanisms")
        print("   - ✅ Dynamic Configuration Management")
        print("   - ✅ Robust service integration with fault tolerance")
        print("=" * 60)
        
    except Exception as e:
        print(f"\n❌ Demonstration failed: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    asyncio.run(main())