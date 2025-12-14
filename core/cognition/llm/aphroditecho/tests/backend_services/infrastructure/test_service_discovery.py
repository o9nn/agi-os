#!/usr/bin/env python3
"""
Comprehensive tests for Service Discovery implementation.

Tests service registration, discovery, health checking, and failure scenarios
for the distributed DTESN service discovery system.
"""

import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock, patch

import sys
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')

from backend_services.infrastructure.service_discovery import (
    ServiceDiscovery, ServiceEndpoint, ServiceHealth, ServiceStatus, ServiceType
)


class TestServiceDiscovery:
    """Test suite for service discovery functionality"""
    
    @pytest.fixture
    def service_discovery(self):
        """Create service discovery instance for testing"""
        return ServiceDiscovery(
            redis_url="redis://localhost:6379",
            health_check_interval=1.0,
            health_check_timeout=2.0,
            max_consecutive_failures=2,
            service_ttl=60
        )
    
    @pytest.fixture
    def sample_endpoint(self):
        """Sample service endpoint for testing"""
        return ServiceEndpoint(
            service_id="test-service-1",
            service_type=ServiceType.COGNITIVE_SERVICE,
            host="localhost",
            port=8080,
            protocol="http",
            path="/",
            weight=1.0,
            metadata={"version": "1.0.0", "region": "us-west"}
        )
    
    @pytest.fixture
    def dtesn_endpoint(self):
        """DTESN membrane service endpoint for testing"""
        return ServiceEndpoint(
            service_id="dtesn-membrane-1",
            service_type=ServiceType.DTESN_MEMBRANE,
            host="localhost",
            port=8081,
            protocol="http",
            path="/membrane",
            weight=2.0,
            metadata={"membrane_type": "memory", "capacity": "1000"}
        )

    @pytest.mark.asyncio
    async def test_initialization(self, service_discovery):
        """Test service discovery initialization"""
        # Mock Redis to avoid external dependency
        with patch('backend_services.infrastructure.service_discovery.aioredis') as mock_aioredis:
            mock_redis = AsyncMock()
            mock_aioredis.from_url.return_value = mock_redis
            
            await service_discovery.initialize()
            
            assert service_discovery._initialized
            assert service_discovery.health_check_task is not None
            assert service_discovery.cleanup_task is not None
            
            await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_registration(self, service_discovery, sample_endpoint):
        """Test service registration functionality"""
        await service_discovery.initialize()
        
        # Register service
        result = await service_discovery.register_service(sample_endpoint)
        assert result is True
        
        # Verify service is stored locally
        assert sample_endpoint.service_id in service_discovery.services
        stored_service = service_discovery.services[sample_endpoint.service_id]
        assert stored_service.host == sample_endpoint.host
        assert stored_service.port == sample_endpoint.port
        assert stored_service.service_type == sample_endpoint.service_type
        
        # Verify health status is initialized
        assert sample_endpoint.service_id in service_discovery.service_health
        health = service_discovery.service_health[sample_endpoint.service_id]
        assert health.status == ServiceStatus.STARTING
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_deregistration(self, service_discovery, sample_endpoint):
        """Test service deregistration functionality"""
        await service_discovery.initialize()
        
        # Register then deregister service
        await service_discovery.register_service(sample_endpoint)
        result = await service_discovery.deregister_service(sample_endpoint.service_id)
        
        assert result is True
        assert sample_endpoint.service_id not in service_discovery.services
        assert sample_endpoint.service_id not in service_discovery.service_health
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_discovery_by_type(self, service_discovery, sample_endpoint, dtesn_endpoint):
        """Test service discovery filtering by type"""
        await service_discovery.initialize()
        
        # Register services of different types
        await service_discovery.register_service(sample_endpoint)
        await service_discovery.register_service(dtesn_endpoint)
        
        # Mark services as healthy for discovery
        service_discovery.service_health[sample_endpoint.service_id].status = ServiceStatus.HEALTHY
        service_discovery.service_health[dtesn_endpoint.service_id].status = ServiceStatus.HEALTHY
        
        # Discover cognitive services
        cognitive_services = await service_discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(cognitive_services) == 1
        assert cognitive_services[0].service_id == sample_endpoint.service_id
        
        # Discover DTESN services
        dtesn_services = await service_discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(dtesn_services) == 1
        assert dtesn_services[0].service_id == dtesn_endpoint.service_id
        
        # Discover all services
        all_services = await service_discovery.discover_services()
        assert len(all_services) == 2
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_get_specific_service(self, service_discovery, sample_endpoint):
        """Test getting specific service by ID"""
        await service_discovery.initialize()
        
        # Register service
        await service_discovery.register_service(sample_endpoint)
        
        # Get service by ID
        retrieved_service = await service_discovery.get_service(sample_endpoint.service_id)
        assert retrieved_service is not None
        assert retrieved_service.service_id == sample_endpoint.service_id
        assert retrieved_service.host == sample_endpoint.host
        
        # Test non-existent service
        non_existent = await service_discovery.get_service("non-existent-service")
        assert non_existent is None
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_health_checking_success(self, service_discovery, sample_endpoint):
        """Test successful health check scenario"""
        await service_discovery.initialize()
        await service_discovery.register_service(sample_endpoint)
        
        # Mock successful health check response
        with patch('backend_services.infrastructure.service_discovery.ClientSession') as mock_session_class:
            mock_session = AsyncMock()
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_session.get.return_value.__aenter__ = AsyncMock(return_value=mock_response)
            mock_session_class.return_value.__aenter__ = AsyncMock(return_value=mock_session)
            
            # Trigger health check
            await service_discovery._check_service_health(sample_endpoint.service_id)
            
            # Verify health status is healthy
            health = service_discovery.service_health[sample_endpoint.service_id]
            assert health.status == ServiceStatus.HEALTHY
            assert health.consecutive_failures == 0
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio 
    async def test_health_checking_failure(self, service_discovery, sample_endpoint):
        """Test health check failure scenario"""
        await service_discovery.initialize()
        await service_discovery.register_service(sample_endpoint)
        
        # Mock failed health check response
        with patch('backend_services.infrastructure.service_discovery.ClientSession') as mock_session_class:
            mock_session = AsyncMock()
            mock_response = AsyncMock()
            mock_response.status = 500
            mock_session.get.return_value.__aenter__ = AsyncMock(return_value=mock_response)
            mock_session_class.return_value.__aenter__ = AsyncMock(return_value=mock_session)
            
            # Trigger multiple health check failures
            await service_discovery._check_service_health(sample_endpoint.service_id)
            await service_discovery._check_service_health(sample_endpoint.service_id)
            
            # Verify health status is unhealthy after consecutive failures
            health = service_discovery.service_health[sample_endpoint.service_id]
            assert health.status == ServiceStatus.UNHEALTHY
            assert health.consecutive_failures >= service_discovery.max_consecutive_failures
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_up_down_callbacks(self, service_discovery, sample_endpoint):
        """Test service up/down callback functionality"""
        await service_discovery.initialize()
        
        # Track callback invocations
        up_calls = []
        down_calls = []
        
        async def on_service_up(service_id):
            up_calls.append(service_id)
        
        async def on_service_down(service_id):
            down_calls.append(service_id)
        
        service_discovery.add_service_up_callback(on_service_up)
        service_discovery.add_service_down_callback(on_service_down)
        
        await service_discovery.register_service(sample_endpoint)
        
        # Simulate service going up (healthy)
        with patch('backend_services.infrastructure.service_discovery.ClientSession') as mock_session_class:
            mock_session = AsyncMock()
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_session.get.return_value.__aenter__ = AsyncMock(return_value=mock_response)
            mock_session_class.return_value.__aenter__ = AsyncMock(return_value=mock_session)
            
            await service_discovery._check_service_health(sample_endpoint.service_id)
            
            # Verify up callback was called
            assert sample_endpoint.service_id in up_calls
        
        # Simulate service going down (unhealthy)
        with patch('backend_services.infrastructure.service_discovery.ClientSession') as mock_session_class:
            mock_session = AsyncMock()
            mock_response = AsyncMock()
            mock_response.status = 500
            mock_session.get.return_value.__aenter__ = AsyncMock(return_value=mock_response)
            mock_session_class.return_value.__aenter__ = AsyncMock(return_value=mock_session)
            
            # Multiple failures to trigger unhealthy status
            for _ in range(service_discovery.max_consecutive_failures):
                await service_discovery._check_service_health(sample_endpoint.service_id)
            
            # Verify down callback was called
            assert sample_endpoint.service_id in down_calls
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_cleanup(self, service_discovery, sample_endpoint):
        """Test expired service cleanup"""
        # Set very short TTL for testing
        service_discovery.service_ttl = 1  # 1 second
        
        await service_discovery.initialize()
        await service_discovery.register_service(sample_endpoint)
        
        # Wait for service to expire
        await asyncio.sleep(1.5)
        
        # Trigger cleanup
        await service_discovery._cleanup_expired_services()
        
        # Verify service was cleaned up
        assert sample_endpoint.service_id not in service_discovery.services
        assert sample_endpoint.service_id not in service_discovery.service_health
        
        await service_discovery.shutdown()

    @pytest.mark.asyncio
    async def test_redis_fallback(self, service_discovery, sample_endpoint):
        """Test fallback to in-memory registry when Redis is unavailable"""
        # Initialize without Redis
        service_discovery.redis = None
        
        await service_discovery.initialize()
        
        # Register service (should work with in-memory registry)
        result = await service_discovery.register_service(sample_endpoint)
        assert result is True
        
        # Discover services (should work with in-memory registry)
        service_discovery.service_health[sample_endpoint.service_id].status = ServiceStatus.HEALTHY
        services = await service_discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(services) == 1
        assert services[0].service_id == sample_endpoint.service_id
        
        await service_discovery.shutdown()

    def test_service_endpoint_properties(self, sample_endpoint):
        """Test ServiceEndpoint property methods"""
        assert sample_endpoint.url == "http://localhost:8080/"
        assert sample_endpoint.address == "localhost:8080"
        
        # Test with custom path
        endpoint_with_path = ServiceEndpoint(
            service_id="test-2",
            service_type=ServiceType.API_GATEWAY,
            host="api.example.com",
            port=443,
            protocol="https",
            path="/api/v1"
        )
        assert endpoint_with_path.url == "https://api.example.com:443/api/v1"
        assert endpoint_with_path.address == "api.example.com:443"

    def test_service_health_properties(self):
        """Test ServiceHealth functionality"""
        health = ServiceHealth(
            service_id="test-service",
            status=ServiceStatus.HEALTHY,
            last_check=time.time(),
            response_time_ms=150.0,
            error_count=2,
            consecutive_failures=0
        )
        
        assert health.service_id == "test-service"
        assert health.status == ServiceStatus.HEALTHY
        assert health.response_time_ms == 150.0


class TestServiceDiscoveryIntegration:
    """Integration tests for service discovery with real scenarios"""
    
    @pytest.mark.asyncio
    async def test_dtesn_service_scenario(self):
        """Test realistic DTESN service discovery scenario"""
        discovery = ServiceDiscovery(health_check_interval=0.5)
        await discovery.initialize()
        
        # Register multiple DTESN services
        services = [
            ServiceEndpoint(
                service_id=f"dtesn-memory-{i}",
                service_type=ServiceType.DTESN_MEMBRANE,
                host="localhost",
                port=8080 + i,
                metadata={"membrane_type": "memory", "instance": i}
            )
            for i in range(3)
        ]
        
        for service in services:
            await discovery.register_service(service)
            # Mark as healthy
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Discover DTESN services
        discovered = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(discovered) == 3
        
        # Verify all services are discoverable
        service_ids = {s.service_id for s in discovered}
        expected_ids = {s.service_id for s in services}
        assert service_ids == expected_ids
        
        await discovery.shutdown()

    @pytest.mark.asyncio
    async def test_load_balancer_integration_scenario(self):
        """Test service discovery integration with load balancer"""
        discovery = ServiceDiscovery(health_check_interval=0.5)
        await discovery.initialize()
        
        # Register services with different weights
        services = [
            ServiceEndpoint(
                service_id="cognitive-1",
                service_type=ServiceType.COGNITIVE_SERVICE,
                host="localhost", 
                port=8001,
                weight=2.0
            ),
            ServiceEndpoint(
                service_id="cognitive-2", 
                service_type=ServiceType.COGNITIVE_SERVICE,
                host="localhost",
                port=8002, 
                weight=1.0
            )
        ]
        
        for service in services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        
        # Simulate load balancer discovering services
        available_services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(available_services) == 2
        
        # Verify services have different weights for load balancing
        weights = [s.weight for s in available_services]
        assert 2.0 in weights
        assert 1.0 in weights
        
        await discovery.shutdown()

    @pytest.mark.asyncio
    async def test_service_failure_recovery_scenario(self):
        """Test service failure and recovery scenario"""
        discovery = ServiceDiscovery(
            health_check_interval=0.1,
            max_consecutive_failures=2
        )
        await discovery.initialize()
        
        endpoint = ServiceEndpoint(
            service_id="test-service",
            service_type=ServiceType.COGNITIVE_SERVICE,
            host="localhost",
            port=8080
        )
        
        await discovery.register_service(endpoint)
        
        # Start healthy
        discovery.service_health[endpoint.service_id].status = ServiceStatus.HEALTHY
        
        # Simulate service failure
        discovery.service_health[endpoint.service_id].status = ServiceStatus.UNHEALTHY
        
        # Service should not be discovered when unhealthy
        services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(services) == 0
        
        # Simulate service recovery
        discovery.service_health[endpoint.service_id].status = ServiceStatus.HEALTHY
        
        # Service should be discovered again when healthy
        services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(services) == 1
        assert services[0].service_id == endpoint.service_id
        
        await discovery.shutdown()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])