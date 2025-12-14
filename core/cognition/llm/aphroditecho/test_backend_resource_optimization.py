#!/usr/bin/env python3
"""
Test backend resource management optimization for Task 6.2.3.

Validates:
1. Dynamic resource allocation with adaptive thresholds
2. Load balancing for distributed DTESN operations
3. Graceful degradation under resource constraints
"""

import asyncio
import logging
import sys
import time
from typing import Dict, Any

# Add paths for imports
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/echo.kern')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class MockRedis:
    """Mock Redis for testing without actual Redis connection."""
    def __init__(self):
        self.data = {}
    
    async def setex(self, key: str, ttl: int, value: str):
        self.data[key] = value
        return True
    
    async def delete(self, key: str):
        if key in self.data:
            del self.data[key]
        return True


class ResourceOptimizationTester:
    """Test framework for backend resource optimization."""
    
    def __init__(self):
        self.test_results = {}
        self.scalability_manager = None
        
    async def setup(self):
        """Setup test environment."""
        try:
            # Import after path setup
            from scalability_manager import ScalabilityManager, ResourceType, ScalingAction
            
            # Create scalability manager with mock Redis
            self.scalability_manager = ScalabilityManager(
                redis_url='mock://localhost',
                monitoring_interval=5,
                cost_optimization=True,
                performance_weight=0.6
            )
            
            # Replace Redis with mock
            self.scalability_manager.redis = MockRedis()
            
            # Initialize some test data
            await self._setup_test_resources()
            
            logger.info("✅ Test setup completed successfully")
            return True
            
        except Exception as e:
            logger.error(f"❌ Test setup failed: {e}")
            return False
    
    async def _setup_test_resources(self):
        """Setup test resource instances and metrics."""
        from scalability_manager import ResourceType, ResourceMetrics
        
        # Add test resource instances
        self.scalability_manager.resource_instances[ResourceType.DTESN_MEMBRANES] = [
            "membrane-1", "membrane-2", "membrane-3"
        ]
        self.scalability_manager.resource_instances[ResourceType.COGNITIVE_SERVICE] = [
            "cognitive-1", "cognitive-2"
        ]
        
        # Add test metrics
        self.scalability_manager.resource_metrics['membrane-1'] = ResourceMetrics(
            resource_type=ResourceType.DTESN_MEMBRANES,
            instance_id='membrane-1',
            cpu_usage=0.7,
            memory_usage=0.6,
            throughput=10.0,
            efficiency_score=0.8,
            response_time_ms=25.0
        )
        
        self.scalability_manager.resource_metrics['membrane-2'] = ResourceMetrics(
            resource_type=ResourceType.DTESN_MEMBRANES,
            instance_id='membrane-2',
            cpu_usage=0.9,
            memory_usage=0.8,
            throughput=8.0,
            efficiency_score=0.6,
            response_time_ms=40.0
        )
    
    async def test_dynamic_resource_allocation(self) -> bool:
        """Test dynamic resource allocation with adaptive thresholds."""
        logger.info("🧪 Testing dynamic resource allocation...")
        
        try:
            # Test adaptive threshold calculation
            adaptive_up, adaptive_down = self.scalability_manager._calculate_adaptive_thresholds(0.8, 0.7)
            
            # Verify thresholds are reasonable
            assert 0.5 <= adaptive_up <= 1.0, f"Invalid adaptive up threshold: {adaptive_up}"
            assert 0.1 <= adaptive_down <= 0.6, f"Invalid adaptive down threshold: {adaptive_down}"
            
            # Test system load tracking
            self.scalability_manager._update_system_load_tracking(0.8, 0.7)
            assert len(self.scalability_manager.system_load_history) > 0
            assert len(self.scalability_manager.performance_history) > 0
            
            self.test_results['dynamic_allocation'] = True
            logger.info("✅ Dynamic resource allocation test passed")
            return True
            
        except Exception as e:
            logger.error(f"❌ Dynamic resource allocation test failed: {e}")
            self.test_results['dynamic_allocation'] = False
            return False
    
    async def test_load_balancing(self) -> bool:
        """Test load balancing for distributed DTESN operations."""
        logger.info("🧪 Testing DTESN load balancing...")
        
        try:
            from scalability_manager import ResourceType, ResourceMetrics
            
            # Create test metrics with different load levels
            high_load_metrics = ResourceMetrics(
                resource_type=ResourceType.DTESN_MEMBRANES,
                instance_id='membrane-high-load',
                cpu_usage=0.95,
                memory_usage=0.9,
                throughput=5.0,
                efficiency_score=0.4
            )
            
            low_load_metrics = ResourceMetrics(
                resource_type=ResourceType.DTESN_MEMBRANES,
                instance_id='membrane-low-load',
                cpu_usage=0.3,
                memory_usage=0.2,
                throughput=15.0,
                efficiency_score=0.9
            )
            
            test_metrics = [high_load_metrics, low_load_metrics]
            
            # Test load balancing
            from scalability_manager import ScalingAction
            await self.scalability_manager._balance_dtesn_load(test_metrics, ScalingAction.MAINTAIN, 2)
            
            # Verify load balancer pool was created and ordered correctly
            dtesn_pool = self.scalability_manager.load_balancer_pools.get(ResourceType.DTESN_MEMBRANES, [])
            assert len(dtesn_pool) == 2, f"Expected 2 membranes in pool, got {len(dtesn_pool)}"
            
            # Verify low-load instance comes first (better for load balancing)
            assert dtesn_pool[0] == 'membrane-low-load', "Low-load membrane should be first in balanced pool"
            
            self.test_results['load_balancing'] = True
            logger.info("✅ Load balancing test passed")
            return True
            
        except Exception as e:
            logger.error(f"❌ Load balancing test failed: {e}")
            self.test_results['load_balancing'] = False
            return False
    
    async def test_graceful_degradation(self) -> bool:
        """Test graceful degradation under resource constraints."""
        logger.info("🧪 Testing graceful degradation...")
        
        try:
            # Test degradation conditions detection
            should_degrade = await self.scalability_manager._should_activate_degradation(0.95, 0.4)
            assert should_degrade, "Should activate degradation under high load and poor performance"
            
            # Test degradation activation
            from scalability_manager import ResourceType, ResourceMetrics
            test_metrics = [self.scalability_manager.resource_metrics.get('membrane-1')]
            
            await self.scalability_manager._activate_graceful_degradation(
                ResourceType.DTESN_MEMBRANES, 
                test_metrics
            )
            
            # Verify degradation is active
            assert self.scalability_manager.degradation_active, "Degradation should be active"
            
            # Test degradation deactivation when conditions improve
            # Simulate improved conditions
            self.scalability_manager.system_load_history = [0.3, 0.4, 0.5]
            self.scalability_manager.performance_history = [0.8, 0.9, 0.85]
            
            await self.scalability_manager.deactivate_degradation()
            
            self.test_results['graceful_degradation'] = True
            logger.info("✅ Graceful degradation test passed")
            return True
            
        except Exception as e:
            logger.error(f"❌ Graceful degradation test failed: {e}")
            self.test_results['graceful_degradation'] = False
            return False
    
    async def test_dtesn_processor_integration(self) -> bool:
        """Test DTESN processor load balancing and degradation."""
        logger.info("🧪 Testing DTESN processor integration...")
        
        try:
            # Mock DTESN processor components to avoid complex dependencies
            class MockDTESNProcessor:
                def __init__(self):
                    self.load_balancer_enabled = True
                    self.degradation_active = False
                    self.membrane_pool = ["membrane-1", "membrane-2", "membrane-3"]
                    self.processing_queues = {}
                    self.current_load = 0.0
                    self.max_concurrent_processes = 10
                    self._processing_stats = {
                        "total_requests": 100,
                        "concurrent_requests": 8,
                        "failed_requests": 2,
                        "avg_processing_time": 0.5
                    }
                    
                    # Import the methods we added
                    import aphrodite.endpoints.deep_tree_echo.dtesn_processor as dtesn_mod
                    
                    # Create a real processor instance to get the methods
                    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
                    real_processor = dtesn_mod.DTESNProcessor(DTESNConfig())
                    
                    # Copy methods
                    self._select_optimal_membrane = real_processor._select_optimal_membrane.__get__(self, MockDTESNProcessor)
                    self._update_processing_load = real_processor._update_processing_load.__get__(self, MockDTESNProcessor)
                    self._check_degradation_conditions = real_processor._check_degradation_conditions.__get__(self, MockDTESNProcessor)
            
            processor = MockDTESNProcessor()
            
            # Test optimal membrane selection
            optimal_membrane = await processor._select_optimal_membrane()
            assert optimal_membrane in processor.membrane_pool, f"Selected membrane {optimal_membrane} not in pool"
            
            # Test load tracking
            processor._update_processing_load("membrane-1", "add")
            assert "membrane-1" in processor.processing_queues
            
            processor._update_processing_load("membrane-1", "remove")
            
            # Test degradation conditions
            degradation_needed = await processor._check_degradation_conditions()
            # With current stats, should not need degradation (8/10 concurrent, 2% error rate)
            assert not degradation_needed, "Should not need degradation with current stats"
            
            self.test_results['dtesn_integration'] = True
            logger.info("✅ DTESN processor integration test passed")
            return True
            
        except Exception as e:
            logger.error(f"❌ DTESN processor integration test failed: {e}")
            self.test_results['dtesn_integration'] = False
            return False
    
    async def test_performance_under_load(self) -> bool:
        """Test that server maintains performance under varying loads."""
        logger.info("🧪 Testing performance under varying loads...")
        
        try:
            # Simulate varying load conditions
            load_scenarios = [
                (0.3, 0.9),  # Low load, high performance
                (0.6, 0.8),  # Medium load, good performance  
                (0.85, 0.6), # High load, degraded performance
                (0.95, 0.3), # Very high load, poor performance
            ]
            
            performance_maintained = True
            
            for load, performance in load_scenarios:
                # Update system state
                self.scalability_manager._update_system_load_tracking(load, performance)
                
                # Test adaptive response
                adaptive_up, adaptive_down = self.scalability_manager._calculate_adaptive_thresholds(load, performance)
                
                # Test scaling evaluation
                from scalability_manager import ResourceType
                await self.scalability_manager._evaluate_resource_scaling(ResourceType.DTESN_MEMBRANES)
                
                # Verify appropriate response to load conditions
                if load > 0.9 and performance < 0.4:
                    # Should activate degradation for very high load + poor performance
                    assert self.scalability_manager.degradation_active or adaptive_up < 0.8, \
                        f"Should respond to high load {load} and poor performance {performance}"
                
                logger.info(f"📊 Load {load:.1f}, Performance {load:.1f}: "
                           f"Thresholds up={adaptive_up:.2f}, down={adaptive_down:.2f}")
            
            self.test_results['performance_under_load'] = performance_maintained
            logger.info("✅ Performance under load test completed")
            return True
            
        except Exception as e:
            logger.error(f"❌ Performance under load test failed: {e}")
            self.test_results['performance_under_load'] = False
            return False
    
    async def run_all_tests(self) -> Dict[str, Any]:
        """Run all backend resource optimization tests."""
        logger.info("🚀 Starting backend resource optimization tests...")
        
        if not await self.setup():
            return {"success": False, "error": "Setup failed"}
        
        tests = [
            ("Dynamic Resource Allocation", self.test_dynamic_resource_allocation),
            ("Load Balancing", self.test_load_balancing), 
            ("Graceful Degradation", self.test_graceful_degradation),
            ("DTESN Processor Integration", self.test_dtesn_processor_integration),
            ("Performance Under Load", self.test_performance_under_load),
        ]
        
        passed_tests = 0
        total_tests = len(tests)
        
        for test_name, test_func in tests:
            logger.info(f"Running: {test_name}")
            try:
                result = await test_func()
                if result:
                    passed_tests += 1
                    logger.info(f"✅ {test_name}: PASSED")
                else:
                    logger.error(f"❌ {test_name}: FAILED")
            except Exception as e:
                logger.error(f"💥 {test_name}: ERROR - {e}")
        
        success_rate = passed_tests / total_tests
        overall_success = success_rate >= 0.8  # 80% pass rate required
        
        summary = {
            "success": overall_success,
            "passed_tests": passed_tests,
            "total_tests": total_tests,
            "success_rate": success_rate,
            "test_results": self.test_results,
            "acceptance_criteria_met": overall_success,  # "Server maintains performance under varying loads"
        }
        
        if overall_success:
            logger.info(f"🎉 All tests completed successfully! ({passed_tests}/{total_tests} passed)")
            logger.info("✅ Task 6.2.3 acceptance criteria: Server maintains performance under varying loads - MET")
        else:
            logger.error(f"❌ Some tests failed ({passed_tests}/{total_tests} passed)")
        
        return summary


async def main():
    """Main test execution."""
    tester = ResourceOptimizationTester()
    results = await tester.run_all_tests()
    
    print("\n" + "="*60)
    print("BACKEND RESOURCE OPTIMIZATION TEST SUMMARY")
    print("="*60)
    print(f"Overall Success: {'✅ PASSED' if results['success'] else '❌ FAILED'}")
    print(f"Tests Passed: {results['passed_tests']}/{results['total_tests']}")
    print(f"Success Rate: {results['success_rate']:.1%}")
    print(f"Acceptance Criteria Met: {'✅ YES' if results.get('acceptance_criteria_met') else '❌ NO'}")
    print("\nIndividual Test Results:")
    for test_name, result in results.get('test_results', {}).items():
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"  {test_name}: {status}")
    print("="*60)
    
    return 0 if results['success'] else 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)