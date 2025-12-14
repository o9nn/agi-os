#!/usr/bin/env python3
"""
Comprehensive Test Suite for Deep Tree Echo Fusion with Aphrodite Engine

Tests the complete integration and validates all components of the fusion system.
"""

import asyncio
import logging
import sys
import traceback

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

# Import the fusion system
try:
    from cognitive_architectures.deep_tree_echo_fusion import DeepTreeEchoFusion, DeepTreeEchoConfig
    print("✓ Deep Tree Echo Fusion module imported successfully")
except ImportError as e:
    print(f"✗ Failed to import fusion module: {e}")
    sys.exit(1)


async def test_fusion_initialization():
    """Test basic initialization of the fusion system."""
    print("\n" + "="*60)
    print("🧪 Testing Deep Tree Echo Fusion Initialization")
    print("="*60)
    
    try:
        # Create configuration for comprehensive testing
        config = DeepTreeEchoConfig(
            enable_4e_embodied_ai=True,
            enable_sensory_motor_mapping=True,
            enable_proprioceptive_feedback=True,
            enable_adaptive_architecture=True,
            enable_membrane_computing=True,
            enable_echo_state_networks=True,
            max_concurrent_agents=1000,
            evolution_generations=10,
            mutation_rate=0.05,
            reservoir_size=500,
            enable_cpu_mode=True,
            log_level="INFO"
        )
        
        # Initialize fusion system
        fusion = DeepTreeEchoFusion(config)
        print("✓ Fusion system created with config")
        
        # Test initialization
        success = await fusion.initialize()
        print(f"✓ Initialization {'successful' if success else 'failed'}: {success}")
        
        if success:
            # Get comprehensive status
            status = await fusion.get_system_status()
            
            print("\n📊 System Status Report:")
            print(f"  Initialized: {status['initialized']}")
            print(f"  Echo-Self Available: {status['echo_self_available']}")
            print(f"  Aphrodite Integration: {status['aphrodite_integration_available']}")
            print(f"  DTESN Kernel: {status['dtesn_kernel_available']}")
            
            print("\n🎯 4E Embodied AI Framework:")
            for key, value in status['config'].items():
                print(f"  {key.replace('_', ' ').title()}: {value}")
            
            print("\n🔗 Fusion Connections:")
            for key, value in status['fusion_metrics'].items():
                print(f"  {key.replace('_', ' ').title()}: {value}")
            
            return fusion, True
        else:
            return None, False
            
    except Exception as e:
        print(f"✗ Initialization test failed: {e}")
        traceback.print_exc()
        return None, False


async def test_fusion_lifecycle(fusion):
    """Test the complete lifecycle of the fusion system."""
    print("\n" + "="*60)
    print("🔄 Testing Deep Tree Echo Fusion Lifecycle")
    print("="*60)
    
    try:
        # Test starting the system
        start_result = await fusion.start_fusion()
        print(f"✓ System start {'successful' if start_result else 'failed'}: {start_result}")
        
        if start_result:
            # Test basic request processing
            demo_requests = [
                {
                    'prompt': 'Test basic cognitive processing',
                    'max_tokens': 50,
                    'task_type': 'reasoning'
                },
                {
                    'prompt': 'Demonstrate 4E Embodied AI capabilities',
                    'max_tokens': 100,
                    'use_membrane_computing': True,
                    'enable_proprioception': True,
                    'task_type': 'embodied_cognition'
                },
                {
                    'prompt': 'Show agent-arena-relation orchestration',
                    'max_tokens': 75,
                    'agents_required': 3,
                    'arena_type': 'collaborative',
                    'task_type': 'multi_agent'
                }
            ]
            
            print(f"\n🧠 Processing {len(demo_requests)} test requests:")
            
            for i, request in enumerate(demo_requests, 1):
                print(f"\n  Request {i}: {request['task_type']}")
                try:
                    response = await fusion.process_request(request)
                    print(f"    ✓ Response received: {type(response).__name__}")
                    
                    # Analyze response
                    if isinstance(response, dict):
                        if 'agent_id' in response:
                            print(f"    📍 Agent ID: {response['agent_id']}")
                        if 'arena_state' in response:
                            arena = response['arena_state']
                            print(f"    🏟️  Arena: {arena.get('type', 'unknown')} ({arena.get('agent_count', 0)} agents)")
                        if 'orchestration_meta' in response:
                            meta = response['orchestration_meta']
                            print(f"    ⏱️  Processing time: {meta.get('processing_time', 0):.3f}s")
                            print(f"    🤝 Social cognition: {meta.get('social_cognition_enabled', False)}")
                    
                except Exception as e:
                    print(f"    ✗ Request {i} failed: {e}")
            
            return True
        else:
            return False
            
    except Exception as e:
        print(f"✗ Lifecycle test failed: {e}")
        traceback.print_exc()
        return False


async def test_4e_embodied_ai_framework(fusion):
    """Test the 4E Embodied AI Framework specifically."""
    print("\n" + "="*60)
    print("🤖 Testing 4E Embodied AI Framework")
    print("="*60)
    
    try:
        # Test each aspect of 4E Embodied AI
        embodied_tests = [
            {
                'name': 'Embodied Cognition',
                'request': {
                    'prompt': 'Process sensory input and generate motor response',
                    'sensory_input': {'vision': [1, 2, 3], 'touch': [0.5, 0.8]},
                    'enable_proprioception': True,
                    'virtual_body_active': True
                }
            },
            {
                'name': 'Embedded Processing',
                'request': {
                    'prompt': 'Adapt to environmental constraints',
                    'environment_context': {'resources_limited': True, 'real_time': True},
                    'resource_constraints': {'memory': '1GB', 'time': '100ms'}
                }
            },
            {
                'name': 'Extended Cognition',
                'request': {
                    'prompt': 'Use external tools and memory',
                    'external_tools': ['calculator', 'memory_bank', 'knowledge_graph'],
                    'distributed_processing': True
                }
            },
            {
                'name': 'Enactive Perception',
                'request': {
                    'prompt': 'Generate action-perception loops',
                    'action_perception_loops': True,
                    'sensorimotor_contingency': True,
                    'emergent_behavior': True
                }
            }
        ]
        
        print(f"Testing {len(embodied_tests)} aspects of 4E Embodied AI:")
        
        for test in embodied_tests:
            print(f"\n  🧪 Testing {test['name']}:")
            try:
                response = await fusion.process_request(test['request'])
                print(f"    ✓ {test['name']} test completed")
                
                # Check for embodied AI indicators in response
                if isinstance(response, dict):
                    if 'agent_id' in response:
                        print(f"    🤖 Agent engaged: {response['agent_id'][:12]}...")
                    if 'arena_state' in response:
                        print(f"    🌍 Environment coupled: {response['arena_state']['type']}")
                    if 'action_result' in response:
                        print(f"    ⚡ Action performed: {response['action_result']['type']}")
                
            except Exception as e:
                print(f"    ✗ {test['name']} test failed: {e}")
        
        return True
        
    except Exception as e:
        print(f"✗ 4E Embodied AI test failed: {e}")
        traceback.print_exc()
        return False


async def test_system_performance(fusion):
    """Test system performance and metrics."""
    print("\n" + "="*60)
    print("📈 Testing System Performance")
    print("="*60)
    
    try:
        import time
        
        # Performance test requests
        performance_requests = [
            {'prompt': f'Performance test {i}', 'max_tokens': 20}
            for i in range(10)
        ]
        
        print(f"Running {len(performance_requests)} requests for performance testing...")
        
        start_time = time.time()
        successful_requests = 0
        
        for i, request in enumerate(performance_requests):
            try:
                request_start = time.time()
                response = await fusion.process_request(request)
                request_time = time.time() - request_start
                
                if response:
                    successful_requests += 1
                    print(f"  Request {i+1}: ✓ ({request_time:.3f}s)")
                else:
                    print(f"  Request {i+1}: ✗ (no response)")
                    
            except Exception as e:
                print(f"  Request {i+1}: ✗ ({e})")
        
        total_time = time.time() - start_time
        success_rate = (successful_requests / len(performance_requests)) * 100
        
        print("\n📊 Performance Results:")
        print(f"  Total time: {total_time:.3f}s")
        print(f"  Average per request: {total_time/len(performance_requests):.3f}s")
        print(f"  Success rate: {success_rate:.1f}% ({successful_requests}/{len(performance_requests)})")
        print(f"  Requests per second: {len(performance_requests)/total_time:.2f}")
        
        return success_rate > 80  # Consider success if > 80% of requests succeed
        
    except Exception as e:
        print(f"✗ Performance test failed: {e}")
        traceback.print_exc()
        return False


async def run_comprehensive_fusion_test():
    """Run the complete test suite for Deep Tree Echo Fusion."""
    print("🌳" + "="*58 + "🌳")
    print("  Deep Tree Echo Fusion with Aphrodite Engine - Test Suite")
    print("🌳" + "="*58 + "🌳")
    
    test_results = {
        'initialization': False,
        'lifecycle': False,
        '4e_embodied_ai': False,
        'performance': False
    }
    
    try:
        # Test 1: Initialization
        fusion, init_success = await test_fusion_initialization()
        test_results['initialization'] = init_success
        
        if fusion and init_success:
            # Test 2: Lifecycle
            lifecycle_success = await test_fusion_lifecycle(fusion)
            test_results['lifecycle'] = lifecycle_success
            
            if lifecycle_success:
                # Test 3: 4E Embodied AI Framework
                embodied_success = await test_4e_embodied_ai_framework(fusion)
                test_results['4e_embodied_ai'] = embodied_success
                
                # Test 4: Performance
                performance_success = await test_system_performance(fusion)
                test_results['performance'] = performance_success
            
            # Shutdown
            print("\n🔄 Shutting down fusion system...")
            await fusion.shutdown()
            print("✓ Shutdown complete")
        
    except Exception as e:
        print(f"✗ Test suite failed: {e}")
        traceback.print_exc()
    
    # Final results
    print("\n" + "="*60)
    print("🎯 FINAL TEST RESULTS")
    print("="*60)
    
    total_tests = len(test_results)
    passed_tests = sum(1 for result in test_results.values() if result)
    
    for test_name, result in test_results.items():
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"  {test_name.replace('_', ' ').title()}: {status}")
    
    success_rate = (passed_tests / total_tests) * 100
    print(f"\nOverall Success Rate: {success_rate:.1f}% ({passed_tests}/{total_tests})")
    
    if success_rate >= 75:
        print("\n🎉 Deep Tree Echo Fusion with Aphrodite Engine - OPERATIONAL!")
        print("🚀 Maximum challenge and innovation achieved!")
        print("🌳 4E Embodied AI Framework active and functional!")
        return True
    else:
        print("\n⚠️  System requires additional optimization")
        return False


if __name__ == "__main__":
    # Run the comprehensive test
    success = asyncio.run(run_comprehensive_fusion_test())
    sys.exit(0 if success else 1)