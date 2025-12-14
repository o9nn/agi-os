#!/usr/bin/env python3
"""
Dynamic Model Updates Example for Aphrodite Engine

Demonstrates online model parameter updates, incremental learning,
and model versioning capabilities with zero service interruption.
"""

import asyncio
import json
import torch

from aphrodite.dynamic_model_manager import (
    DynamicModelManager,
    IncrementalUpdateRequest,
    DynamicUpdateConfig
)
from aphrodite.dtesn_integration import (
    DTESNDynamicIntegration,
    DTESNLearningConfig
)

# Mock components for demonstration
class MockEngineClient:
    """Mock engine client for example."""
    
    async def get_model_parameters(self):
        return {
            "timestamp": 1000.0,
            "parameter_count": 1000000,
            "model_state": {"layer1.weight": torch.randn(10, 10)}
        }

class MockModelConfig:
    """Mock model config for example."""
    
    def __init__(self):
        self.model = "example-model"
        self.max_model_len = 2048


async def demonstrate_basic_dynamic_updates():
    """Demonstrate basic dynamic model update functionality."""
    print("🚀 Dynamic Model Updates Demo")
    print("=" * 50)
    
    # Initialize components
    engine_client = MockEngineClient()
    model_config = MockModelConfig()
    
    config = DynamicUpdateConfig(
        max_versions=5,
        checkpoint_interval=3,
        auto_rollback_threshold=0.1,
        backup_dir="/tmp/model_checkpoints_demo"
    )
    
    manager = DynamicModelManager(
        engine_client=engine_client,
        model_config=model_config,
        config=config
    )
    
    # Mock the model parameter operations for demo
    async def mock_get_params():
        return {
            "timestamp": asyncio.get_event_loop().time(),
            "parameter_count": 1000000,
            "model_state": {"layer1.weight": torch.randn(10, 10)}
        }
    
    async def mock_load_params(params):
        pass
        
    async def mock_apply_update(request):
        pass
    
    manager._get_model_parameters = mock_get_params
    manager._load_model_parameters = mock_load_params
    manager._apply_parameter_update = mock_apply_update
    
    # Simulate varying performance metrics
    performance_sequence = [
        {"accuracy": 0.85, "latency_ms": 100.0, "throughput": 50.0},
        {"accuracy": 0.87, "latency_ms": 95.0, "throughput": 52.0},
        {"accuracy": 0.89, "latency_ms": 90.0, "throughput": 55.0},
        {"accuracy": 0.75, "latency_ms": 150.0, "throughput": 30.0},  # Poor performance
        {"accuracy": 0.88, "latency_ms": 92.0, "throughput": 53.0}
    ]
    
    metric_index = 0
    async def mock_get_performance():
        nonlocal metric_index
        if metric_index < len(performance_sequence):
            result = performance_sequence[metric_index]
            metric_index += 1
            return result
        return performance_sequence[-1]
    
    manager._get_performance_metrics = mock_get_performance
    
    # 1. Create initial version
    print("\n📦 Creating initial model version...")
    initial_version = await manager.create_initial_version("Initial baseline model")
    print(f"✅ Created version: {initial_version}")
    
    # 2. Apply incremental updates
    print("\n🔧 Applying incremental parameter updates...")
    
    updates = [
        ("layer1.weight", torch.tensor([0.01, 0.02, 0.03]), 0.01),
        ("layer2.bias", torch.tensor([0.005, 0.008]), 0.015),
        ("attention.weights", torch.tensor([0.1, 0.05]), 0.02),
        ("output.projection", torch.tensor([-0.05]), 0.05),  # This will cause poor performance
        ("layer3.norm", torch.tensor([0.001, 0.002]), 0.01)
    ]
    
    for i, (param_name, update_data, lr) in enumerate(updates, 1):
        print(f"\n  Update {i}: {param_name}")
        
        request = IncrementalUpdateRequest(
            parameter_name=param_name,
            update_data=update_data,
            learning_rate=lr,
            update_type="additive"
        )
        
        result = await manager.apply_incremental_update(request)
        
        if result["success"]:
            print(f"  ✅ Applied successfully (ID: {result['update_id']})")
            print(f"     Pre-metrics: {result.get('pre_metrics', {})}")
            print(f"     Post-metrics: {result.get('post_metrics', {})}")
        else:
            print(f"  ❌ Update failed: {result['reason']}")
            
        # Create checkpoint after some updates
        if i == 2:
            checkpoint_version = await manager.create_version(f"Checkpoint after {i} updates")
            print(f"  📋 Created checkpoint: {checkpoint_version}")
    
    # 3. Show version history
    print("\n📚 Version History:")
    versions = manager.list_versions()
    for version in versions:
        status = "🟢 ACTIVE" if version["is_active"] else "⚪ INACTIVE"
        print(f"  {status} {version['version_id']} - {version['description']}")
        print(f"    Timestamp: {version['timestamp']:.1f}")
        if version["performance_metrics"]:
            print(f"    Metrics: {version['performance_metrics']}")
    
    # 4. Demonstrate rollback
    print("\n🔄 Rolling back to checkpoint...")
    rollback_result = await manager.rollback_to_version(checkpoint_version)
    if rollback_result["success"]:
        print(f"✅ Rolled back to: {rollback_result['rolled_back_to']}")
    else:
        print(f"❌ Rollback failed: {rollback_result['reason']}")
    
    # 5. Show final status
    print("\n📊 Final Status:")
    status = manager.get_status()
    print(f"  Current Version: {status['current_version']}")
    print(f"  Total Versions: {status['total_versions']}")
    print(f"  Total Updates: {status['total_updates']}")
    print(f"  Config: {json.dumps(status['config'], indent=2)}")
    
    print("\n✨ Demo completed successfully!")


async def demonstrate_dtesn_integration():
    """Demonstrate DTESN cognitive learning integration."""
    print("\n🧠 DTESN Cognitive Learning Integration Demo")
    print("=" * 55)
    
    # Initialize components
    engine_client = MockEngineClient()
    model_config = MockModelConfig()
    
    dynamic_config = DynamicUpdateConfig(
        max_versions=3,
        checkpoint_interval=2,
        enable_incremental_learning=True
    )
    
    dtesn_config = DTESNLearningConfig(
        learning_rate=0.01,
        adaptation_rate=0.001,
        enable_plasticity=True,
        enable_homeostasis=True
    )
    
    manager = DynamicModelManager(
        engine_client=engine_client,
        model_config=model_config,
        config=dynamic_config
    )
    
    # Initialize DTESN integration
    dtesn_integration = DTESNDynamicIntegration(
        dynamic_manager=manager,
        dtesn_config=dtesn_config
    )
    
    print(f"DTESN Available: {dtesn_integration.dtesn_available}")
    
    # Demonstrate enhanced updates
    print("\n🎯 Enhanced Parameter Updates with DTESN:")
    
    learning_scenarios = [
        ("High Performance", 0.8, {"accuracy_change": 0.05, "latency_change": -5}),
        ("Moderate Performance", 0.3, {"accuracy_change": 0.02, "latency_change": 2}),
        ("Poor Performance", -0.2, {"accuracy_change": -0.03, "latency_change": 15}),
        ("Very Poor Performance", -0.7, {"accuracy_change": -0.08, "latency_change": 25})
    ]
    
    for scenario_name, expected_feedback, context in learning_scenarios:
        print(f"\n  Scenario: {scenario_name}")
        print(f"  Context: {context}")
        
        result = await dtesn_integration.enhanced_incremental_update(
            parameter_name=f"layer_{scenario_name.lower().replace(' ', '_')}",
            update_data=torch.randn(5, 5),
            performance_context=context
        )
        
        if result["success"]:
            print("  ✅ Enhanced update successful")
            if "dtesn_metrics" in result["data"]:
                metrics = result["data"]["dtesn_metrics"]
                print(f"     Algorithm: {metrics.get('learning_type', 'N/A')}")
                print(f"     Learning Rate: {metrics.get('learning_rate', 'N/A')}")
                if "weight_delta_mean" in metrics:
                    print(f"     Weight Delta Mean: {metrics['weight_delta_mean']:.6f}")
        else:
            print(f"  ❌ Enhanced update failed: {result['reason']}")
    
    # Show learning history
    print("\n📈 DTESN Learning History:")
    history = dtesn_integration.get_learning_history()
    for i, entry in enumerate(history[-3:], 1):  # Show last 3 entries
        print(f"  {i}. {entry['parameter_name']} - {entry['metrics']['learning_type']}")
        print(f"     Feedback: {entry['performance_feedback']:.3f}")
        print(f"     Timestamp: {entry['timestamp']:.1f}")
    
    # Show integration status
    print("\n🔧 Integration Status:")
    status = dtesn_integration.get_integration_status()
    print(f"  DTESN Available: {status['dtesn_available']}")
    print(f"  Total Learning Updates: {status['total_learning_updates']}")
    print(f"  Recent Algorithms: {status['recent_algorithms']}")
    
    print("\n✨ DTESN Integration demo completed!")


async def main():
    """Run all demonstrations."""
    try:
        await demonstrate_basic_dynamic_updates()
        await demonstrate_dtesn_integration()
        
    except Exception as e:
        print(f"\n❌ Demo failed with error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    print("🌟 Aphrodite Engine - Dynamic Model Updates")
    print("Demonstration of Task 4.1.2 Implementation")
    print("=" * 60)
    
    asyncio.run(main())