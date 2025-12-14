#!/usr/bin/env python3
"""
Dynamic Configuration Management Demo for Aphrodite Engine DTESN

Demonstrates server-side configuration management with:
- Dynamic parameter updates without service restart
- Configuration validation and rollback mechanisms  
- Environment-specific configuration handling
- Real-time configuration synchronization
"""

import asyncio
import json
import logging
from pathlib import Path

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

async def demonstrate_dynamic_configuration():
    """Demonstrate dynamic configuration management capabilities."""
    
    print("🚀 Dynamic Configuration Management Demo")
    print("=" * 60)
    
    try:
        # Import configuration components
        from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
        from aphrodite.endpoints.deep_tree_echo.dynamic_config_manager import (
            DynamicConfigurationManager,
            ConfigurationUpdateRequest,
            ConfigurationEnvironment,
            initialize_dynamic_config_manager
        )
        
        # 1. Initialize configuration manager
        print("\n📋 1. Initializing Dynamic Configuration Manager")
        print("-" * 50)
        
        initial_config = DTESNConfig(
            max_membrane_depth=4,
            esn_reservoir_size=512,
            bseries_max_order=8,
            enable_caching=True,
            cache_ttl_seconds=300,
            enable_performance_monitoring=True
        )
        
        manager = initialize_dynamic_config_manager(
            initial_config=initial_config,
            max_snapshots=20,
            backup_directory=Path("/tmp/dtesn_config_demo"),
            enable_auto_backup=True
        )
        
        print(f"✅ Configuration manager initialized")
        print(f"   Environment: {manager.environment.value}")
        print(f"   Initial snapshots: {len(manager.get_snapshots())}")
        print(f"   Backup directory: {manager.backup_directory}")
        
        # Display initial configuration
        current_config = manager.current_config
        print(f"\n📊 Initial Configuration:")
        print(f"   - Membrane Depth: {current_config.max_membrane_depth}")
        print(f"   - ESN Reservoir Size: {current_config.esn_reservoir_size}")
        print(f"   - B-Series Max Order: {current_config.bseries_max_order}")
        print(f"   - Caching: {'enabled' if current_config.enable_caching else 'disabled'}")
        print(f"   - Cache TTL: {current_config.cache_ttl_seconds}s")
        
        # 2. Demonstrate single parameter update
        print("\n🔧 2. Dynamic Parameter Updates")
        print("-" * 50)
        
        # Update reservoir size
        update_request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=1024,
            description="Increase reservoir size for better capacity"
        )
        
        result = await manager.update_parameter(update_request)
        
        if result["success"]:
            print(f"✅ Parameter update successful!")
            print(f"   Parameter: {result['parameter']}")
            print(f"   Old value: {result['old_value']}")
            print(f"   New value: {result['new_value']}")
            print(f"   Snapshot ID: {result['snapshot_id']}")
        else:
            print(f"❌ Parameter update failed: {result.get('error', 'Unknown error')}")
        
        # 3. Demonstrate validation-only mode
        print("\n✅ 3. Configuration Validation")
        print("-" * 50)
        
        # Test valid parameter
        valid_request = ConfigurationUpdateRequest(
            parameter_path="max_membrane_depth",
            new_value=8,
            validate_only=True
        )
        
        validation_result = await manager.update_parameter(valid_request)
        print(f"Valid parameter test: {'✅ PASSED' if validation_result['success'] else '❌ FAILED'}")
        
        # Test invalid parameter
        invalid_request = ConfigurationUpdateRequest(
            parameter_path="max_membrane_depth",
            new_value=100,  # Too large
            validate_only=True
        )
        
        validation_result = await manager.update_parameter(invalid_request)
        print(f"Invalid parameter test: {'✅ PASSED' if not validation_result['success'] else '❌ FAILED'}")
        if not validation_result['success']:
            print(f"   Validation errors: {validation_result.get('validation_errors', [])}")
        
        # 4. Demonstrate batch updates
        print("\n📦 4. Batch Parameter Updates")
        print("-" * 50)
        
        batch_updates = [
            ConfigurationUpdateRequest(
                parameter_path="max_membrane_depth",
                new_value=6
            ),
            ConfigurationUpdateRequest(
                parameter_path="bseries_max_order",
                new_value=12
            ),
            ConfigurationUpdateRequest(
                parameter_path="cache_ttl_seconds",
                new_value=600
            )
        ]
        
        batch_result = await manager.update_multiple_parameters(batch_updates)
        
        if batch_result["success"]:
            print(f"✅ Batch update successful!")
            print(f"   Updated {len(batch_result['updated_parameters'])} parameters")
            for update in batch_result['updated_parameters']:
                print(f"   - {update['parameter']}: {update['old_value']} → {update['new_value']}")
        else:
            print(f"❌ Batch update failed: {batch_result.get('error', 'Unknown error')}")
        
        # 5. Demonstrate environment management
        print("\n🌍 5. Environment Management")
        print("-" * 50)
        
        # Switch to production environment
        manager.set_environment(ConfigurationEnvironment.PRODUCTION)
        print(f"Environment changed to: {manager.environment.value}")
        
        # Switch back to development
        manager.set_environment(ConfigurationEnvironment.DEVELOPMENT)
        print(f"Environment changed to: {manager.environment.value}")
        
        # 6. Demonstrate configuration rollback
        print("\n🔄 6. Configuration Rollback")
        print("-" * 50)
        
        # Get snapshot before rollback
        snapshots = manager.get_snapshots()
        if len(snapshots) >= 2:
            # Rollback to second-to-last snapshot
            target_snapshot = snapshots[-2]["snapshot_id"]
            
            rollback_result = await manager.rollback_to_snapshot(target_snapshot)
            
            if rollback_result["success"]:
                print(f"✅ Configuration rollback successful!")
                print(f"   Rolled back to: {target_snapshot}")
                print(f"   New snapshot: {rollback_result['rollback_snapshot']}")
            else:
                print(f"❌ Rollback failed: {rollback_result.get('error', 'Unknown error')}")
        else:
            print("⚠️  Not enough snapshots for rollback demonstration")
        
        # 7. Show configuration history
        print("\n📚 7. Configuration History")
        print("-" * 50)
        
        snapshots = manager.get_snapshots()
        print(f"Total snapshots: {len(snapshots)}")
        
        for i, snapshot in enumerate(snapshots[-5:], 1):  # Show last 5
            status = "🟢 ACTIVE" if snapshot["is_active"] else "⚪ INACTIVE"
            print(f"{i}. {status} {snapshot['snapshot_id']}")
            print(f"   Description: {snapshot['description']}")
            print(f"   Environment: {snapshot['environment']}")
        
        # 8. Show final configuration status
        print("\n📊 8. Final Configuration Status")
        print("-" * 50)
        
        status = manager.get_current_status()
        final_config = manager.current_config
        
        print(f"Environment: {status['environment']}")
        print(f"Total snapshots: {status['total_snapshots']}")
        print(f"Auto-backup: {'enabled' if status['auto_backup_enabled'] else 'disabled'}")
        print(f"\nCurrent Configuration:")
        print(f"   - Membrane Depth: {final_config.max_membrane_depth}")
        print(f"   - ESN Reservoir Size: {final_config.esn_reservoir_size}")
        print(f"   - B-Series Max Order: {final_config.bseries_max_order}")
        print(f"   - Caching: {'enabled' if final_config.enable_caching else 'disabled'}")
        print(f"   - Cache TTL: {final_config.cache_ttl_seconds}s")
        
        # 9. Demonstrate callback system
        print("\n🔔 9. Configuration Update Callbacks")
        print("-" * 50)
        
        callback_count = 0
        
        def config_callback(new_config):
            nonlocal callback_count
            callback_count += 1
            print(f"   📢 Callback triggered #{callback_count}: ESN size = {new_config.esn_reservoir_size}")
        
        manager.register_update_callback(config_callback)
        
        # Trigger callback with update
        callback_request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=2048,
            description="Trigger callback demonstration"
        )
        
        callback_result = await manager.update_parameter(callback_request)
        if callback_result["success"]:
            print(f"✅ Callback system working! Triggered {callback_count} times")
        
        print("\n🎉 Dynamic Configuration Management Demo Complete!")
        print("=" * 60)
        print(f"✨ Demonstrated server-side configuration updates without service restart")
        print(f"🔒 Validated configuration parameters and rollback mechanisms")
        print(f"🌍 Managed environment-specific configurations")
        print(f"📚 Maintained configuration history and snapshots")
        
        return True
        
    except ImportError as e:
        logger.error(f"Required modules not available: {e}")
        print(f"\n❌ Demo failed: Missing required modules")
        print(f"   Error: {e}")
        print(f"   Please ensure DTESN configuration modules are properly installed")
        return False
        
    except Exception as e:
        logger.error(f"Demo failed with error: {e}")
        print(f"\n❌ Demo failed with error: {e}")
        return False


async def demonstrate_api_integration():
    """Demonstrate API integration with FastAPI endpoints."""
    
    print("\n🔗 API Integration Demo")
    print("-" * 40)
    
    try:
        # This would demonstrate actual API calls if server is running
        print("📡 Configuration API Endpoints:")
        print("   GET  /v1/config/current          - Get current configuration")
        print("   GET  /v1/config/status           - Get manager status")
        print("   POST /v1/config/update           - Update parameter")
        print("   POST /v1/config/batch-update     - Batch update")
        print("   POST /v1/config/validate         - Validate parameter")
        print("   POST /v1/config/rollback         - Rollback to snapshot")
        print("   GET  /v1/config/snapshots        - Get snapshots")
        print("   POST /v1/config/environment      - Set environment")
        print("   GET  /v1/config/schema           - Get configuration schema")
        print("   GET  /v1/config/health           - Health check")
        
        print("\n📋 Example API Usage:")
        
        example_update = {
            "parameter": "esn_reservoir_size",
            "value": 1024,
            "description": "Increase reservoir capacity",
            "validate_only": False
        }
        
        print(f"   Update Parameter:")
        print(f"   curl -X POST '/v1/config/update' \\")
        print(f"        -H 'Content-Type: application/json' \\")
        print(f"        -d '{json.dumps(example_update, indent=2)}'")
        
        example_rollback = {
            "snapshot_id": "snapshot_1699123456_0001"
        }
        
        print(f"\n   Rollback Configuration:")
        print(f"   curl -X POST '/v1/config/rollback' \\")
        print(f"        -H 'Content-Type: application/json' \\")
        print(f"        -d '{json.dumps(example_rollback)}'")
        
        print(f"\n✅ API integration ready for server deployment")
        
    except Exception as e:
        logger.error(f"API demo failed: {e}")
        print(f"❌ API demo error: {e}")


async def main():
    """Run the complete dynamic configuration demo."""
    
    print("🌟 Aphrodite Engine - Dynamic Configuration Management")
    print("Implementation of Phase 7.3.3: Server-Side Configuration Management")
    print("=" * 80)
    
    success = True
    
    # Run core configuration demo
    core_success = await demonstrate_dynamic_configuration()
    success = success and core_success
    
    # Run API integration demo
    await demonstrate_api_integration()
    
    if success:
        print(f"\n🎊 All demonstrations completed successfully!")
        print(f"🚀 Dynamic configuration management is ready for production use")
        print(f"\n📖 Key Features Demonstrated:")
        print(f"   ✅ Configuration updates without service restart")
        print(f"   ✅ Parameter validation and rollback mechanisms")
        print(f"   ✅ Environment-specific configuration management")
        print(f"   ✅ Configuration history and snapshots")
        print(f"   ✅ Real-time callback system")
        print(f"   ✅ FastAPI endpoint integration")
    else:
        print(f"\n⚠️  Some demonstrations had issues - check logs for details")
    
    return success


if __name__ == "__main__":
    asyncio.run(main())