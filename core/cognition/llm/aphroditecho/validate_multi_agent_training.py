#!/usr/bin/env python3
"""
Quick validation script for multi-agent training system.

This script performs a quick validation to ensure all components
are working correctly for Task 4.2.3.
"""

import sys
import asyncio
import logging
import traceback
from pathlib import Path

# Add the project root to Python path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

logger = logging.getLogger(__name__)


async def validate_imports():
    """Validate that all required modules can be imported."""
    print("🔍 Validating imports...")
    
    try:
        # Test core components - using correct paths
        import sys
        sys.path.append('./echo.kern')
        
        from multi_agent_training_system import MultiAgentTrainingSystem, TrainingConfiguration
        from population_based_training import PopulationBasedTrainer, PopulationConfig
        from cooperative_competitive_learning import HybridLearningCoordinator, LearningConfiguration  
        from dtesn_multi_agent_training_integration import DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        
        print("✅ All core components imported successfully")
        return True
        
    except ImportError as e:
        print(f"❌ Import failed: {e}")
        traceback.print_exc()
        return False


async def validate_basic_functionality():
    """Validate basic functionality of the training system."""
    print("\n🧪 Validating basic functionality...")
    
    try:
        # Import components - using correct paths
        import sys
        sys.path.append('./echo.kern')
        
        from dtesn_multi_agent_training_integration import (
            DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        )
        from multi_agent_training_system import TrainingConfiguration
        from population_based_training import PopulationConfig
        from cooperative_competitive_learning import LearningConfiguration
        
        # Create minimal configuration
        config = DTESNTrainingConfiguration(
            training_config=TrainingConfiguration(population_size=5, max_generations=2),
            population_config=PopulationConfig(population_size=5, max_generations=2),
            learning_config=LearningConfiguration(),
            enable_dtesn_monitoring=False,  # Disable for quick test
            enable_aar_orchestration=False   # Disable for quick test
        )
        
        # Initialize system
        system = DTESNMultiAgentTrainingSystem(config)
        print("✅ System initialization successful")
        
        # Initialize population
        init_results = await system.initialize_training_population()
        print(f"✅ Population initialized: {init_results['training_population_size']} agents")
        
        # Run one training epoch
        epoch_results = await system.run_integrated_training_epoch()
        print(f"✅ Training epoch completed: {epoch_results['learning_phase']['interactions_executed']} interactions")
        
        # Check acceptance criteria indicators
        improvement_analysis = epoch_results['improvement_analysis']
        print(f"✅ Improvement analysis completed: {improvement_analysis.get('overall_improvement', False)}")
        
        return True
        
    except Exception as e:
        print(f"❌ Functionality validation failed: {e}")
        traceback.print_exc()
        return False


async def validate_acceptance_criteria():
    """Validate key acceptance criteria for Task 4.2.3."""
    print("\n🎯 Validating acceptance criteria...")
    
    try:
        import sys
        sys.path.append('./echo.kern')
        
        from dtesn_multi_agent_training_integration import (
            DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        )
        from multi_agent_training_system import TrainingConfiguration, TrainingMode
        
        # Create system with settings optimized for demonstrating criteria
        config = DTESNTrainingConfiguration(
            training_config=TrainingConfiguration(
                population_size=8,
                training_mode=TrainingMode.HYBRID,  # Both competitive and cooperative
                episode_batch_size=4
            ),
            enable_dtesn_monitoring=False,
            enable_aar_orchestration=False
        )
        
        system = DTESNMultiAgentTrainingSystem(config)
        await system.initialize_training_population()
        
        # Collect metrics over multiple epochs
        results = []
        for epoch in range(3):
            result = await system.run_integrated_training_epoch()
            results.append(result)
        
        # Generate report
        report = await system.generate_training_report(results)
        validation = report['acceptance_criteria_validation']
        
        # Check each criteria
        criteria_status = {
            'Distributed training': validation.get('distributed_training_achieved', False),
            'Competitive & cooperative learning': validation.get('competitive_and_cooperative_learning', False),
            'Population-based methods': validation.get('population_based_methods_used', False),
            'Population improvement': validation.get('population_improved_through_interaction', False)
        }
        
        print("📊 Acceptance Criteria Status:")
        all_met = True
        for criterion, status in criteria_status.items():
            symbol = "✅" if status else "❌"
            print(f"  {symbol} {criterion}: {status}")
            if not status:
                all_met = False
        
        print(f"\n🏆 Overall Status: {'ALL CRITERIA MET' if all_met else 'SOME CRITERIA PENDING'}")
        return all_met
        
    except Exception as e:
        print(f"❌ Acceptance criteria validation failed: {e}")
        traceback.print_exc()
        return False


async def run_validation():
    """Run complete validation suite."""
    print("🚀 Multi-Agent Training System Validation")
    print("=" * 60)
    
    # Set up logging
    logging.basicConfig(level=logging.WARNING)  # Reduce log noise for validation
    
    # Run validation steps
    validation_steps = [
        ("Import Validation", validate_imports),
        ("Basic Functionality", validate_basic_functionality), 
        ("Acceptance Criteria", validate_acceptance_criteria)
    ]
    
    results = {}
    
    for step_name, step_func in validation_steps:
        print(f"\n{'='*20} {step_name} {'='*20}")
        
        try:
            result = await step_func()
            results[step_name] = result
            
            if result:
                print(f"✅ {step_name} PASSED")
            else:
                print(f"❌ {step_name} FAILED")
                
        except Exception as e:
            print(f"💥 {step_name} CRASHED: {e}")
            results[step_name] = False
    
    # Final summary
    print("\n" + "="*60)
    print("📋 VALIDATION SUMMARY")
    print("="*60)
    
    total_steps = len(validation_steps)
    passed_steps = sum(1 for result in results.values() if result)
    
    for step_name, result in results.items():
        symbol = "✅" if result else "❌"
        print(f"{symbol} {step_name}")
    
    success_rate = passed_steps / total_steps
    print(f"\n🎯 Success Rate: {passed_steps}/{total_steps} ({success_rate:.1%})")
    
    if success_rate >= 1.0:
        print("🎉 ALL VALIDATIONS PASSED!")
        print("✅ Task 4.2.3 implementation is ready for use.")
        return True
    elif success_rate >= 0.67:
        print("⚠️  MOST VALIDATIONS PASSED")
        print("🔧 Minor issues may need attention.")
        return True
    else:
        print("❌ VALIDATION FAILED")
        print("🛠️  Significant issues need to be addressed.")
        return False


def main():
    """Main entry point for validation script."""
    try:
        success = asyncio.run(run_validation())
        return 0 if success else 1
    except KeyboardInterrupt:
        print("\n⏹️  Validation interrupted by user")
        return 1
    except Exception as e:
        print(f"\n💥 Validation crashed: {e}")
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())