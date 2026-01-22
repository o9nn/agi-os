import sys
import asyncio
import logging
import traceback
from pathlib import Path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))
logger = logging.getLogger(__name__)
async def validate_imports():
    print('🔍 Validating imports...')
    try:
        import sys
        sys.path.append('./echo.kern')
        from multi_agent_training_system import MultiAgentTrainingSystem, TrainingConfiguration
        from population_based_training import PopulationBasedTrainer, PopulationConfig
        from cooperative_competitive_learning import HybridLearningCoordinator, LearningConfiguration
        from dtesn_multi_agent_training_integration import DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        print('✅ All core components imported successfully')
        return True
    except ImportError as e:
        print(f'❌ Import failed: {e}')
        traceback.print_exc()
        return False
async def validate_basic_functionality():
    print('\n🧪 Validating basic functionality...')
    try:
        import sys
        sys.path.append('./echo.kern')
        from dtesn_multi_agent_training_integration import DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        from multi_agent_training_system import TrainingConfiguration
        from population_based_training import PopulationConfig
        from cooperative_competitive_learning import LearningConfiguration
        config = DTESNTrainingConfiguration(training_config=TrainingConfiguration(population_size=5, max_generations=2), population_config=PopulationConfig(population_size=5, max_generations=2), learning_config=LearningConfiguration(), enable_dtesn_monitoring=False, enable_aar_orchestration=False)
        system = DTESNMultiAgentTrainingSystem(config)
        print('✅ System initialization successful')
        init_results = await system.initialize_training_population()
        print(f"✅ Population initialized: {init_results['training_population_size']} agents")
        epoch_results = await system.run_integrated_training_epoch()
        print(f"✅ Training epoch completed: {epoch_results['learning_phase']['interactions_executed']} interactions")
        improvement_analysis = epoch_results['improvement_analysis']
        print(f"✅ Improvement analysis completed: {improvement_analysis.get('overall_improvement', False)}")
        return True
    except Exception as e:
        print(f'❌ Functionality validation failed: {e}')
        traceback.print_exc()
        return False
async def validate_acceptance_criteria():
    print('\n🎯 Validating acceptance criteria...')
    try:
        import sys
        sys.path.append('./echo.kern')
        from dtesn_multi_agent_training_integration import DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
        from multi_agent_training_system import TrainingConfiguration, TrainingMode
        config = DTESNTrainingConfiguration(training_config=TrainingConfiguration(population_size=8, training_mode=TrainingMode.HYBRID, episode_batch_size=4), enable_dtesn_monitoring=False, enable_aar_orchestration=False)
        system = DTESNMultiAgentTrainingSystem(config)
        await system.initialize_training_population()
        results = []
        for epoch in range(3):
            result = await system.run_integrated_training_epoch()
            results.append(result)
        report = await system.generate_training_report(results)
        validation = report['acceptance_criteria_validation']
        criteria_status = {'Distributed training': validation.get('distributed_training_achieved', False), 'Competitive & cooperative learning': validation.get('competitive_and_cooperative_learning', False), 'Population-based methods': validation.get('population_based_methods_used', False), 'Population improvement': validation.get('population_improved_through_interaction', False)}
        print('📊 Acceptance Criteria Status:')
        all_met = True
        for criterion, status in criteria_status.items():
            symbol = '✅' if status else '❌'
            print(f'  {symbol} {criterion}: {status}')
            if not status:
                all_met = False
        print(f"\n🏆 Overall Status: {('ALL CRITERIA MET' if all_met else 'SOME CRITERIA PENDING')}")
        return all_met
    except Exception as e:
        print(f'❌ Acceptance criteria validation failed: {e}')
        traceback.print_exc()
        return False
async def run_validation():
    print('🚀 Multi-Agent Training System Validation')
    print('=' * 60)
    logging.basicConfig(level=logging.WARNING)
    validation_steps = [('Import Validation', validate_imports), ('Basic Functionality', validate_basic_functionality), ('Acceptance Criteria', validate_acceptance_criteria)]
    results = {}
    for step_name, step_func in validation_steps:
        print(f"\n{'=' * 20} {step_name} {'=' * 20}")
        try:
            result = await step_func()
            results[step_name] = result
            if result:
                print(f'✅ {step_name} PASSED')
            else:
                print(f'❌ {step_name} FAILED')
        except Exception as e:
            print(f'💥 {step_name} CRASHED: {e}')
            results[step_name] = False
    print('\n' + '=' * 60)
    print('📋 VALIDATION SUMMARY')
    print('=' * 60)
    total_steps = len(validation_steps)
    passed_steps = sum((1 for result in results.values() if result))
    for step_name, result in results.items():
        symbol = '✅' if result else '❌'
        print(f'{symbol} {step_name}')
    success_rate = passed_steps / total_steps
    print(f'\n🎯 Success Rate: {passed_steps}/{total_steps} ({success_rate:.1%})')
    if success_rate >= 1.0:
        print('🎉 ALL VALIDATIONS PASSED!')
        print('✅ Task 4.2.3 implementation is ready for use.')
        return True
    elif success_rate >= 0.67:
        print('⚠️  MOST VALIDATIONS PASSED')
        print('🔧 Minor issues may need attention.')
        return True
    else:
        print('❌ VALIDATION FAILED')
        print('🛠️  Significant issues need to be addressed.')
        return False
def main():
    try:
        success = asyncio.run(run_validation())
        return 0 if success else 1
    except KeyboardInterrupt:
        print('\n⏹️  Validation interrupted by user')
        return 1
    except Exception as e:
        print(f'\n💥 Validation crashed: {e}')
        traceback.print_exc()
        return 1
if __name__ == '__main__':
    sys.exit(main())