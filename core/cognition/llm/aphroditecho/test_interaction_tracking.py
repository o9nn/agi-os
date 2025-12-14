#!/usr/bin/env python3
"""
Quick test of multi-agent interaction tracking
"""

import sys
import asyncio
import logging
from pathlib import Path

# Add the project root to Python path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))
sys.path.append('./echo.kern')

# Set up logging
logging.basicConfig(level=logging.WARNING)

async def test_interaction_tracking():
    """Test that agent interactions are properly tracked."""
    print("🧪 Testing Multi-Agent Interaction Tracking")
    print("=" * 50)
    
    from dtesn_multi_agent_training_integration import (
        DTESNMultiAgentTrainingSystem, DTESNTrainingConfiguration
    )
    from multi_agent_training_system import TrainingConfiguration, TrainingMode
    from population_based_training import PopulationConfig
    from cooperative_competitive_learning import LearningConfiguration
    
    # Create small test configuration
    config = DTESNTrainingConfiguration(
        training_config=TrainingConfiguration(
            population_size=8,
            training_mode=TrainingMode.HYBRID,
            episode_batch_size=6
        ),
        population_config=PopulationConfig(population_size=8),
        learning_config=LearningConfiguration(),
        enable_dtesn_monitoring=False,
        enable_aar_orchestration=False
    )
    
    system = DTESNMultiAgentTrainingSystem(config)
    await system.initialize_training_population()
    
    print(f"✓ Initialized system with {len(system.training_system.population)} agents")
    
    # Check initial interaction counts
    initial_interactions = [agent.interaction_count for agent in system.training_system.population.values()]
    print(f"  Initial interaction counts: {sum(initial_interactions)} total")
    
    # Run one training epoch
    print("🔄 Running training epoch...")
    epoch_results = await system.run_integrated_training_epoch()
    
    # Check final interaction counts
    final_interactions = [agent.interaction_count for agent in system.training_system.population.values()]
    total_final = sum(final_interactions)
    
    print("✓ Training epoch completed")
    print(f"  Final interaction counts: {total_final} total")
    print(f"  Interactions per agent: {[f'{agent_id}:{agent.interaction_count}' for agent_id, agent in list(system.training_system.population.items())[:3]]}")
    
    # Check learning phase results
    learning_phase = epoch_results['learning_phase']
    print(f"  Learning phase executed: {learning_phase['interactions_executed']} interactions")
    
    # Check population metrics
    pop_metrics = epoch_results['population_metrics']
    print(f"  Population metrics: {pop_metrics['total_interactions']} total interactions")
    print(f"  Average per agent: {pop_metrics['average_interactions_per_agent']:.1f}")
    
    # Validation
    success = total_final > 0 and learning_phase['interactions_executed'] > 0
    
    print(f"\n{'✅ SUCCESS' if success else '❌ FAILED'}: Interaction tracking {'works' if success else 'needs fixing'}")
    
    return success


async def main():
    try:
        success = await test_interaction_tracking()
        return 0 if success else 1
    except Exception as e:
        print(f"❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))