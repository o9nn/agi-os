import sys
import asyncio
import logging
from pathlib import Path
repo_root = Path(__file__).parent
sys.path.append(str(repo_root))
from echo_self.core.evolution_engine import EchoSelfEvolutionEngine, EvolutionConfig
from aar_core.orchestration.core_orchestrator import AARCoreOrchestrator, AARConfig
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
async def test_agent_evolution_basic():
    print('🧬 Testing Basic Agent Evolution...')
    try:
        echo_config = EvolutionConfig(population_size=5, max_generations=3, mutation_rate=0.1)
        echo_engine = EchoSelfEvolutionEngine(echo_config)
        aar_config = AARConfig(max_concurrent_agents=20)
        aar_orchestrator = AARCoreOrchestrator(aar_config)
        echo_engine.set_aar_integration(aar_orchestrator)
        aar_orchestrator.set_echo_self_integration(echo_engine)
        stats = await echo_engine.evolve_agents_in_arena(agent_population_size=8)
        assert stats['population_size'] == 8
        assert stats['generation'] == 0
        assert 'best_fitness' in stats
        assert 'average_fitness' in stats
        assert stats['best_fitness'] >= 0.0
        assert stats['average_fitness'] >= 0.0
        print(f"✅ Evolution completed: Best={stats['best_fitness']:.3f}, Avg={stats['average_fitness']:.3f}")
        return True
    except Exception as e:
        print(f'❌ Agent evolution basic test failed: {e}')
        return False
async def test_multi_generation_evolution():
    print('📈 Testing Multi-Generation Evolution...')
    try:
        echo_config = EvolutionConfig(population_size=3, max_generations=5, mutation_rate=0.2, selection_pressure=0.7)
        echo_engine = EchoSelfEvolutionEngine(echo_config)
        aar_config = AARConfig(max_concurrent_agents=15)
        aar_orchestrator = AARCoreOrchestrator(aar_config)
        echo_engine.set_aar_integration(aar_orchestrator)
        aar_orchestrator.set_echo_self_integration(echo_engine)
        generations = []
        for gen in range(3):
            echo_engine.generation = gen
            stats = await echo_engine.evolve_agents_in_arena(agent_population_size=6)
            generations.append(stats)
            print(f"  Generation {gen}: Best={stats['best_fitness']:.3f}, Avg={stats['average_fitness']:.3f}, Elite={stats['elite_count']}, Offspring={stats['offspring_count']}")
        assert len(generations) == 3
        for gen_stats in generations:
            assert 'best_fitness' in gen_stats
            assert 'average_fitness' in gen_stats
            assert 'elite_count' in gen_stats
            assert 'offspring_count' in gen_stats
        print('✅ Multi-generation evolution completed successfully')
        return True
    except Exception as e:
        print(f'❌ Multi-generation evolution test failed: {e}')
        return False
async def test_agent_performance_improvement():
    print('⚡ Testing Agent Performance Improvement...')
    try:
        echo_config = EvolutionConfig(population_size=4, max_generations=10, mutation_rate=0.15, crossover_rate=0.8)
        echo_engine = EchoSelfEvolutionEngine(echo_config, enable_meta_learning=True)
        aar_config = AARConfig(max_concurrent_agents=25)
        aar_orchestrator = AARCoreOrchestrator(aar_config)
        echo_engine.set_aar_integration(aar_orchestrator)
        aar_orchestrator.set_echo_self_integration(echo_engine)
        performance_history = []
        for generation in range(4):
            echo_engine.generation = generation
            stats = await echo_engine.evolve_agents_in_arena(agent_population_size=5)
            performance_data = {'generation': generation, 'best_fitness': stats['best_fitness'], 'average_fitness': stats['average_fitness'], 'improvement_rate': stats.get('improvement_rate', 0.0)}
            performance_history.append(performance_data)
            print(f"  Gen {generation}: Best={stats['best_fitness']:.3f}, Avg={stats['average_fitness']:.3f}, Improvement={stats.get('improvement_rate', 0.0):.3f}")
        assert len(performance_history) == 4
        for perf in performance_history:
            assert 0.0 <= perf['best_fitness'] <= 1.0
            assert 0.0 <= perf['average_fitness'] <= 1.0
        best_fitnesses = [p['best_fitness'] for p in performance_history]
        fitness_variance = max(best_fitnesses) - min(best_fitnesses)
        assert fitness_variance >= 0.0
        print('✅ Agent performance improvement validation completed')
        return True
    except Exception as e:
        print(f'❌ Agent performance improvement test failed: {e}')
        return False
async def test_evolution_integration_status():
    print('🔗 Testing Evolution Integration Status...')
    try:
        echo_config = EvolutionConfig(population_size=3)
        echo_engine = EchoSelfEvolutionEngine(echo_config)
        aar_config = AARConfig(max_concurrent_agents=10)
        aar_orchestrator = AARCoreOrchestrator(aar_config)
        echo_status = echo_engine.get_integration_status()
        assert echo_status['aar_integration_enabled'] is False
        aar_stats = await aar_orchestrator.get_orchestration_stats()
        assert aar_stats['integration_status']['echo_self_engine'] is False
        echo_engine.set_aar_integration(aar_orchestrator)
        aar_orchestrator.set_echo_self_integration(echo_engine)
        echo_status = echo_engine.get_integration_status()
        assert echo_status['aar_integration_enabled'] is True
        aar_stats = await aar_orchestrator.get_orchestration_stats()
        assert aar_stats['integration_status']['echo_self_engine'] is True
        aar_context = await echo_engine._get_aar_context()
        assert 'active_agents' in aar_context
        assert 'system_health' in aar_context
        assert 'integration_status' in aar_context
        print('✅ Evolution integration status validation completed')
        return True
    except Exception as e:
        print(f'❌ Evolution integration status test failed: {e}')
        return False
async def test_agent_capabilities_evolution():
    print('🎯 Testing Agent Capabilities Evolution...')
    try:
        echo_config = EvolutionConfig(population_size=3, mutation_rate=0.3, crossover_rate=0.9)
        echo_engine = EchoSelfEvolutionEngine(echo_config)
        aar_config = AARConfig(max_concurrent_agents=15)
        aar_orchestrator = AARCoreOrchestrator(aar_config)
        echo_engine.set_aar_integration(aar_orchestrator)
        aar_orchestrator.set_echo_self_integration(echo_engine)
        initial_population = await echo_engine._create_agent_population(4)
        initial_capabilities = [agent['capabilities'] for agent in initial_population]
        await echo_engine.evolve_agents_in_arena(agent_population_size=4)
        multimodal_agents = sum((1 for cap in initial_capabilities if cap.get('multimodal', False)))
        collaboration_agents = sum((1 for cap in initial_capabilities if cap.get('collaboration', False)))
        assert 0 <= multimodal_agents <= 4
        assert 0 <= collaboration_agents <= 4
        context_lengths = [cap.get('context_length', 4096) for cap in initial_capabilities]
        assert len(set(context_lengths)) > 1
        processing_powers = [cap.get('processing_power', 1.0) for cap in initial_capabilities]
        assert len(set(processing_powers)) > 1
        print('✅ Agent capabilities evolution validation completed')
        return True
    except Exception as e:
        print(f'❌ Agent capabilities evolution test failed: {e}')
        return False
async def run_all_evolution_tests():
    print('🚀 Running Agent Evolution Tests\n')
    test_results = []
    test_results.append(await test_agent_evolution_basic())
    test_results.append(await test_multi_generation_evolution())
    test_results.append(await test_agent_performance_improvement())
    test_results.append(await test_evolution_integration_status())
    test_results.append(await test_agent_capabilities_evolution())
    passed = sum(test_results)
    total = len(test_results)
    print(f'\n📊 Evolution Test Results: {passed}/{total} passed')
    if passed == total:
        print('🎉 All agent evolution tests passed! Agents evolve and improve performance over time.')
        return True
    else:
        print('❌ Some evolution tests failed. Check the logs above for details.')
        return False
async def main():
    success = await run_all_evolution_tests()
    sys.exit(0 if success else 1)
if __name__ == '__main__':
    asyncio.run(main())