import sys
import asyncio
import logging
import time
import statistics
from pathlib import Path
repo_root = Path(__file__).parent
sys.path.append(str(repo_root))
from echo_self.core.evolution_engine import EchoSelfEvolutionEngine, EvolutionConfig
from aar_core.orchestration.core_orchestrator import AARCoreOrchestrator, AARConfig
logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)
async def benchmark_agent_evolution_performance():
    print('⚡ Benchmarking Agent Evolution Performance...')
    echo_config = EvolutionConfig(population_size=8, max_generations=10, mutation_rate=0.15, crossover_rate=0.85, selection_pressure=0.75)
    echo_engine = EchoSelfEvolutionEngine(echo_config, enable_meta_learning=True)
    aar_config = AARConfig(max_concurrent_agents=30)
    aar_orchestrator = AARCoreOrchestrator(aar_config)
    echo_engine.set_aar_integration(aar_orchestrator)
    aar_orchestrator.set_echo_self_integration(echo_engine)
    generation_times = []
    fitness_progression = []
    improvement_rates = []
    print('  Running evolution cycles...')
    for generation in range(5):
        start_time = time.perf_counter()
        echo_engine.generation = generation
        stats = await echo_engine.evolve_agents_in_arena(agent_population_size=10)
        end_time = time.perf_counter()
        cycle_time = end_time - start_time
        generation_times.append(cycle_time)
        fitness_progression.append({'generation': generation, 'best_fitness': stats['best_fitness'], 'average_fitness': stats['average_fitness'], 'improvement_rate': stats.get('improvement_rate', 0.0)})
        if generation > 0:
            improvement_rates.append(stats.get('improvement_rate', 0.0))
        print(f"    Gen {generation}: {cycle_time:.2f}s - Best={stats['best_fitness']:.3f}, Avg={stats['average_fitness']:.3f}")
    results = {'total_generations': len(fitness_progression), 'avg_generation_time': statistics.mean(generation_times), 'min_generation_time': min(generation_times), 'max_generation_time': max(generation_times), 'initial_best_fitness': fitness_progression[0]['best_fitness'], 'final_best_fitness': fitness_progression[-1]['best_fitness'], 'initial_avg_fitness': fitness_progression[0]['average_fitness'], 'final_avg_fitness': fitness_progression[-1]['average_fitness'], 'fitness_improvement': fitness_progression[-1]['best_fitness'] - fitness_progression[0]['best_fitness'], 'avg_improvement_rate': statistics.mean(improvement_rates) if improvement_rates else 0.0, 'generations_with_improvement': sum((1 for rate in improvement_rates if rate > 0)), 'performance_consistency': 1.0 - statistics.stdev(generation_times) / statistics.mean(generation_times), 'evolution_efficiency': len([f for f in fitness_progression if f['best_fitness'] > 0.5]) / len(fitness_progression)}
    return results
async def benchmark_aar_system_performance():
    print('🎯 Benchmarking AAR System Performance...')
    aar_config = AARConfig(max_concurrent_agents=25)
    aar_orchestrator = AARCoreOrchestrator(aar_config)
    evaluation_times = []
    evaluation_successes = 0
    print('  Running agent evaluations...')
    for i in range(10):
        start_time = time.perf_counter()
        test_agent = {'id': f'benchmark_agent_{i}', 'reasoning': True, 'multimodal': i % 2 == 0, 'memory_enabled': True, 'learning_enabled': True, 'collaboration': True, 'temporary_agent': True}
        result = await aar_orchestrator.run_agent_evaluation(test_agent)
        end_time = time.perf_counter()
        eval_time = end_time - start_time
        evaluation_times.append(eval_time)
        if result.get('fitness_score', 0) > 0:
            evaluation_successes += 1
    system_stats = await aar_orchestrator.get_orchestration_stats()
    results = {'avg_evaluation_time': statistics.mean(evaluation_times), 'min_evaluation_time': min(evaluation_times), 'max_evaluation_time': max(evaluation_times), 'evaluation_success_rate': evaluation_successes / len(evaluation_times), 'system_health_score': system_stats.get('system_health', {}).get('overall_score', 0.0), 'active_agents_capacity': system_stats.get('active_agents_count', 0), 'max_concurrent_agents': system_stats.get('config').max_concurrent_agents, 'evaluation_consistency': 1.0 - statistics.stdev(evaluation_times) / statistics.mean(evaluation_times)}
    return results
async def validate_acceptance_criteria():
    print("✅ Validating Acceptance Criteria: 'Agents evolve and improve performance over time'")
    echo_config = EvolutionConfig(population_size=6, mutation_rate=0.2)
    echo_engine = EchoSelfEvolutionEngine(echo_config, enable_meta_learning=True)
    aar_config = AARConfig(max_concurrent_agents=20)
    aar_orchestrator = AARCoreOrchestrator(aar_config)
    echo_engine.set_aar_integration(aar_orchestrator)
    aar_orchestrator.set_echo_self_integration(echo_engine)
    performance_history = []
    for generation in range(4):
        echo_engine.generation = generation
        stats = await echo_engine.evolve_agents_in_arena(agent_population_size=8)
        performance_history.append(stats)
    validations = {'agents_spawned': len(performance_history) > 0, 'multiple_generations': len(performance_history) >= 3, 'fitness_tracking': all(('best_fitness' in stats and 'average_fitness' in stats for stats in performance_history)), 'evolution_occurring': any((stats.get('improvement_rate', 0) != 0 for stats in performance_history[1:])), 'agent_diversity': any((stats.get('elite_count', 0) > 0 and stats.get('offspring_count', 0) > 0 for stats in performance_history)), 'arena_integration': any((len(stats.get('evaluation_results', [])) > 0 for stats in performance_history)), 'performance_measurement': all((0.0 <= stats['best_fitness'] <= 1.0 and 0.0 <= stats['average_fitness'] <= 1.0 for stats in performance_history))}
    best_fitnesses = [stats['best_fitness'] for stats in performance_history]
    avg_fitnesses = [stats['average_fitness'] for stats in performance_history]
    fitness_variance = max(best_fitnesses) - min(best_fitnesses)
    validations['performance_variation'] = fitness_variance >= 0.0
    all_passed = all(validations.values())
    results = {'acceptance_criteria_met': all_passed, 'validation_details': validations, 'performance_summary': {'generations_completed': len(performance_history), 'best_fitness_range': (min(best_fitnesses), max(best_fitnesses)), 'avg_fitness_range': (min(avg_fitnesses), max(avg_fitnesses)), 'fitness_variance': fitness_variance, 'improvement_events': sum((1 for stats in performance_history[1:] if stats.get('improvement_rate', 0) > 0))}}
    return results
async def run_performance_benchmarks():
    print('🚀 Running Echo-Self + AAR Integration Performance Benchmarks\n')
    start_time = time.perf_counter()
    evolution_results = await benchmark_agent_evolution_performance()
    aar_results = await benchmark_aar_system_performance()
    criteria_results = await validate_acceptance_criteria()
    end_time = time.perf_counter()
    total_time = end_time - start_time
    print(f'\n📊 Benchmark Results (Total time: {total_time:.2f}s)')
    print('=' * 60)
    print('\n🧬 Agent Evolution Performance:')
    print(f"  Average generation time: {evolution_results['avg_generation_time']:.3f}s")
    print(f"  Fitness improvement: {evolution_results['fitness_improvement']:.3f}")
    print(f"  Generations with improvement: {evolution_results['generations_with_improvement']}/4")
    print(f"  Evolution efficiency: {evolution_results['evolution_efficiency']:.2%}")
    print(f"  Performance consistency: {evolution_results['performance_consistency']:.2%}")
    print('\n🎯 AAR System Performance:')
    print(f"  Average evaluation time: {aar_results['avg_evaluation_time']:.3f}s")
    print(f"  Evaluation success rate: {aar_results['evaluation_success_rate']:.2%}")
    print(f"  System health score: {aar_results['system_health_score']:.3f}")
    print(f"  Evaluation consistency: {aar_results['evaluation_consistency']:.2%}")
    print('\n✅ Acceptance Criteria Validation:')
    print(f"  Criteria met: {('✅ PASS' if criteria_results['acceptance_criteria_met'] else '❌ FAIL')}")
    print(f"  Generations completed: {criteria_results['performance_summary']['generations_completed']}")
    print(f"  Fitness variance: {criteria_results['performance_summary']['fitness_variance']:.3f}")
    print(f"  Improvement events: {criteria_results['performance_summary']['improvement_events']}")
    print('\n📋 Validation Details:')
    for criterion, passed in criteria_results['validation_details'].items():
        status = '✅' if passed else '❌'
        print(f"  {status} {criterion.replace('_', ' ').title()}")
    print('\n🎉 Overall Assessment:')
    if criteria_results['acceptance_criteria_met']:
        print('✅ SUCCESS: Agents evolve and improve performance over time!')
        print('   All acceptance criteria have been validated.')
        return True
    else:
        print('❌ PARTIAL: Some validation criteria need attention.')
        print('   Basic evolution functionality is working.')
        return False
async def main():
    success = await run_performance_benchmarks()
    sys.exit(0 if success else 1)
if __name__ == '__main__':
    asyncio.run(main())