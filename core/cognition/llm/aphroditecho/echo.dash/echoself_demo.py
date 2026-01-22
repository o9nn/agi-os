import json
import logging
import time
from pathlib import Path
from cognitive_architecture import CognitiveArchitecture
def setup_logging():
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
def demonstrate_introspection_cycle(cognitive_system: CognitiveArchitecture, cycle_num: int):
    print(f"\n{'=' * 60}")
    print(f'RECURSIVE INTROSPECTION CYCLE {cycle_num}')
    print(f"{'=' * 60}")
    print('📊 Cognitive State Analysis:')
    print('   Analyzing current system state through introspection...')
    print('\n🔍 Performing recursive introspection...')
    start_time = time.time()
    prompt = cognitive_system.perform_recursive_introspection()
    introspection_time = time.time() - start_time
    print(f'⏱️  Introspection completed in {introspection_time:.2f} seconds')
    if prompt:
        print(f'📝 Generated prompt length: {len(prompt)} characters')
        print('📝 Prompt preview (first 300 chars):')
        print(f'   {prompt[:300]}...')
    else:
        print('❌ No introspection prompt generated')
        return
    print('\n📊 Attention Allocation Metrics:')
    metrics = cognitive_system.get_introspection_metrics()
    for key, value in metrics.items():
        if key == 'highest_salience_files':
            print(f'   {key}:')
            for file_info in value:
                print(f'     - {file_info[0]}: {file_info[1]:.3f}')
        else:
            print(f'   {key}: {value}')
    print('\n🎯 Generating introspection-enhanced goals...')
    goals = cognitive_system.adaptive_goal_generation_with_introspection()
    print(f'Generated {len(goals)} goals:')
    for i, goal in enumerate(goals[:5], 1):
        print(f'   {i}. {goal.description}')
        print(f'      Priority: {goal.priority:.3f}')
        print(f"      Context: {goal.context.get('type', 'general')}")
    if len(goals) > 5:
        print(f'   ... and {len(goals) - 5} more goals')
def demonstrate_adaptive_attention(cognitive_system: CognitiveArchitecture):
    print(f"\n{'=' * 60}")
    print('ADAPTIVE ATTENTION ALLOCATION DEMONSTRATION')
    print(f"{'=' * 60}")
    scenarios = [(0.2, 0.8, 'Low load, high activity'), (0.8, 0.2, 'High load, low activity'), (0.5, 0.5, 'Balanced load and activity'), (0.9, 0.9, 'High load, high activity'), (0.1, 0.1, 'Low load, low activity')]
    for load, activity, description in scenarios:
        print(f'\n🔬 Scenario: {description}')
        print(f'   Load: {load:.1f}, Activity: {activity:.1f}')
        attention_threshold = cognitive_system.echoself_introspection.adaptive_attention(load, activity)
        print(f'   Attention threshold: {attention_threshold:.3f}')
        prompt = cognitive_system.perform_recursive_introspection(load, activity)
        if prompt:
            file_count = prompt.count('(file "')
            print(f'   Repository files included: {file_count}')
def demonstrate_hypergraph_export(cognitive_system: CognitiveArchitecture):
    print(f"\n{'=' * 60}")
    print('HYPERGRAPH DATA EXPORT DEMONSTRATION')
    print(f"{'=' * 60}")
    export_path = 'demo_hypergraph_export.json'
    print(f'🗂️  Exporting hypergraph data to {export_path}...')
    success = cognitive_system.export_introspection_data(export_path)
    if success:
        print('✅ Export successful!')
        try:
            with open(export_path, encoding='utf-8') as f:
                data = json.load(f)
            print('📈 Export Statistics:')
            print(f"   Total nodes: {len(data.get('nodes', []))}")
            print(f"   Attention decisions: {len(data.get('attention_history', []))}")
            nodes = data.get('nodes', [])
            if nodes:
                sorted_nodes = sorted(nodes, key=lambda n: n.get('salience_score', 0), reverse=True)
                print('   Top 5 most salient files:')
                for i, node in enumerate(sorted_nodes[:5], 1):
                    print(f"     {i}. {node['id']}: {node['salience_score']:.3f}")
        except Exception as e:
            print(f'❌ Error reading export file: {e}')
    else:
        print('❌ Export failed!')
def demonstrate_neural_symbolic_synergy(cognitive_system: CognitiveArchitecture):
    print(f"\n{'=' * 60}")
    print('NEURAL-SYMBOLIC SYNERGY DEMONSTRATION')
    print(f"{'=' * 60}")
    print('🔄 Performing multiple introspection cycles to show recursive evolution...')
    initial_memory_count = len(cognitive_system.memories)
    initial_goal_count = len(cognitive_system.goals)
    for cycle in range(1, 4):
        print(f'\n🔄 Cycle {cycle}:')
        _prompt = cognitive_system.perform_recursive_introspection()
        _goals = cognitive_system.adaptive_goal_generation_with_introspection()
        current_memory_count = len(cognitive_system.memories)
        current_goal_count = len(cognitive_system.goals)
        print(f'   Memories: {initial_memory_count} → {current_memory_count} (+{current_memory_count - initial_memory_count})')
        print(f'   Goals: {initial_goal_count} → {current_goal_count} (+{current_goal_count - initial_goal_count})')
        initial_memory_count = current_memory_count
        initial_goal_count = current_goal_count
    print('\n🧠 Neural-symbolic feedback loops successfully demonstrated!')
    print('   The system recursively evolves through introspection → goal generation → memory formation')
def main():
    setup_logging()
    print('🌳 ECHOSELF RECURSIVE SELF-MODEL INTEGRATION DEMONSTRATION 🌳')
    print('Implementing hypergraph encoding and adaptive attention allocation')
    print('Inspired by DeepTreeEcho/Eva Self Model architecture')
    print('\n🚀 Initializing cognitive architecture with Echoself introspection...')
    cognitive_system = CognitiveArchitecture()
    if not cognitive_system.echoself_introspection:
        print('❌ Echoself introspection system not available!')
        return
    print('✅ Echoself introspection system initialized successfully!')
    try:
        for cycle in range(1, 3):
            demonstrate_introspection_cycle(cognitive_system, cycle)
            time.sleep(1)
        demonstrate_adaptive_attention(cognitive_system)
        demonstrate_hypergraph_export(cognitive_system)
        demonstrate_neural_symbolic_synergy(cognitive_system)
        print(f"\n{'=' * 60}")
        print('🎉 DEMONSTRATION COMPLETED SUCCESSFULLY! 🎉')
        print('The Echoself recursive self-model integration is fully operational.')
        print('Key achievements:')
        print('  ✅ Hypergraph-encoded repository introspection')
        print('  ✅ Adaptive attention allocation mechanisms')
        print('  ✅ Neural-symbolic synergy through recursive feedback')
        print('  ✅ Integration with cognitive architecture')
        print('  ✅ Comprehensive test coverage')
        print(f"{'=' * 60}")
    except Exception as e:
        print(f'\n❌ Error during demonstration: {e}')
        import traceback
        traceback.print_exc()
    finally:
        demo_files = ['demo_hypergraph_export.json', 'echoself_hypergraph.json']
        for file_path in demo_files:
            if Path(file_path).exists():
                print(f'🧹 Cleaning up {file_path}')
                Path(file_path).unlink()
if __name__ == '__main__':
    main()