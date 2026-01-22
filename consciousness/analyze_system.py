import os
import json
import re
from pathlib import Path
from collections import defaultdict
class Echo9llamaAnalyzer:
    def __init__(self, repo_path):
        self.repo_path = Path(repo_path)
        self.issues = defaultdict(list)
        self.improvements = defaultdict(list)
        self.stats = defaultdict(int)
    def analyze(self):
        print('🔍 Starting Echo9llama System Analysis...')
        print('=' * 60)
        self.check_core_systems()
        self.check_integration_completeness()
        self.check_persistence_layer()
        self.check_llm_providers()
        self.check_hypergraph_implementation()
        self.check_echobeats_architecture()
        self.check_echodream_system()
        self.check_skill_learning()
        self.check_goal_orchestration()
        self.check_consciousness_layers()
        self.generate_report()
    def check_core_systems(self):
        print('\n📦 Checking Core Systems...')
        core_systems = ['deeptreeecho', 'echobeats', 'echodream', 'echoself', 'consciousness', 'memory', 'goals', 'skills', 'wisdom', 'llm']
        core_path = self.repo_path / 'core'
        for system in core_systems:
            system_path = core_path / system
            if system_path.exists():
                self.stats[f'core_{system}_exists'] = 1
                go_files = list(system_path.glob('*.go'))
                self.stats[f'core_{system}_files'] = len(go_files)
                print(f'  ✓ {system}: {len(go_files)} files')
            else:
                self.issues['missing_core_systems'].append(system)
                print(f'  ✗ {system}: MISSING')
    def check_integration_completeness(self):
        print('\n🔗 Checking Integration Completeness...')
        unified_path = self.repo_path / 'core/deeptreeecho/unified_autonomous_echoself.go'
        if not unified_path.exists():
            self.issues['integration'].append('UnifiedAutonomousEchoself not found')
            return
        content = unified_path.read_text()
        required_integrations = {'wakeRestManager': 'AutonomousWakeRestManager', 'consciousnessLayers': 'ConsciousnessLayerCommunication', 'goalOrchestrator': 'GoalOrchestrator', 'echobeatsScheduler': 'EchobeatsScheduler', 'echodreamSystem': 'EchodreamKnowledgeIntegration', 'interestPatterns': 'InterestPatternSystem'}
        for field, type_name in required_integrations.items():
            if field in content and type_name in content:
                print(f'  ✓ {field} integrated')
                self.stats[f'integration_{field}'] = 1
            else:
                print(f'  ✗ {field} missing or incomplete')
                self.issues['integration'].append(f'{field} not properly integrated')
    def check_persistence_layer(self):
        print('\n💾 Checking Persistence Layer...')
        persistence_files = ['core/deeptreeecho/persistent_consciousness_state.go', 'core/deeptreeecho/supabase_persistence.go', 'core/persistence']
        for pfile in persistence_files:
            ppath = self.repo_path / pfile
            if ppath.exists():
                print(f'  ✓ {pfile}')
                self.stats['persistence_components'] += 1
            else:
                print(f'  ✗ {pfile} missing')
        unified_path = self.repo_path / 'core/deeptreeecho/unified_autonomous_echoself.go'
        if unified_path.exists():
            content = unified_path.read_text()
            if 'persistence' not in content.lower() and 'save' not in content.lower():
                self.improvements['persistence'].append('UnifiedAutonomousEchoself does not save/load state - no true persistence')
                print(f'  ⚠️  No persistence integration in UnifiedAutonomousEchoself')
    def check_llm_providers(self):
        print('\n🤖 Checking LLM Providers...')
        providers = {'anthropic_provider.go': 'Anthropic', 'openai_provider.go': 'OpenAI', 'openrouter_provider.go': 'OpenRouter', 'multi_provider_llm.go': 'Multi-Provider'}
        deeptree_path = self.repo_path / 'core/deeptreeecho'
        for filename, name in providers.items():
            if (deeptree_path / filename).exists():
                print(f'  ✓ {name} provider')
                self.stats[f'llm_provider_{name}'] = 1
            else:
                print(f'  ✗ {name} provider missing')
        unified_path = self.repo_path / 'core/deeptreeecho/unified_autonomous_echoself.go'
        if unified_path.exists():
            content = unified_path.read_text()
            if 'llmProvider' in content:
                print(f'  ✓ LLM provider integrated in UnifiedAutonomousEchoself')
            else:
                self.issues['llm'].append('LLM provider not used in UnifiedAutonomousEchoself')
    def check_hypergraph_implementation(self):
        print('\n🕸️  Checking Hypergraph Memory...')
        memory_path = self.repo_path / 'core/memory'
        if not memory_path.exists():
            self.issues['hypergraph'].append('Memory module directory missing')
            print(f'  ✗ Memory module missing')
            return
        go_files = list(memory_path.glob('*.go'))
        print(f'  ✓ Memory module exists: {len(go_files)} files')
        has_hyperedges = False
        has_atomspace = False
        for gf in go_files:
            content = gf.read_text()
            if 'hyperedge' in content.lower() or 'hyperlink' in content.lower():
                has_hyperedges = True
            if 'atomspace' in content.lower() or 'atom' in content.lower():
                has_atomspace = True
        if has_hyperedges:
            print(f'  ✓ Hyperedge support detected')
        else:
            self.improvements['hypergraph'].append('No hyperedge implementation found - using simple graph')
            print(f'  ⚠️  No hyperedge support - needs true hypergraph')
        if has_atomspace:
            print(f'  ✓ AtomSpace integration detected')
        else:
            self.improvements['hypergraph'].append('No AtomSpace integration - missing OpenCog foundation')
            print(f'  ⚠️  No AtomSpace - missing OpenCog integration')
    def check_echobeats_architecture(self):
        print('\n🎵 Checking Echobeats Architecture...')
        echobeats_path = self.repo_path / 'core/deeptreeecho/echobeats_scheduler.go'
        if not echobeats_path.exists():
            self.issues['echobeats'].append('Echobeats scheduler not found')
            print(f'  ✗ Echobeats scheduler missing')
            return
        content = echobeats_path.read_text()
        if 'engine1' in content and 'engine2' in content and ('engine3' in content):
            print(f'  ✓ 3 concurrent inference engines')
            self.stats['echobeats_engines'] = 3
        else:
            self.improvements['echobeats'].append('Missing 3 concurrent inference engines')
            print(f'  ⚠️  3 concurrent engines not fully implemented')
        step_count = content.count('step') + content.count('Step')
        print(f'  ℹ️  Step references found: {step_count}')
        phases = ['expressive', 'reflective', 'anticipatory']
        phase_count = sum((1 for p in phases if p in content.lower()))
        print(f'  ℹ️  Phases detected: {phase_count}/3')
        if phase_count < 3:
            self.improvements['echobeats'].append('Missing full 3-phase structure (expressive, reflective, anticipatory)')
    def check_echodream_system(self):
        print('\n🌙 Checking Echodream System...')
        echodream_path = self.repo_path / 'core/deeptreeecho/echodream_knowledge_integration.go'
        if not echodream_path.exists():
            self.issues['echodream'].append('Echodream system not found')
            print(f'  ✗ Echodream system missing')
            return
        content = echodream_path.read_text()
        features = {'ConsolidateKnowledge': 'Memory consolidation', 'ExtractWisdom': 'Wisdom extraction', 'PatternExtraction': 'Pattern extraction', 'MemoryPruning': 'Memory pruning'}
        for func, desc in features.items():
            if func in content:
                print(f'  ✓ {desc}')
                self.stats[f'echodream_{func}'] = 1
            else:
                print(f'  ⚠️  {desc} missing')
                self.improvements['echodream'].append(f'Missing {desc} function')
    def check_skill_learning(self):
        print('\n🎯 Checking Skill Learning System...')
        skills_path = self.repo_path / 'core/skills'
        if not skills_path.exists():
            self.issues['skills'].append('Skills module missing')
            print(f'  ✗ Skills module missing')
            return
        go_files = list(skills_path.glob('*.go'))
        print(f'  ✓ Skills module exists: {len(go_files)} files')
        unified_path = self.repo_path / 'core/deeptreeecho/unified_autonomous_echoself.go'
        if unified_path.exists():
            content = unified_path.read_text()
            if 'skill' not in content.lower():
                self.improvements['skills'].append('Skills not integrated into UnifiedAutonomousEchoself')
                print(f'  ⚠️  Skills not integrated into autonomous agent')
            else:
                print(f'  ✓ Skills integrated')
    def check_goal_orchestration(self):
        print('\n🎯 Checking Goal Orchestration...')
        goal_path = self.repo_path / 'core/deeptreeecho/goal_orchestrator.go'
        if not goal_path.exists():
            self.issues['goals'].append('Goal orchestrator not found')
            print(f'  ✗ Goal orchestrator missing')
            return
        content = goal_path.read_text()
        features = {'CreateGoal': 'Goal creation', 'DecomposeGoal': 'Goal decomposition', 'GetActiveGoals': 'Active goal tracking', 'UpdateGoalProgress': 'Progress tracking'}
        for func, desc in features.items():
            if func in content:
                print(f'  ✓ {desc}')
                self.stats[f'goals_{func}'] = 1
            else:
                print(f'  ⚠️  {desc} missing')
        echobeats_path = self.repo_path / 'core/deeptreeecho/echobeats_scheduler.go'
        if echobeats_path.exists():
            echobeats_content = echobeats_path.read_text()
            if 'goal' in echobeats_content.lower():
                print(f'  ✓ Goals integrated with Echobeats')
            else:
                self.improvements['goals'].append('Goals not driving Echobeats task generation')
                print(f'  ⚠️  Goals not driving Echobeats scheduler')
    def check_consciousness_layers(self):
        print('\n🧠 Checking Consciousness Layers...')
        consciousness_path = self.repo_path / 'core/deeptreeecho/consciousness_layers.go'
        if not consciousness_path.exists():
            self.issues['consciousness'].append('Consciousness layers not found')
            print(f'  ✗ Consciousness layers missing')
            return
        content = consciousness_path.read_text()
        layers = ['sensory', 'perceptual', 'cognitive', 'metacognitive']
        layer_count = sum((1 for layer in layers if layer in content.lower()))
        print(f'  ℹ️  Consciousness layers detected: {layer_count}/4')
        if layer_count < 4:
            self.improvements['consciousness'].append('Missing full consciousness layer hierarchy')
    def generate_report(self):
        print('\n' + '=' * 60)
        print('📊 ANALYSIS SUMMARY')
        print('=' * 60)
        print(f'\n🔴 CRITICAL ISSUES ({len(self.issues)} categories):')
        for category, issues in self.issues.items():
            print(f'\n  {category.upper()}:')
            for issue in issues:
                print(f'    - {issue}')
        print(f'\n🟡 IMPROVEMENT OPPORTUNITIES ({len(self.improvements)} categories):')
        for category, improvements in self.improvements.items():
            print(f'\n  {category.upper()}:')
            for improvement in improvements:
                print(f'    - {improvement}')
        print(f'\n📈 STATISTICS:')
        for stat, value in sorted(self.stats.items()):
            print(f'  {stat}: {value}')
        report_path = self.repo_path / 'ITERATION_ANALYSIS_CURRENT.json'
        report = {'issues': dict(self.issues), 'improvements': dict(self.improvements), 'stats': dict(self.stats)}
        with open(report_path, 'w') as f:
            json.dump(report, f, indent=2)
        print(f'\n💾 Full report saved to: {report_path}')
if __name__ == '__main__':
    analyzer = Echo9llamaAnalyzer('/home/ubuntu/echo9llama')
    analyzer.analyze()