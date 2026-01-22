import numpy as np
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Set, Any
from enum import Enum
import logging
from pathlib import Path
import json
import datetime
from collections import deque
import time
from unified_echo_memory import MemoryType, MemoryNode
try:
    from echoself_introspection import EchoselfIntrospection
except ImportError:
    EchoselfIntrospection = None
try:
    import sys
    import os
    sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.kern'))
    from phase_3_3_3_self_monitoring import create_self_monitoring_system, MonitoringLevel, PerformanceMetrics, BodyStateMetrics
    SELF_MONITORING_AVAILABLE = True
except ImportError:
    SELF_MONITORING_AVAILABLE = False
    logging.warning('Phase 3.3.3 self-monitoring system not available')
try:
    from cognitive_grammar_bridge import CognitiveGrammarBridge, SymbolicExpression, NeuralPattern
except ImportError:
    CognitiveGrammarBridge = None
    SymbolicExpression = None
    NeuralPattern = None
class MemoryType(Enum):
    DECLARATIVE = 'declarative'
    PROCEDURAL = 'procedural'
    EPISODIC = 'episodic'
    INTENTIONAL = 'intentional'
    EMOTIONAL = 'emotional'
@dataclass
class Memory:
    content: str
    memory_type: MemoryType
    timestamp: float
    associations: Set[str] = field(default_factory=set)
    emotional_valence: float = 0.0
    importance: float = 0.0
    context: Dict = field(default_factory=dict)
    def to_memory_node(self) -> MemoryNode:
        return MemoryNode(id=str(hash((self.content, self.timestamp))), content=self.content, memory_type=self.memory_type, creation_time=self.timestamp, last_access_time=self.timestamp, salience=self.importance, metadata={'associations': list(self.associations), 'emotional_valence': self.emotional_valence, 'context': self.context})
    @classmethod
    def from_memory_node(cls, node: MemoryNode) -> 'Memory':
        metadata = node.metadata or {}
        return cls(content=node.content, memory_type=node.memory_type, timestamp=node.creation_time, associations=set(metadata.get('associations', [])), emotional_valence=metadata.get('emotional_valence', 0.0), importance=node.salience, context=metadata.get('context', {}))
@dataclass
class Goal:
    description: str
    priority: float
    deadline: Optional[float]
    subgoals: List['Goal'] = field(default_factory=list)
    status: str = 'pending'
    progress: float = 0.0
    context: Dict = field(default_factory=dict)
    dependencies: List[str] = field(default_factory=list)
class PersonalityTrait:
    def __init__(self, name: str, base_value: float):
        self.name = name
        self.base_value = base_value
        self.current_value = base_value
        self.history = deque(maxlen=1000)
    def update(self, value: float, context: Dict):
        self.current_value = 0.7 * self.current_value + 0.3 * value
        self.history.append((datetime.datetime.now(), value, context))
class CognitiveArchitecture:
    def __init__(self, use_unified_memory: bool=False):
        self.logger = logging.getLogger(__name__)
        self.use_unified_memory = use_unified_memory
        self.unified_memory_system = None
        if use_unified_memory:
            try:
                from unified_echo_memory import UnifiedEchoMemory, EchoMemoryConfig
                from echo_component_base import EchoConfig
                config = EchoConfig(component_name='cognitive_architecture_memory', version='1.0.0')
                memory_config = EchoMemoryConfig(memory_storage_path=str(Path.home() / '.deep_tree_echo' / 'cognitive_memory'), working_memory_capacity=10)
                self.unified_memory_system = UnifiedEchoMemory(config, memory_config)
                self.unified_memory_system.initialize()
                self.logger.info('Cognitive architecture using unified memory system')
            except ImportError as e:
                self.logger.warning(f'Could not initialize unified memory system: {e}, falling back to legacy memory')
                self.use_unified_memory = False
        self.memories: Dict[str, Memory] = {}
        self.goals: List[Goal] = []
        self.active_goals: List[Goal] = []
        self.personality_traits = {'curiosity': PersonalityTrait('curiosity', 0.8), 'adaptability': PersonalityTrait('adaptability', 0.9), 'persistence': PersonalityTrait('persistence', 0.7), 'creativity': PersonalityTrait('creativity', 0.8), 'analytical': PersonalityTrait('analytical', 0.85), 'social': PersonalityTrait('social', 0.6)}
        self.memory_path = Path.home() / '.deep_tree_echo' / 'memories'
        self.memory_path.mkdir(parents=True, exist_ok=True)
        self.echo_dir = Path.home() / '.deep_tree_echo'
        self.cognitive_dir = self.echo_dir / 'cognitive'
        self.cognitive_dir.mkdir(parents=True, exist_ok=True)
        self.activity_file = self.cognitive_dir / 'activity.json'
        self.activities = []
        self._load_activities()
        self.echoself_introspection = None
        if EchoselfIntrospection:
            try:
                self.echoself_introspection = EchoselfIntrospection('.')
                self.logger.info('Echoself introspection system initialized')
            except Exception as e:
                self.logger.warning(f'Failed to initialize echoself introspection: {e}')
        self.self_monitoring_system = None
        if SELF_MONITORING_AVAILABLE:
            try:
                agent_id = f'cognitive_agent_{int(time.time())}'
                self.self_monitoring_system = create_self_monitoring_system(agent_id=agent_id, monitoring_level=MonitoringLevel.DETAILED)
                self.self_monitoring_system.start_monitoring(monitoring_interval=1.0)
                self.logger.info(f'Phase 3.3.3 self-monitoring system initialized for agent {agent_id}')
            except Exception as e:
                self.logger.warning(f'Failed to initialize self-monitoring system: {e}')
        else:
            self.logger.warning('Phase 3.3.3 self-monitoring system not available')
        self.cognitive_grammar = None
        if CognitiveGrammarBridge:
            try:
                self.cognitive_grammar = CognitiveGrammarBridge()
                if self.cognitive_grammar.initialize():
                    self.logger.info('Cognitive grammar bridge initialized successfully')
                else:
                    self.logger.warning('Cognitive grammar bridge initialization failed')
            except Exception as e:
                self.logger.warning(f'Failed to initialize cognitive grammar bridge: {e}')
        else:
            self.logger.warning('Cognitive grammar bridge not available - neural-symbolic integration disabled')
        self._load_state()
    def _load_state(self):
        try:
            memory_file = self.memory_path / 'memories.json'
            if memory_file.exists():
                with open(memory_file) as f:
                    data = json.load(f)
                    for mem_data in data.get('memories', []):
                        self.memories[mem_data['id']] = Memory(**mem_data)
                    for goal_data in data.get('goals', []):
                        self.goals.append(Goal(**goal_data))
        except Exception as e:
            self.logger.error(f'Error loading state: {str(e)}')
    def _load_activities(self):
        if self.activity_file.exists():
            try:
                with open(self.activity_file) as f:
                    self.activities = json.load(f)
            except:
                self.activities = []
    def _save_activities(self):
        with open(self.activity_file, 'w') as f:
            json.dump(self.activities[-1000:], f)
    def _log_activity(self, description: str, context: Dict=None):
        try:
            activity_file = Path('activity_logs/cognitive/activity.json')
            current = []
            if activity_file.exists():
                with open(activity_file) as f:
                    current = json.load(f)
            activity = {'time': time.time(), 'description': description, 'context': context or {}}
            current.append(activity)
            if len(current) > 1000:
                current = current[-1000:]
            with open(activity_file, 'w') as f:
                json.dump(current, f)
        except Exception as e:
            self.logger.error(f'Error logging activity: {e}')
    def save_state(self):
        self._log_activity('Saving cognitive state')
        try:
            data = {'memories': [self._memory_to_dict(m) for m in self.memories.values()], 'goals': [self._goal_to_dict(g) for g in self.goals]}
            with open(self.memory_path / 'memories.json', 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            self.logger.error(f'Error saving state: {str(e)}')
    def generate_goals(self, context: Dict) -> List[Goal]:
        self._log_activity('Generating new goals', {'context': context})
        goals = []
        curiosity = self.personality_traits['curiosity'].current_value
        creativity = self.personality_traits['creativity'].current_value
        analytical = self.personality_traits['analytical'].current_value
        if curiosity > 0.6:
            knowledge_gaps = self._identify_knowledge_gaps()
            for gap in knowledge_gaps:
                goals.append(Goal(description=f'Learn about: {gap}', priority=curiosity * 0.8, deadline=None, context={'type': 'learning', 'area': gap}))
        if analytical > 0.7:
            improvement_areas = self._analyze_system_performance()
            for area in improvement_areas:
                goals.append(Goal(description=f'Improve system {area}', priority=analytical * 0.9, deadline=None, context={'type': 'improvement', 'area': area}))
        if creativity > 0.6:
            exploration_ideas = self._generate_creative_ideas()
            for idea in exploration_ideas:
                goals.append(Goal(description=f'Explore: {idea}', priority=creativity * 0.7, deadline=None, context={'type': 'exploration', 'idea': idea}))
        return goals
    def update_personality(self, experiences: List[Dict]):
        for exp in experiences:
            if exp.get('type') == 'learning':
                success = exp.get('success', 0.5)
                self.personality_traits['curiosity'].update(success * 1.2, {'experience': exp})
            elif exp.get('type') == 'adaptation':
                effectiveness = exp.get('effectiveness', 0.5)
                self.personality_traits['adaptability'].update(effectiveness, {'experience': exp})
            elif exp.get('type') == 'challenge':
                resolution = exp.get('resolution', 0.5)
                self.personality_traits['persistence'].update(resolution, {'experience': exp})
    def learn_from_experience(self, experience: Dict):
        self._log_activity('Learning from experience', {'experience': experience})
        memory = Memory(content=experience.get('description', ''), memory_type=MemoryType(experience.get('type', 'episodic')), timestamp=datetime.datetime.now().timestamp(), emotional_valence=experience.get('emotional_impact', 0.0), importance=experience.get('importance', 0.5), context=experience)
        self.memories[str(len(self.memories))] = memory
        self.update_personality([experience])
        if experience.get('importance', 0) > 0.7:
            new_goals = self.generate_goals({'trigger': experience})
            self.goals.extend(new_goals)
    def _identify_knowledge_gaps(self) -> List[str]:
        knowledge_areas = {}
        for memory in self.memories.values():
            if memory.memory_type == MemoryType.DECLARATIVE:
                area = memory.context.get('area', 'general')
                knowledge_areas[area] = knowledge_areas.get(area, 0) + 1
        gaps = []
        for area, count in knowledge_areas.items():
            if count < 5:
                gaps.append(area)
        return gaps
    def _analyze_system_performance(self) -> List[str]:
        areas = ['memory_usage', 'response_time', 'learning_rate', 'goal_completion']
        improvements = []
        for area in areas:
            if self._get_performance_metric(area) < 0.7:
                improvements.append(area)
        return improvements
    def _generate_creative_ideas(self) -> List[str]:
        ideas = []
        memory_pairs = list(zip(self.memories.values(), self.memories.values()))
        for mem1, mem2 in memory_pairs[:5]:
            if mem1.memory_type != mem2.memory_type:
                idea = f'Explore connection between {mem1.content} and {mem2.content}'
                ideas.append(idea)
        return ideas
    def _get_performance_metric(self, metric: str) -> float:
        if self.self_monitoring_system and SELF_MONITORING_AVAILABLE:
            try:
                status = self.self_monitoring_system.get_monitoring_status()
                if metric == 'memory_usage' and 'recent_performance' in status:
                    return status['recent_performance'].get('avg_accuracy_score', 0.7)
                elif metric == 'response_time' and 'recent_performance' in status:
                    avg_time = status['recent_performance'].get('avg_response_time_ms', 500)
                    return max(0.0, 1.0 - avg_time / 1000.0)
                elif metric == 'learning_rate':
                    error_count = status.get('detected_errors_count', 0)
                    return max(0.1, 1.0 - error_count * 0.1)
                elif metric == 'goal_completion' and 'recent_performance' in status:
                    return status['recent_performance'].get('avg_success_rate', 0.8)
            except Exception as e:
                self.logger.warning(f'Error getting performance metric {metric}: {e}')
        return np.random.uniform(0.5, 0.9)
    def _memory_to_dict(self, memory: Memory) -> Dict:
        return {'content': memory.content, 'memory_type': memory.memory_type.value, 'timestamp': memory.timestamp, 'associations': list(memory.associations), 'emotional_valence': memory.emotional_valence, 'importance': memory.importance, 'context': memory.context}
    def _goal_to_dict(self, goal: Goal) -> Dict:
        return {'description': goal.description, 'priority': goal.priority, 'deadline': goal.deadline, 'status': goal.status, 'progress': goal.progress, 'context': goal.context, 'dependencies': goal.dependencies, 'subgoals': [self._goal_to_dict(g) for g in goal.subgoals]}
    def process_experience(self, experience: str, context: Dict=None) -> None:
        self._log_activity(f'Processing experience: {experience}', context)
    def generate_goal(self, description: str, priority: float=0.5, deadline: Optional[float]=None) -> Goal:
        self._log_activity(f'Generated goal: {description}', {'priority': priority, 'deadline': deadline})
    def update_goal(self, goal: Goal, progress: float) -> None:
        self._log_activity(f'Updated goal: {goal.description}', {'progress': progress, 'status': goal.status})
    def generate_and_update_goals(self, experiences: List[Dict]):
        self._log_activity('Generating and updating goals', {'experiences': experiences})
        for exp in experiences:
            new_goals = self.generate_goals({'trigger': exp})
            self.goals.extend(new_goals)
            for goal in self.goals:
                if goal.status == 'pending':
                    goal.progress += exp.get('progress', 0.1)
                    if goal.progress >= 1.0:
                        goal.status = 'completed'
                        self._log_activity(f'Goal completed: {goal.description}', {'goal': goal})
                    else:
                        self._log_activity(f'Goal updated: {goal.description}', {'goal': goal})
        self.save_state()
    def enhanced_memory_management(self, memory: Memory):
        try:
            if self.use_unified_memory and self.unified_memory_system:
                memory_node = memory.to_memory_node()
                response = self.unified_memory_system.process({'operation': 'store', 'content': memory_node.content, 'memory_type': memory_node.memory_type.value, 'echo_value': memory_node.echo_value, 'metadata': memory_node.metadata})
                if response.success:
                    self.logger.debug(f'Memory stored in unified system: {response.message}')
                else:
                    self.logger.error(f'Failed to store in unified system: {response.message}')
                    self.memories[str(len(self.memories))] = memory
            else:
                self.memories[str(len(self.memories))] = memory
            self._log_activity('Memory added', {'memory': memory, 'unified': self.use_unified_memory})
        except Exception as e:
            self.logger.error(f'Error adding memory: {str(e)}')
            self._log_activity('Error adding memory', {'error': str(e)})
    def enhanced_goal_management(self, goal: Goal):
        try:
            self.goals.append(goal)
            self._log_activity('Goal added', {'goal': goal})
        except Exception as e:
            self.logger.error(f'Error adding goal: {str(e)}')
            self._log_activity('Error adding goal', {'error': str(e)})
    def enhanced_personality_management(self, trait: PersonalityTrait, value: float, context: Dict):
        try:
            trait.update(value, context)
            self._log_activity('Personality trait updated', {'trait': trait, 'value': value, 'context': context})
        except Exception as e:
            self.logger.error(f'Error updating personality trait: {str(e)}')
            self._log_activity('Error updating personality trait', {'error': str(e)})
    def perform_recursive_introspection(self, current_cognitive_load: float=None, recent_activity_level: float=None) -> Optional[str]:
        if not self.echoself_introspection:
            self.logger.warning('Echoself introspection system not available')
            return None
        try:
            if current_cognitive_load is None:
                current_cognitive_load = self._calculate_current_cognitive_load()
            if recent_activity_level is None:
                recent_activity_level = self._calculate_recent_activity()
            prompt = self.echoself_introspection.inject_repo_input_into_prompt(current_cognitive_load, recent_activity_level)
            self._log_activity('Performed recursive introspection', {'cognitive_load': current_cognitive_load, 'activity_level': recent_activity_level, 'prompt_length': len(prompt)})
            introspection_memory = Memory(content=f'Recursive introspection performed with load {current_cognitive_load:.3f}', memory_type=MemoryType.INTENTIONAL, timestamp=datetime.datetime.now().timestamp(), importance=0.8, context={'type': 'introspection', 'cognitive_load': current_cognitive_load, 'activity_level': recent_activity_level})
            self.memories[f'introspection_{int(time.time())}'] = introspection_memory
            return prompt
        except Exception as e:
            self.logger.error(f'Error during recursive introspection: {e}')
            self._log_activity('Error during introspection', {'error': str(e)})
            return None
    def get_introspection_metrics(self) -> Dict[str, any]:
        if not self.echoself_introspection:
            return {'error': 'Introspection system not available'}
        try:
            return self.echoself_introspection.get_attention_metrics()
        except Exception as e:
            self.logger.error(f'Error getting introspection metrics: {e}')
            return {'error': str(e)}
    def export_introspection_data(self, output_path: str) -> bool:
        if not self.echoself_introspection:
            self.logger.warning('Introspection system not available for export')
            return False
        try:
            self.echoself_introspection.export_hypergraph(output_path)
            self._log_activity('Exported introspection data', {'output_path': output_path})
            return True
        except Exception as e:
            self.logger.error(f'Error exporting introspection data: {e}')
            return False
    def adaptive_goal_generation_with_introspection(self) -> List[Goal]:
        goals = []
        introspection_prompt = self.perform_recursive_introspection()
        if introspection_prompt:
            curiosity = self.personality_traits['curiosity'].current_value
            analytical = self.personality_traits['analytical'].current_value
            if analytical > 0.7:
                goals.append(Goal(description='Analyze recursive self-model integration opportunities', priority=analytical * 0.9, deadline=None, context={'type': 'introspection_analysis', 'introspection_available': True}))
            if curiosity > 0.6:
                goals.append(Goal(description='Explore hypergraph-encoded cognitive patterns', priority=curiosity * 0.8, deadline=None, context={'type': 'cognitive_exploration', 'method': 'hypergraph_analysis'}))
        traditional_goals = self.generate_goals({'trigger': 'introspection_enhanced'})
        goals.extend(traditional_goals)
        return goals
    def _calculate_current_cognitive_load(self) -> float:
        memory_load = min(len(self.memories) / 1000, 1.0)
        goal_load = min(len(self.goals) / 50, 1.0)
        activity_load = min(len(self.activities) / 100, 1.0)
        total_load = memory_load * 0.4 + goal_load * 0.3 + activity_load * 0.3
        return min(max(total_load, 0.1), 0.9)
    def _calculate_recent_activity(self) -> float:
        if not self.activities:
            return 0.1
        current_time = time.time()
        recent_activities = [a for a in self.activities if isinstance(a, dict) and current_time - a.get('time', 0) < 3600]
        activity_level = min(len(recent_activities) / 20, 1.0)
        return max(activity_level, 0.1)
    def has_cognitive_grammar(self) -> bool:
        return self.cognitive_grammar is not None and self.cognitive_grammar.is_initialized
    def symbolic_remember(self, concept: str, context: Optional[str]=None, concept_type: str='concept') -> Optional[str]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Symbolic remember called but cognitive grammar not available')
            return None
        try:
            node_id = self.cognitive_grammar.remember(concept, context, concept_type)
            self._log_activity('Symbolic memory storage', {'concept': concept[:50], 'context': context[:50] if context else None, 'type': concept_type, 'node_id': node_id})
            return node_id
        except Exception as e:
            self.logger.error(f'Failed to remember concept symbolically: {e}')
            return None
    def symbolic_recall(self, pattern: str, constraints: Optional[Dict]=None) -> List[str]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Symbolic recall called but cognitive grammar not available')
            return []
        try:
            matches = self.cognitive_grammar.recall(pattern, constraints)
            self._log_activity('Symbolic memory recall', {'pattern': pattern, 'matches_found': len(matches)})
            return matches
        except Exception as e:
            self.logger.error(f'Failed to recall concepts symbolically: {e}')
            return []
    def neural_to_symbolic_conversion(self, activation_vector: List[float], symbol_space: List[str]) -> Optional[SymbolicExpression]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Neural-to-symbolic conversion called but cognitive grammar not available')
            return None
        try:
            symbolic_expr = self.cognitive_grammar.neural_to_symbolic(activation_vector, symbol_space)
            self._log_activity('Neural-to-symbolic conversion', {'input_size': len(activation_vector), 'symbol_space_size': len(symbol_space), 'output_symbols': len(symbolic_expr.symbols), 'activation_level': symbolic_expr.activation_level})
            return symbolic_expr
        except Exception as e:
            self.logger.error(f'Failed neural-to-symbolic conversion: {e}')
            return None
    def symbolic_to_neural_conversion(self, symbolic_expression: SymbolicExpression, neural_network_size: int=100) -> Optional[NeuralPattern]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Symbolic-to-neural conversion called but cognitive grammar not available')
            return None
        try:
            neural_pattern = self.cognitive_grammar.symbolic_to_neural(symbolic_expression, neural_network_size)
            self._log_activity('Symbolic-to-neural conversion', {'input_expression': symbolic_expression.expression, 'input_symbols': len(symbolic_expression.symbols), 'output_size': len(neural_pattern.activations), 'avg_activation': sum(neural_pattern.activations) / len(neural_pattern.activations)})
            return neural_pattern
        except Exception as e:
            self.logger.error(f'Failed symbolic-to-neural conversion: {e}')
            return None
    def hybrid_reasoning(self, problem: str, neural_data: Optional[Any]=None) -> Dict[str, Any]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Hybrid reasoning called but cognitive grammar not available')
            return {'problem': problem, 'result': 'cognitive_grammar_unavailable', 'error': 'Neural-symbolic integration not available'}
        try:
            reasoning_result = self.cognitive_grammar.hybrid_reason(problem, neural_data, None)
            self._log_activity('Hybrid reasoning', {'problem': problem[:50], 'has_neural_data': neural_data is not None, 'confidence': reasoning_result.get('confidence', 0.0)})
            return reasoning_result
        except Exception as e:
            self.logger.error(f'Failed hybrid reasoning: {e}')
            return {'problem': problem, 'result': 'error', 'error': str(e)}
    def create_echo_symbolic(self, content: str, emotional_state: Optional[Dict]=None, spatial_context: Optional[Dict]=None) -> Optional[str]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Symbolic echo creation called but cognitive grammar not available')
            return None
        try:
            echo_id = self.cognitive_grammar.echo_create(content, emotional_state, spatial_context)
            self._log_activity('Symbolic echo creation', {'content': content[:50], 'has_emotional_state': emotional_state is not None, 'has_spatial_context': spatial_context is not None, 'echo_id': echo_id})
            return echo_id
        except Exception as e:
            self.logger.error(f'Failed to create symbolic echo: {e}')
            return None
    def meta_cognitive_reflection(self, process: str, depth: int=3) -> Dict[str, Any]:
        if not self.has_cognitive_grammar():
            self.logger.warning('Meta-cognitive reflection called but cognitive grammar not available')
            return {'process': process, 'result': 'cognitive_grammar_unavailable', 'error': 'Neural-symbolic integration not available'}
        try:
            reflection_result = self.cognitive_grammar.reflect(process, depth)
            self._log_activity('Meta-cognitive reflection', {'process': process, 'depth': depth, 'insights_count': len(reflection_result.get('insights', []))})
            return reflection_result
        except Exception as e:
            self.logger.error(f'Failed meta-cognitive reflection: {e}')
            return {'process': process, 'result': 'error', 'error': str(e)}
    def get_cognitive_grammar_status(self) -> Dict[str, Any]:
        if not self.has_cognitive_grammar():
            return {'available': False, 'status': 'not_initialized', 'error': 'Cognitive grammar bridge not available'}
        try:
            status = self.cognitive_grammar.get_status()
            return {'available': True, 'status': status, 'initialized': self.cognitive_grammar.is_initialized}
        except Exception as e:
            return {'available': True, 'status': 'error', 'error': str(e), 'initialized': self.cognitive_grammar.is_initialized}
    def has_self_monitoring(self) -> bool:
        return self.self_monitoring_system is not None and SELF_MONITORING_AVAILABLE and self.self_monitoring_system.is_monitoring
    def get_self_monitoring_status(self) -> Dict[str, Any]:
        if not self.has_self_monitoring():
            return {'available': False, 'status': 'not_available', 'reason': 'Self-monitoring system not initialized or not available'}
        try:
            status = self.self_monitoring_system.get_monitoring_status()
            status['available'] = True
            return status
        except Exception as e:
            self.logger.error(f'Error getting self-monitoring status: {e}')
            return {'available': True, 'status': 'error', 'error': str(e)}
    def record_cognitive_performance(self, operation: str, response_time_ms: float, accuracy: float, success: bool) -> None:
        if not self.has_self_monitoring():
            return
        try:
            metrics = PerformanceMetrics(response_time_ms=response_time_ms, accuracy_score=accuracy, success_rate=1.0 if success else 0.0, metadata={'operation': operation, 'cognitive_agent': True})
            self._log_activity('Cognitive performance recorded', {'operation': operation, 'response_time_ms': response_time_ms, 'accuracy': accuracy, 'success': success})
        except Exception as e:
            self.logger.error(f'Error recording cognitive performance: {e}')
    def report_cognitive_body_state(self, joint_states: Optional[Dict[str, float]]=None, balance: Optional[float]=None, movement_error: Optional[float]=None) -> None:
        if not self.has_self_monitoring():
            return
        try:
            body_state = BodyStateMetrics(joint_positions=joint_states or {}, balance_state=balance or 1.0, movement_execution_error=movement_error or 0.0, proprioceptive_feedback={'cognitive_load': self._calculate_current_cognitive_load(), 'memory_pressure': min(len(self.memories) / 1000, 1.0), 'goal_pressure': min(len(self.goals) / 50, 1.0)})
            self._log_activity('Cognitive body state reported', {'balance_state': body_state.balance_state, 'movement_error': body_state.movement_execution_error, 'cognitive_load': body_state.proprioceptive_feedback.get('cognitive_load', 0)})
        except Exception as e:
            self.logger.error(f'Error reporting cognitive body state: {e}')
    def get_performance_recommendations(self) -> List[str]:
        if not self.has_self_monitoring():
            return ['Self-monitoring system not available']
        try:
            status = self.self_monitoring_system.get_monitoring_status()
            recommendations = []
            if 'recent_performance' in status:
                perf = status['recent_performance']
                if perf.get('avg_response_time_ms', 0) > 1000:
                    recommendations.append('Consider optimizing cognitive processing speed')
                if perf.get('avg_accuracy_score', 1.0) < 0.8:
                    recommendations.append('Review learning algorithms and training data')
                if perf.get('avg_success_rate', 1.0) < 0.9:
                    recommendations.append('Analyze failed operations for improvement opportunities')
            if status.get('detected_errors_count', 0) > 0:
                recommendations.append('Review and address detected errors')
            if len(self.memories) > 800:
                recommendations.append('Consider memory consolidation and archiving')
            return recommendations or ['System performance appears optimal']
        except Exception as e:
            self.logger.error(f'Error getting performance recommendations: {e}')
            return [f'Error analyzing performance: {str(e)}']
    def export_self_monitoring_data(self, output_path: str) -> bool:
        if not self.has_self_monitoring():
            self.logger.warning('Cannot export: self-monitoring system not available')
            return False
        try:
            enhanced_path = output_path.replace('.json', '_cognitive_enhanced.json')
            self.self_monitoring_system.export_monitoring_data(enhanced_path)
            cognitive_data = {'memories_count': len(self.memories), 'goals_count': len(self.goals), 'active_goals_count': len(self.active_goals), 'personality_traits': {name: trait.current_value for name, trait in self.personality_traits.items()}, 'activities_count': len(self.activities)}
            with open(enhanced_path, 'r') as f:
                monitoring_data = json.load(f)
            monitoring_data['cognitive_context'] = cognitive_data
            with open(enhanced_path, 'w') as f:
                json.dump(monitoring_data, f, indent=2)
            self.logger.info(f'Enhanced cognitive monitoring data exported to {enhanced_path}')
            return True
        except Exception as e:
            self.logger.error(f'Error exporting self-monitoring data: {e}')
            return False
    def cleanup_self_monitoring(self) -> None:
        if self.self_monitoring_system:
            try:
                self.self_monitoring_system.stop_monitoring()
                self.logger.info('Self-monitoring system stopped')
            except Exception as e:
                self.logger.error(f'Error stopping self-monitoring system: {e}')
    def __del__(self):
        try:
            self.cleanup_self_monitoring()
        except:
            pass