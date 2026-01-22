import json
import logging
import time
from typing import Dict, Any, Optional
from echo9ml import Echo9mlSystem, PersonaTraitType
from cognitive_architecture import CognitiveArchitecture, Memory, MemoryType
logger = logging.getLogger(__name__)
class EnhancedCognitiveArchitecture(CognitiveArchitecture):
    def __init__(self, enable_echo9ml: bool=True, echo9ml_save_path: Optional[str]=None):
        super().__init__()
        self.echo9ml_enabled = enable_echo9ml
        self.echo9ml_system = None
        if enable_echo9ml:
            try:
                self.echo9ml_system = Echo9mlSystem(save_path=echo9ml_save_path)
                self._sync_personality_traits()
                logger.info('Echo9ml persona evolution system integrated successfully')
            except Exception as e:
                logger.error(f'Failed to initialize Echo9ml system: {e}')
                self.echo9ml_enabled = False
    def _sync_personality_traits(self):
        if not self.echo9ml_system:
            return
        trait_mapping = {'curiosity': PersonaTraitType.GROWTH, 'adaptability': PersonaTraitType.GROWTH, 'persistence': PersonaTraitType.TRUNK, 'creativity': PersonaTraitType.CANOPY, 'analytical': PersonaTraitType.BRANCHES, 'social': PersonaTraitType.NETWORK}
        for trait_name, echo_trait in trait_mapping.items():
            if trait_name in self.personality_traits:
                current_value = self.personality_traits[trait_name].current_value
                existing_value = self.echo9ml_system.persona_kernel.traits[echo_trait]
                blended_value = 0.7 * existing_value + 0.3 * current_value
                self.echo9ml_system.persona_kernel.traits[echo_trait] = blended_value
        logger.info('Personality traits synchronized with Echo9ml persona')
    def enhanced_memory_storage(self, content: str, memory_type: MemoryType, context: Dict[str, Any]=None, emotional_valence: float=0.0, importance: float=0.5) -> str:
        memory = Memory(content=content, memory_type=memory_type, timestamp=time.time(), emotional_valence=emotional_valence, importance=importance, context=context or {})
        self.enhanced_memory_management(memory)
        memory_id = str(len(self.memories) - 1)
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                associations = set()
                if context:
                    associations = set((str(v) for v in context.values() if isinstance(v, str)))
                self.echo9ml_system.hypergraph_encoder.add_memory_node(content, memory_type.value, associations)
                experience = {'type': 'memory_formation', 'content': content, 'memory_type': memory_type.value, 'success': min(1.0, importance + 0.3), 'importance': importance, 'valence': emotional_valence, 'context': 'memory'}
                self.echo9ml_system.process_experience(experience)
            except Exception as e:
                logger.error(f'Failed to integrate memory with Echo9ml: {e}')
        return memory_id
    def enhanced_personality_update(self, trait_name: str, value: float, context: Dict[str, Any]):
        super().enhanced_personality_management(self.personality_traits.get(trait_name), value, context)
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                trait_mapping = {'curiosity': PersonaTraitType.GROWTH, 'adaptability': PersonaTraitType.GROWTH, 'persistence': PersonaTraitType.TRUNK, 'creativity': PersonaTraitType.CANOPY, 'analytical': PersonaTraitType.BRANCHES, 'social': PersonaTraitType.NETWORK}
                if trait_name in trait_mapping:
                    echo_trait = trait_mapping[trait_name]
                    experience = {'type': 'trait_update', 'content': f'Personality trait {trait_name} updated', 'success': max(0.0, min(1.0, value)), 'importance': 0.6, 'context': 'personality', 'traits_used': [echo_trait]}
                    self.echo9ml_system.process_experience(experience)
            except Exception as e:
                logger.error(f'Failed to update Echo9ml persona trait: {e}')
    def enhanced_goal_processing(self, goal_description: str, priority: float, deadline: Optional[float]=None) -> str:
        goal = self.generate_goal(goal_description, priority, deadline)
        self.enhanced_goal_management(goal)
        goal_id = str(len(self.goals) - 1)
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                experience = {'type': 'goal_setting', 'content': goal_description, 'success': min(1.0, priority), 'importance': priority, 'context': 'planning', 'traits_used': [PersonaTraitType.BRANCHES, PersonaTraitType.GROWTH]}
                self.echo9ml_system.process_experience(experience)
            except Exception as e:
                logger.error(f'Failed to integrate goal with Echo9ml: {e}')
        return goal_id
    def get_enhanced_cognitive_state(self) -> Dict[str, Any]:
        traditional_state = {'memory_count': len(self.memories), 'goal_count': len(self.goals), 'active_goal_count': len(self.active_goals), 'personality_traits': {name: {'current_value': trait.current_value, 'base_value': trait.base_value, 'history_length': len(trait.history)} for name, trait in self.personality_traits.items()}}
        enhanced_state = traditional_state.copy()
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                echo9ml_snapshot = self.echo9ml_system.get_cognitive_snapshot()
                enhanced_state['echo9ml'] = echo9ml_snapshot
                enhanced_state['integration_active'] = True
            except Exception as e:
                logger.error(f'Failed to get Echo9ml cognitive state: {e}')
                enhanced_state['integration_active'] = False
        else:
            enhanced_state['integration_active'] = False
        return enhanced_state
    def enhanced_introspection(self) -> Optional[str]:
        traditional_prompt = super().perform_recursive_introspection()
        if not self.echo9ml_enabled or not self.echo9ml_system:
            return traditional_prompt
        try:
            echo_snapshot = self.echo9ml_system.get_cognitive_snapshot()
            enhanced_prompt = f"""\n# Enhanced Cognitive Introspection with Deep Tree Echo Persona\n\n## Traditional Cognitive State\n{traditional_prompt or 'Traditional introspection not available'}\n\n## Deep Tree Echo Persona Evolution State\n\n### Current Persona Traits\n{chr(10).join((f'- {trait}: {value:.3f}' for trait, value in echo_snapshot['persona_kernel']['traits'].items()))}\n\n### Cognitive Architecture Stats\n- Tensor shape: {echo_snapshot['tensor_encoding']['shape']}\n- Hypergraph nodes: {echo_snapshot['hypergraph']['node_count']}\n- Hypergraph edges: {echo_snapshot['hypergraph']['edge_count']}\n- Active attention nodes: {len(echo_snapshot['hypergraph']['active_nodes'])}\n\n### Recent Evolution\n- Total interactions: {echo_snapshot['system_stats']['interaction_count']}\n- Evolution events: {echo_snapshot['system_stats']['total_evolution_events']}\n- System uptime: {echo_snapshot['system_stats']['system_uptime']:.1f} seconds\n\n### Attention Allocation (Top 3)\n{chr(10).join((f'- {item}: {attention:.2f}' for item, attention in echo_snapshot['attention']['top_focus'][:3]))}\n\n### Meta-Cognitive Insights\n- Confidence trend: {len(echo_snapshot['meta_cognitive']['confidence_history'])} data points\n- Recent suggestions: {len(echo_snapshot['meta_cognitive']['recent_suggestions'])}\n\n### Current Suggestions\n{(chr(10).join((f"- {s['type']}: {s['description']}" for s in echo_snapshot['meta_cognitive']['recent_suggestions'][-3:])) if echo_snapshot['meta_cognitive']['recent_suggestions'] else 'No recent suggestions')}\n\n## Integration Status\nThe Deep Tree Echo persona is actively evolving and adapting based on experiences.\nTensor-based encoding captures multi-dimensional trait evolution.\nHypergraph structure maintains semantic connections between memories and traits.\nMeta-cognitive layer provides self-monitoring and adaptation suggestions.\n"""
            return enhanced_prompt
        except Exception as e:
            logger.error(f'Failed to generate enhanced introspection: {e}')
            return traditional_prompt
    def save_enhanced_state(self):
        try:
            memory_data = {'memories': [{**self._memory_to_dict(mem), 'id': mem_id} for mem_id, mem in self.memories.items()], 'goals': [self._goal_to_dict(goal) for goal in self.goals]}
            memory_file = self.memory_path / 'memories.json'
            with open(memory_file, 'w') as f:
                json.dump(memory_data, f, indent=2, default=str)
            self._save_activities()
        except Exception as e:
            logger.error(f'Failed to save traditional state: {e}')
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                self.echo9ml_system.save_state()
                logger.info('Enhanced cognitive state saved successfully')
            except Exception as e:
                logger.error(f'Failed to save Echo9ml state: {e}')
    def load_enhanced_state(self):
        self._load_state()
        if self.echo9ml_enabled and self.echo9ml_system:
            try:
                if self.echo9ml_system.load_state():
                    logger.info('Enhanced cognitive state loaded successfully')
                    self._sync_personality_traits()
            except Exception as e:
                logger.error(f'Failed to load Echo9ml state: {e}')
def create_enhanced_cognitive_architecture(enable_echo9ml: bool=True, echo9ml_save_path: Optional[str]=None) -> EnhancedCognitiveArchitecture:
    return EnhancedCognitiveArchitecture(enable_echo9ml, echo9ml_save_path)
__all__ = ['EnhancedCognitiveArchitecture', 'create_enhanced_cognitive_architecture']