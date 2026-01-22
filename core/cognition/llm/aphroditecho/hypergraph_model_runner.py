import asyncio
import logging
import time
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
import torch
import numpy as np
from hypergraph_service import HypergraphService, EchoPropagationEngine, IdentityFragment, InteractionData
logger = logging.getLogger(__name__)
@dataclass
class IdentityContext:
    trigger_nodes: List[str]
    context_keywords: str
    confidence_score: float
    dominant_persona: Optional[str] = None
    active_fragments: List[IdentityFragment] = None
@dataclass
class HypergraphMetrics:
    propagation_latency: float
    identity_retrieval_latency: float
    active_fragment_count: int
    cache_hit_rate: float
    sync_operations: int
class HypergraphEnhancedModelRunner:
    def __init__(self, echo_self_engine, aar_orchestrator, dtesn_kernel, hypergraph_service):
        self.echo_self = echo_self_engine
        self.aar = aar_orchestrator
        self.dtesn = dtesn_kernel
        self.hypergraph = hypergraph_service
        self.echo_propagation_engine = EchoPropagationEngine(hypergraph_service)
        self.embodied_agents = {}
        self.identity_cache = {}
        self.metrics = HypergraphMetrics(0, 0, 0, 0, 0)
        self.total_requests = 0
        self.cache_hits = 0
        self.propagation_times = []
    async def initialize(self):
        logger.info('Initializing hypergraph-enhanced model runner')
        if not self.hypergraph.connection_pool:
            await self.hypergraph.initialize()
        await self._load_initial_configurations()
        logger.info('Hypergraph-enhanced model runner initialized')
    async def _load_initial_configurations(self):
        try:
            active_config = await self.hypergraph.get_active_configuration()
            common_types = ['persona', 'skill', 'memory', 'belief', 'value']
            for fragment_type in common_types:
                fragments = await self.hypergraph.search_nodes_by_type(fragment_type, limit=20)
                cache_key = f'type_{fragment_type}'
                self.identity_cache[cache_key] = fragments
            logger.info(f'Loaded initial configurations: {len(self.identity_cache)} cached fragment sets')
        except Exception as e:
            logger.error(f'Error loading initial configurations: {e}')
    @torch.no_grad()
    async def execute_model(self, scheduler_output):
        start_time = time.time()
        self.total_requests += 1
        try:
            identity_context = await self._extract_identity_context(scheduler_output)
            propagation_start = time.time()
            activated_fragments = await self.echo_propagation_engine.propagate(identity_context.trigger_nodes, max_depth=3, min_weight=0.1)
            propagation_time = time.time() - propagation_start
            self.propagation_times.append(propagation_time)
            identity_context.active_fragments = await self._get_fragments_from_propagation(activated_fragments)
            membrane_states = await self.dtesn.process_input(scheduler_output, identity_fragments=identity_context.active_fragments)
            agent_allocation = await self.aar.allocate_agents(membrane_states, identity_profile=identity_context.active_fragments)
            embodied_results = await self._execute_identity_aware_inference(agent_allocation, scheduler_output, identity_context)
            await self._update_hypergraph_from_results(embodied_results, identity_context)
            total_time = time.time() - start_time
            await self._update_metrics(total_time, propagation_time, len(activated_fragments))
            return embodied_results
        except Exception as e:
            logger.error(f'Error in hypergraph-enhanced model execution: {e}')
            return await self._fallback_execution(scheduler_output)
    async def _extract_identity_context(self, scheduler_output) -> IdentityContext:
        try:
            request_metadata = getattr(scheduler_output, 'metadata', {})
            context_keywords = request_metadata.get('context', '')
            if not context_keywords and hasattr(scheduler_output, 'prompt'):
                context_keywords = await self._analyze_prompt_for_context(scheduler_output.prompt)
            retrieval_start = time.time()
            relevant_fragments = await self.hypergraph.get_identity_fragments(context_keywords)
            retrieval_time = time.time() - retrieval_start
            trigger_nodes = [f.id for f in relevant_fragments[:3]]
            confidence_score = np.mean([f.activation_strength for f in relevant_fragments]) if relevant_fragments else 0.5
            dominant_persona = self._determine_dominant_persona(relevant_fragments)
            return IdentityContext(trigger_nodes=trigger_nodes, context_keywords=context_keywords, confidence_score=confidence_score, dominant_persona=dominant_persona, active_fragments=relevant_fragments)
        except Exception as e:
            logger.error(f'Error extracting identity context: {e}')
            return IdentityContext(trigger_nodes=[], context_keywords='', confidence_score=0.5)
    async def _analyze_prompt_for_context(self, prompt: str) -> str:
        keywords = []
        technical_terms = ['code', 'programming', 'algorithm', 'technical', 'analysis', 'data']
        if any((term in prompt.lower() for term in technical_terms)):
            keywords.append('technical')
        creative_terms = ['story', 'creative', 'imagine', 'artistic', 'design', 'poetry']
        if any((term in prompt.lower() for term in creative_terms)):
            keywords.append('creative')
        analytical_terms = ['analyze', 'compare', 'evaluate', 'assess', 'research']
        if any((term in prompt.lower() for term in analytical_terms)):
            keywords.append('analytical')
        empathetic_terms = ['help', 'support', 'understand', 'feel', 'emotion']
        if any((term in prompt.lower() for term in empathetic_terms)):
            keywords.append('empathetic')
        return ' '.join(keywords) if keywords else 'general'
    def _determine_dominant_persona(self, fragments: List[IdentityFragment]) -> Optional[str]:
        persona_fragments = [f for f in fragments if f.type == 'persona']
        if persona_fragments:
            dominant = max(persona_fragments, key=lambda f: f.activation_strength)
            return dominant.data.get('name', 'unknown')
        return None
    async def _get_fragments_from_propagation(self, propagation_results) -> List[IdentityFragment]:
        fragments = []
        for result in propagation_results[:10]:
            fragment = await self.hypergraph.get_node_by_id(result.node_id)
            if fragment:
                fragment.activation_strength = result.accumulated_weight
                fragments.append(fragment)
        return fragments
    async def _execute_identity_aware_inference(self, agent_allocation, scheduler_output, identity_context: IdentityContext):
        try:
            enhanced_input = await self._enhance_input_with_identity(scheduler_output, identity_context)
            results = []
            for agent in agent_allocation:
                await self._configure_agent_identity(agent, identity_context)
                agent_result = await agent.process_with_identity(enhanced_input, identity_context)
                results.append(agent_result)
            final_result = await self._aggregate_identity_aware_results(results, identity_context)
            return final_result
        except Exception as e:
            logger.error(f'Error in identity-aware inference: {e}')
            return await self._standard_agent_processing(agent_allocation, scheduler_output)
    async def _enhance_input_with_identity(self, scheduler_output, identity_context: IdentityContext):
        enhanced_input = {'original_input': scheduler_output, 'identity_context': {'dominant_persona': identity_context.dominant_persona, 'active_fragments': [{'type': f.type, 'name': f.data.get('name', 'unknown'), 'activation': f.activation_strength} for f in identity_context.active_fragments[:5]], 'confidence': identity_context.confidence_score, 'context_keywords': identity_context.context_keywords}}
        return enhanced_input
    async def _configure_agent_identity(self, agent, identity_context: IdentityContext):
        try:
            agent.identity_profile = {'dominant_persona': identity_context.dominant_persona, 'active_fragments': identity_context.active_fragments, 'confidence_score': identity_context.confidence_score}
            await self._update_agent_parameters(agent, identity_context)
        except Exception as e:
            logger.error(f'Error configuring agent identity: {e}')
    async def _update_agent_parameters(self, agent, identity_context: IdentityContext):
        if identity_context.dominant_persona == 'Creative Writer':
            agent.creativity_weight = 0.8
            agent.analytical_weight = 0.4
        elif identity_context.dominant_persona == 'Technical Analyst':
            agent.creativity_weight = 0.3
            agent.analytical_weight = 0.9
        elif identity_context.dominant_persona == 'Empathetic Counselor':
            agent.empathy_weight = 0.9
            agent.supportiveness_weight = 0.8
        agent.confidence_modifier = identity_context.confidence_score
    async def _aggregate_identity_aware_results(self, results, identity_context: IdentityContext):
        if not results:
            return None
        weighted_results = []
        for result in results:
            alignment_score = await self._calculate_identity_alignment(result, identity_context)
            weighted_result = {'content': result, 'weight': alignment_score, 'identity_alignment': alignment_score}
            weighted_results.append(weighted_result)
        if len(weighted_results) == 1:
            return weighted_results[0]['content']
        return await self._blend_weighted_results(weighted_results)
    async def _calculate_identity_alignment(self, result, identity_context: IdentityContext) -> float:
        alignment_score = 0.5
        if hasattr(result, 'content') and identity_context.context_keywords:
            content_lower = str(result.content).lower()
            keywords = identity_context.context_keywords.split()
            keyword_matches = sum((1 for keyword in keywords if keyword in content_lower))
            alignment_score += keyword_matches / len(keywords) * 0.3
        alignment_score *= identity_context.confidence_score
        return min(1.0, alignment_score)
    async def _blend_weighted_results(self, weighted_results):
        best_result = max(weighted_results, key=lambda x: x['weight'])
        return best_result['content']
    async def _update_hypergraph_from_results(self, results, identity_context: IdentityContext):
        try:
            if not identity_context.active_fragments:
                return
            interaction_data = InteractionData(trigger_id=identity_context.trigger_nodes[0] if identity_context.trigger_nodes else '', affected_nodes=[f.id for f in identity_context.active_fragments], strength=identity_context.confidence_score, context={'keywords': identity_context.context_keywords, 'dominant_persona': identity_context.dominant_persona, 'result_quality': await self._assess_result_quality(results)}, timestamp=time.time())
            await self.hypergraph.update_from_interaction(interaction_data)
        except Exception as e:
            logger.error(f'Error updating hypergraph from results: {e}')
    async def _assess_result_quality(self, results) -> float:
        if not results:
            return 0.0
        return 0.7
    async def _fallback_execution(self, scheduler_output):
        logger.warning('Falling back to standard execution')
        membrane_states = await self.dtesn.process_input(scheduler_output)
        agent_allocation = await self.aar.allocate_agents(membrane_states)
        return await self._standard_agent_processing(agent_allocation, scheduler_output)
    async def _standard_agent_processing(self, agent_allocation, scheduler_output):
        results = []
        for agent in agent_allocation:
            result = await agent.process(scheduler_output)
            results.append(result)
        return results[0] if results else None
    async def _update_metrics(self, total_time: float, propagation_time: float, fragment_count: int):
        self.metrics.propagation_latency = propagation_time
        self.metrics.active_fragment_count = fragment_count
        if self.total_requests > 0:
            self.metrics.cache_hit_rate = self.cache_hits / self.total_requests
        if self.total_requests % 100 == 0:
            await self._log_performance_metrics()
    async def _log_performance_metrics(self):
        avg_propagation_time = np.mean(self.propagation_times[-100:]) if self.propagation_times else 0
        logger.info(f'Hypergraph Model Runner Metrics:')
        logger.info(f'  Total Requests: {self.total_requests}')
        logger.info(f'  Cache Hit Rate: {self.metrics.cache_hit_rate:.2%}')
        logger.info(f'  Avg Propagation Time: {avg_propagation_time:.3f}s')
        logger.info(f'  Active Fragments: {self.metrics.active_fragment_count}')
    async def get_performance_stats(self) -> Dict[str, Any]:
        avg_propagation_time = np.mean(self.propagation_times[-100:]) if self.propagation_times else 0
        return {'total_requests': self.total_requests, 'cache_hit_rate': self.metrics.cache_hit_rate, 'average_propagation_time': avg_propagation_time, 'active_fragment_count': self.metrics.active_fragment_count, 'identity_cache_size': len(self.identity_cache), 'hypergraph_stats': await self.hypergraph.get_statistics()}
    async def shutdown(self):
        logger.info('Shutting down hypergraph-enhanced model runner')
        await self._log_performance_metrics()
        self.identity_cache.clear()
        logger.info('Hypergraph-enhanced model runner shutdown complete')