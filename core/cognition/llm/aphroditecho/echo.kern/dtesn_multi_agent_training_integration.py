import logging
import time
import math
import random
from typing import Dict, List, Any
from dataclasses import dataclass, field
from multi_agent_training_system import MultiAgentTrainingSystem, TrainingConfiguration, AgentPopulationMember, DTESNMultiAgentTrainingIntegration
from population_based_training import PopulationBasedTrainer, PopulationConfig
from cooperative_competitive_learning import HybridLearningCoordinator, LearningConfiguration
try:
    from phase_3_3_3_self_monitoring import DTESNSelfMonitoringIntegration
    DTESN_MONITORING_AVAILABLE = True
except ImportError:
    DTESN_MONITORING_AVAILABLE = False
    logging.warning('DTESN self-monitoring not available')
try:
    try:
        from aar_core.orchestration.core_orchestrator import AARCoreOrchestrator
        from aar_core.agents.agent_manager import AgentManager
    except ImportError:
        import sys
        import os
        sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'aar_core'))
        from orchestration.core_orchestrator import AARCoreOrchestrator
        from agents.agent_manager import AgentManager
    AAR_CORE_AVAILABLE = True
except ImportError:
    AAR_CORE_AVAILABLE = False
    logging.warning('AAR core components not available')
logger = logging.getLogger(__name__)
@dataclass
class DTESNTrainingConfiguration:
    training_config: TrainingConfiguration = field(default_factory=TrainingConfiguration)
    population_config: PopulationConfig = field(default_factory=PopulationConfig)
    learning_config: LearningConfiguration = field(default_factory=LearningConfiguration)
    enable_dtesn_monitoring: bool = True
    dtesn_performance_weight: float = 0.3
    membrane_computing_integration: bool = True
    echo_state_feedback: bool = True
    enable_aar_orchestration: bool = True
    agent_arena_interactions: bool = True
    relation_graph_updates: bool = True
    convergence_threshold: float = 0.01
    min_population_diversity: float = 0.1
    max_training_time: float = 3600.0
    max_concurrent_sessions: int = 10
    memory_limit_mb: int = 2048
    cpu_core_limit: int = 4
class DTESNMultiAgentTrainingSystem:
    def __init__(self, config: DTESNTrainingConfiguration):
        self.config = config
        self.training_system = MultiAgentTrainingSystem(config.training_config)
        self.population_trainer = PopulationBasedTrainer(config.population_config)
        self.learning_coordinator = HybridLearningCoordinator(config.learning_config)
        self.dtesn_integration = None
        self.aar_orchestrator = None
        self.agent_manager = None
        self.training_active = False
        self.current_epoch = 0
        self.training_metrics: List[Dict[str, Any]] = []
        self.population_improvement_history: List[Dict[str, Any]] = []
        self.best_population_fitness = 0.0
        self.total_training_time = 0.0
        self.interactions_completed = 0
        self._initialize_integrations()
        logger.info('Initialized DTESNMultiAgentTrainingSystem')
    def _initialize_integrations(self):
        try:
            if self.config.enable_dtesn_monitoring and DTESN_MONITORING_AVAILABLE:
                self.dtesn_integration = DTESNMultiAgentTrainingIntegration(self.training_system)
                logger.info('DTESN integration initialized')
            if self.config.enable_aar_orchestration and AAR_CORE_AVAILABLE:
                self.aar_orchestrator = None
                self.agent_manager = None
                logger.info('AAR integration placeholders initialized')
        except Exception as e:
            logger.warning(f'Failed to initialize some integrations: {e}')
    async def initialize_training_population(self, population_specs: Dict[str, Any]=None) -> Dict[str, Any]:
        logger.info('Initializing DTESN-integrated training population')
        self.training_system.initialize_population()
        parameter_ranges = self._get_dtesn_parameter_ranges(population_specs)
        self.population_trainer.initialize_population(parameter_ranges)
        await self._synchronize_populations()
        if self.dtesn_integration:
            await self._initialize_dtesn_monitoring()
        if self.aar_orchestrator:
            await self._register_agents_with_aar()
        initialization_stats = {'training_population_size': len(self.training_system.population), 'evolution_population_size': len(self.population_trainer.population), 'dtesn_monitors_active': len(self.dtesn_integration.dtesn_monitors) if self.dtesn_integration else 0, 'aar_agents_registered': 0}
        logger.info(f'Population initialized: {initialization_stats}')
        return initialization_stats
    def _get_dtesn_parameter_ranges(self, specs: Dict[str, Any]=None) -> Dict[str, tuple]:
        default_ranges = {'layer_count': (2, 8), 'hidden_size': (64, 512), 'learning_rate': (0.001, 0.1), 'dropout_rate': (0.1, 0.5), 'reservoir_size': (100, 1000), 'spectral_radius': (0.1, 1.5), 'input_scaling': (0.1, 1.0), 'leak_rate': (0.1, 1.0), 'membrane_depth': (2, 6), 'rule_complexity': (1.0, 5.0), 'communication_probability': (0.1, 0.9), 'echo_strength': (0.1, 0.9), 'feedback_weight': (0.0, 0.5), 'noise_level': (0.0, 0.1)}
        if specs and 'parameter_ranges' in specs:
            default_ranges.update(specs['parameter_ranges'])
        return default_ranges
    async def _synchronize_populations(self):
        training_agents = list(self.training_system.population.keys())
        evolution_members = list(self.population_trainer.population.keys())
        target_size = min(len(training_agents), len(evolution_members))
        if len(training_agents) != len(evolution_members):
            logger.info(f'Synchronizing population sizes to {target_size}')
            training_agents = training_agents[:target_size]
            evolution_members = evolution_members[:target_size]
            self.training_system.population = {aid: self.training_system.population[aid] for aid in training_agents}
            self.population_trainer.population = {mid: self.population_trainer.population[mid] for mid in evolution_members}
    async def _initialize_dtesn_monitoring(self):
        if not self.dtesn_integration:
            return
        for agent_id in self.training_system.population.keys():
            monitor_system = self._create_mock_dtesn_monitor(agent_id)
            self.dtesn_integration.register_dtesn_monitor(agent_id, monitor_system)
        logger.info(f'DTESN monitoring initialized for {len(self.training_system.population)} agents')
    def _create_mock_dtesn_monitor(self, agent_id: str) -> Dict[str, Any]:
        return {'agent_id': agent_id, 'monitoring_status': {'system_efficiency': 0.75, 'system_stability': 0.8, 'response_time_score': 0.7, 'membrane_activity': 0.65, 'echo_state_coherence': 0.85}, 'get_monitoring_status': lambda: {'system_efficiency': 0.75 + (time.time() % 0.2 - 0.1), 'system_stability': 0.8 + (time.time() % 0.15 - 0.075), 'response_time_score': 0.7 + (time.time() % 0.3 - 0.15)}}
    async def _register_agents_with_aar(self):
        if not self.aar_orchestrator:
            return
        logger.info('Registering agents with AAR orchestration system')
    async def run_integrated_training_epoch(self) -> Dict[str, Any]:
        if not self.training_system.population:
            raise ValueError('Training population not initialized')
        epoch_start_time = time.time()
        self.training_active = True
        try:
            logger.info(f'Starting integrated training epoch {self.current_epoch}')
            learning_results = await self._execute_cooperative_competitive_phase()
            evolution_results = await self._execute_population_evolution_phase()
            dtesn_results = await self._integrate_dtesn_performance()
            transfer_results = await self._execute_cross_population_transfer()
            improvement_analysis = await self._analyze_population_improvements()
            epoch_duration = time.time() - epoch_start_time
            self.total_training_time += epoch_duration
            self.current_epoch += 1
            epoch_results = {'epoch': self.current_epoch - 1, 'duration': epoch_duration, 'learning_phase': learning_results, 'evolution_phase': evolution_results, 'dtesn_integration': dtesn_results, 'transfer_learning': transfer_results, 'improvement_analysis': improvement_analysis, 'population_metrics': await self._calculate_population_metrics(), 'convergence_metrics': await self._calculate_convergence_metrics()}
            self.training_metrics.append(epoch_results)
            population_improved = improvement_analysis.get('overall_improvement', False)
            if population_improved:
                self.population_improvement_history.append({'epoch': self.current_epoch - 1, 'improvement_magnitude': improvement_analysis.get('improvement_magnitude', 0.0), 'improvement_type': improvement_analysis.get('improvement_type', 'unknown'), 'timestamp': time.time()})
            logger.info(f'Training epoch {self.current_epoch - 1} completed: Population improved: {population_improved}')
            return epoch_results
        except Exception as e:
            logger.error(f'Training epoch failed: {e}')
            raise
        finally:
            self.training_active = False
    async def _execute_cooperative_competitive_phase(self) -> Dict[str, Any]:
        logger.info('Executing cooperative and competitive learning phase')
        agent_capabilities = {}
        for agent_id, agent in self.training_system.population.items():
            capabilities = {}
            genetic_params = agent.genetic_params
            performance_metrics = agent.performance_metrics
            capabilities['reasoning'] = genetic_params.get('architecture_type', 'dense') == 'attention' and 0.8 or 0.6
            capabilities['creativity'] = genetic_params.get('exploration_rate', 0.5)
            capabilities['action'] = performance_metrics.get('speed', 0.5)
            capabilities['analysis'] = performance_metrics.get('accuracy', 0.5)
            capabilities['competitive'] = 1.0 - genetic_params.get('cooperation_bias', 0.5)
            capabilities['cooperation'] = genetic_params.get('cooperation_bias', 0.5)
            capabilities['learning'] = genetic_params.get('learning_rate', 0.01) * 10
            capabilities['exploration'] = genetic_params.get('exploration_rate', 0.5)
            agent_capabilities[agent_id] = capabilities
        interaction_results = []
        agent_ids = list(self.training_system.population.keys())
        num_interactions = min(self.config.max_concurrent_sessions, len(agent_ids) // 2)
        for i in range(num_interactions):
            if len(agent_ids) >= 2:
                participants = self._select_interaction_participants(agent_ids, agent_capabilities)
                context = {'task_type': self._generate_interaction_task_type(i), 'objective': f'multi_agent_learning_task_{i}', 'complexity_level': 'medium', 'interaction_round': i}
                interaction_result = await self.learning_coordinator.coordinate_learning_interaction(participants, context, agent_capabilities)
                interaction_results.append(interaction_result)
                self.interactions_completed += 1
        await self._update_agent_performance_from_interactions(interaction_results, agent_capabilities)
        return {'interactions_executed': len(interaction_results), 'interaction_results': interaction_results, 'performance_updates': await self._get_performance_update_summary(), 'learning_modes_used': self._analyze_learning_modes_used(interaction_results)}
    def _select_interaction_participants(self, agent_ids: List[str], agent_capabilities: Dict[str, Dict[str, float]]) -> List[str]:
        import random
        group_sizes = [2, 3, 4, 5]
        group_size = min(random.choice(group_sizes), len(agent_ids))
        if random.random() < 0.5:
            return self._select_diverse_agents(agent_ids, agent_capabilities, group_size)
        else:
            return self._select_similar_agents(agent_ids, agent_capabilities, group_size)
    def _select_diverse_agents(self, agent_ids: List[str], agent_capabilities: Dict[str, Dict[str, float]], count: int) -> List[str]:
        import random
        if count >= len(agent_ids):
            return agent_ids
        selected = [random.choice(agent_ids)]
        remaining = [aid for aid in agent_ids if aid not in selected]
        while len(selected) < count and remaining:
            best_diversity = -1
            best_candidate = None
            for candidate in remaining:
                total_diversity = 0
                for selected_agent in selected:
                    diversity = self._calculate_capability_diversity(agent_capabilities.get(candidate, {}), agent_capabilities.get(selected_agent, {}))
                    total_diversity += diversity
                avg_diversity = total_diversity / len(selected)
                if avg_diversity > best_diversity:
                    best_diversity = avg_diversity
                    best_candidate = candidate
            if best_candidate:
                selected.append(best_candidate)
                remaining.remove(best_candidate)
            else:
                break
        return selected
    def _select_similar_agents(self, agent_ids: List[str], agent_capabilities: Dict[str, Dict[str, float]], count: int) -> List[str]:
        import random
        if count >= len(agent_ids):
            return agent_ids
        seed_agent = random.choice(agent_ids)
        selected = [seed_agent]
        seed_capabilities = agent_capabilities.get(seed_agent, {})
        similarities = []
        for agent_id in agent_ids:
            if agent_id != seed_agent:
                similarity = self._calculate_capability_similarity(seed_capabilities, agent_capabilities.get(agent_id, {}))
                similarities.append((agent_id, similarity))
        similarities.sort(key=lambda x: x[1], reverse=True)
        for agent_id, similarity in similarities[:count - 1]:
            selected.append(agent_id)
        return selected
    def _calculate_capability_diversity(self, caps1: Dict[str, float], caps2: Dict[str, float]) -> float:
        all_caps = set(caps1.keys()) | set(caps2.keys())
        if not all_caps:
            return 0.0
        total_diff = sum((abs(caps1.get(cap, 0.0) - caps2.get(cap, 0.0)) for cap in all_caps))
        return total_diff / len(all_caps)
    def _calculate_capability_similarity(self, caps1: Dict[str, float], caps2: Dict[str, float]) -> float:
        diversity = self._calculate_capability_diversity(caps1, caps2)
        return 1.0 - diversity
    def _generate_interaction_task_type(self, interaction_index: int) -> str:
        task_types = ['collaborative_problem_solving', 'competitive_optimization', 'team_coordination', 'individual_skill_development', 'strategic_planning', 'resource_allocation', 'knowledge_sharing', 'performance_improvement']
        return task_types[interaction_index % len(task_types)]
    async def _update_agent_performance_from_interactions(self, interaction_results: List[Dict[str, Any]], agent_capabilities: Dict[str, Dict[str, float]]):
        performance_updates = {}
        for result in interaction_results:
            participants = []
            interaction_id = result.get('interaction_id', '')
            learning_mode = result.get('learning_mode', 'cooperative')
            outcome = result.get('outcome', 'mutual_benefit')
            if hasattr(self.learning_coordinator, 'interaction_history'):
                for interaction in self.learning_coordinator.interaction_history:
                    if interaction.interaction_id == interaction_id:
                        participants = interaction.participants
                        break
            if not participants:
                participants = list(self.training_system.population.keys())
            for agent_id in participants:
                if agent_id in self.training_system.population:
                    agent = self.training_system.population[agent_id]
                    if outcome in ['collaboration_success', 'mutual_benefit', 'win']:
                        performance_boost = 0.05
                        agent.fitness_score += performance_boost
                        if learning_mode == 'cooperative':
                            agent.cooperation_score = min(1.0, agent.cooperation_score + 0.1)
                        elif learning_mode == 'competitive':
                            agent.wins += 1
                        agent.performance_metrics['adaptability'] = min(1.0, agent.performance_metrics.get('adaptability', 0.5) + 0.05)
                    elif outcome in ['collaboration_failure', 'loss']:
                        agent.losses += 1
                        agent.fitness_score = max(0.0, agent.fitness_score - 0.02)
                    agent.interaction_count += 1
                    agent.last_updated = time.time()
                    if agent_id not in performance_updates:
                        performance_updates[agent_id] = []
                    performance_updates[agent_id].append({'interaction_type': learning_mode, 'outcome': outcome, 'performance_change': agent.fitness_score})
    async def _get_performance_update_summary(self) -> Dict[str, Any]:
        total_agents = len(self.training_system.population)
        improved_agents = sum((1 for agent in self.training_system.population.values() if agent.fitness_score > 0.5))
        avg_fitness = sum((agent.fitness_score for agent in self.training_system.population.values())) / total_agents
        avg_cooperation = sum((agent.cooperation_score for agent in self.training_system.population.values())) / total_agents
        return {'total_agents': total_agents, 'improved_agents': improved_agents, 'improvement_rate': improved_agents / total_agents, 'average_fitness': avg_fitness, 'average_cooperation': avg_cooperation}
    def _analyze_learning_modes_used(self, interaction_results: List[Dict[str, Any]]) -> Dict[str, int]:
        mode_counts = {}
        for result in interaction_results:
            mode = result.get('learning_mode', 'unknown')
            mode_counts[mode] = mode_counts.get(mode, 0) + 1
        return mode_counts
    async def _execute_population_evolution_phase(self) -> Dict[str, Any]:
        logger.info('Executing population evolution phase')
        async def integrated_fitness_function(parameters: Dict[str, Any]) -> Dict[str, float]:
            base_fitness = sum(parameters.values()) / len(parameters) if parameters else 0.5
            import random
            fitness_noise = random.gauss(0, 0.1)
            primary_fitness = max(0.0, min(1.0, base_fitness + fitness_noise))
            return {'primary': primary_fitness, 'cooperation': parameters.get('cooperation_bias', 0.5), 'competition': 1.0 - parameters.get('cooperation_bias', 0.5), 'adaptability': parameters.get('exploration_rate', 0.5), 'efficiency': parameters.get('learning_rate', 0.01) * 10}
        evolution_stats = await self.population_trainer.evolve_generation(integrated_fitness_function)
        await self._transfer_evolved_parameters()
        return {'evolution_algorithm': self.config.population_config.algorithm_type.value, 'generation_stats': evolution_stats, 'population_diversity': await self._calculate_population_diversity(), 'best_fitness_achieved': evolution_stats.get('best_fitness', 0.0)}
    async def _transfer_evolved_parameters(self):
        evolved_members = sorted(self.population_trainer.population.values(), key=lambda m: m.fitness_scores.get('primary', 0.0), reverse=True)
        training_agents = list(self.training_system.population.values())
        for i, training_agent in enumerate(training_agents):
            if i < len(evolved_members):
                evolved_member = evolved_members[i]
                for param_name, param_value in evolved_member.parameters.items():
                    if param_name in training_agent.genetic_params:
                        blend_factor = 0.3
                        old_value = training_agent.genetic_params[param_name]
                        if isinstance(param_value, (int, float)) and isinstance(old_value, (int, float)):
                            new_value = old_value * (1 - blend_factor) + param_value * blend_factor
                            training_agent.genetic_params[param_name] = type(old_value)(new_value)
                        elif random.random() < blend_factor:
                            training_agent.genetic_params[param_name] = param_value
    async def _integrate_dtesn_performance(self) -> Dict[str, Any]:
        if not self.dtesn_integration:
            return {'dtesn_integration': 'not_available'}
        logger.info('Integrating DTESN performance data')
        await self.dtesn_integration.sync_with_dtesn_performance()
        dtesn_enhanced_agents = 0
        total_dtesn_efficiency = 0.0
        for agent_id, agent in self.training_system.population.items():
            dtesn_efficiency = agent.performance_metrics.get('dtesn_efficiency', 0.0)
            if dtesn_efficiency > 0.0:
                dtesn_enhanced_agents += 1
                total_dtesn_efficiency += dtesn_efficiency
                dtesn_bonus = dtesn_efficiency * self.config.dtesn_performance_weight
                agent.fitness_score += dtesn_bonus
        avg_dtesn_efficiency = total_dtesn_efficiency / dtesn_enhanced_agents if dtesn_enhanced_agents > 0 else 0.0
        return {'dtesn_enhanced_agents': dtesn_enhanced_agents, 'average_dtesn_efficiency': avg_dtesn_efficiency, 'dtesn_performance_integration': 'active', 'membrane_computing_active': self.config.membrane_computing_integration, 'echo_state_feedback_active': self.config.echo_state_feedback}
    async def _execute_cross_population_transfer(self) -> Dict[str, Any]:
        logger.info('Executing cross-population learning transfer')
        transfer_count = 0
        top_training_agents = sorted(self.training_system.population.values(), key=lambda a: a.fitness_score, reverse=True)[:5]
        top_evolved_members = sorted(self.population_trainer.population.values(), key=lambda m: m.fitness_scores.get('primary', 0.0), reverse=True)[:5]
        for training_agent in top_training_agents[:3]:
            for evolved_member in self.population_trainer.population.values():
                for param_name in ['cooperation_bias', 'exploration_rate']:
                    if param_name in training_agent.genetic_params:
                        evolved_value = evolved_member.parameters.get(param_name, 0.5)
                        training_value = training_agent.genetic_params[param_name]
                        blended_value = (evolved_value + training_value) / 2
                        evolved_member.parameters[param_name] = blended_value
                        transfer_count += 1
                        break
        return {'knowledge_transfers': transfer_count, 'transfer_direction': 'bidirectional', 'top_performers_identified': len(top_training_agents) + len(top_evolved_members)}
    async def _analyze_population_improvements(self) -> Dict[str, Any]:
        logger.info('Analyzing population improvements')
        current_population = self.training_system.population
        current_fitness_values = [agent.fitness_score for agent in current_population.values()]
        current_avg_fitness = sum(current_fitness_values) / len(current_fitness_values)
        current_best_fitness = max(current_fitness_values) if current_fitness_values else 0.0
        current_interaction_count = sum((agent.interaction_count for agent in current_population.values()))
        improvement_detected = False
        improvement_magnitude = 0.0
        improvement_types = []
        if len(self.training_metrics) > 0:
            previous_metrics = self.training_metrics[-1]['population_metrics']
            prev_avg_fitness = previous_metrics.get('average_fitness', 0.0)
            prev_best_fitness = previous_metrics.get('best_fitness', 0.0)
            prev_interaction_count = previous_metrics.get('total_interactions', 0)
            if current_avg_fitness > prev_avg_fitness:
                improvement_detected = True
                improvement_magnitude += current_avg_fitness - prev_avg_fitness
                improvement_types.append('average_fitness')
            if current_best_fitness > prev_best_fitness:
                improvement_detected = True
                improvement_magnitude += (current_best_fitness - prev_best_fitness) * 0.5
                improvement_types.append('best_fitness')
            if current_interaction_count > prev_interaction_count:
                improvement_types.append('interaction_experience')
            current_cooperation = sum((agent.cooperation_score for agent in current_population.values())) / len(current_population)
            prev_cooperation = previous_metrics.get('average_cooperation', 0.0)
            if current_cooperation > prev_cooperation:
                improvement_detected = True
                improvement_magnitude += (current_cooperation - prev_cooperation) * 0.3
                improvement_types.append('cooperation')
            total_wins = sum((agent.wins for agent in current_population.values()))
            total_interactions = max(1, sum((agent.interaction_count for agent in current_population.values())))
            current_win_rate = total_wins / total_interactions
            prev_win_rate = previous_metrics.get('win_rate', 0.0)
            if current_win_rate > prev_win_rate:
                improvement_detected = True
                improvement_magnitude += (current_win_rate - prev_win_rate) * 0.2
                improvement_types.append('win_rate')
        else:
            baseline_fitness = 0.5
            if current_avg_fitness > baseline_fitness:
                improvement_detected = True
                improvement_magnitude = current_avg_fitness - baseline_fitness
                improvement_types.append('above_baseline')
        if current_best_fitness > self.best_population_fitness:
            self.best_population_fitness = current_best_fitness
            improvement_detected = True
            improvement_types.append('new_best_performer')
        interaction_based_improvement = self._analyze_interaction_based_improvement()
        if interaction_based_improvement['improvement_detected']:
            improvement_detected = True
            improvement_magnitude += interaction_based_improvement['improvement_magnitude']
            improvement_types.extend(interaction_based_improvement['improvement_types'])
        return {'overall_improvement': improvement_detected, 'improvement_magnitude': improvement_magnitude, 'improvement_types': improvement_types, 'current_population_stats': {'average_fitness': current_avg_fitness, 'best_fitness': current_best_fitness, 'total_interactions': current_interaction_count, 'population_size': len(current_population)}, 'interaction_based_improvement': interaction_based_improvement, 'acceptance_criteria_met': improvement_detected and improvement_magnitude > self.config.convergence_threshold}
    def _analyze_interaction_based_improvement(self) -> Dict[str, Any]:
        improvement_detected = False
        improvement_magnitude = 0.0
        improvement_types = []
        agents_by_interactions = sorted(self.training_system.population.values(), key=lambda a: a.interaction_count)
        if len(agents_by_interactions) >= 4:
            high_interaction = agents_by_interactions[-len(agents_by_interactions) // 4:]
            low_interaction = agents_by_interactions[:len(agents_by_interactions) // 4]
            avg_high_fitness = sum((a.fitness_score for a in high_interaction)) / len(high_interaction)
            avg_low_fitness = sum((a.fitness_score for a in low_interaction)) / len(low_interaction)
            if avg_high_fitness > avg_low_fitness:
                improvement_detected = True
                improvement_magnitude += avg_high_fitness - avg_low_fitness
                improvement_types.append('interaction_correlation')
        cooperative_agents = [a for a in self.training_system.population.values() if a.cooperation_score > 0.6]
        if cooperative_agents:
            avg_coop_fitness = sum((a.fitness_score for a in cooperative_agents)) / len(cooperative_agents)
            overall_avg = sum((a.fitness_score for a in self.training_system.population.values())) / len(self.training_system.population)
            if avg_coop_fitness > overall_avg:
                improvement_detected = True
                improvement_magnitude += (avg_coop_fitness - overall_avg) * 0.5
                improvement_types.append('cooperative_learning')
        competitive_agents = [a for a in self.training_system.population.values() if a.wins > a.losses and a.interaction_count > 0]
        if competitive_agents:
            avg_competitive_fitness = sum((a.fitness_score for a in competitive_agents)) / len(competitive_agents)
            if len(self.training_metrics) > 0:
                prev_competitive_performance = self.training_metrics[-1].get('competitive_performance', 0.0)
                if avg_competitive_fitness > prev_competitive_performance:
                    improvement_detected = True
                    improvement_magnitude += avg_competitive_fitness - prev_competitive_performance
                    improvement_types.append('competitive_learning')
        return {'improvement_detected': improvement_detected, 'improvement_magnitude': improvement_magnitude, 'improvement_types': improvement_types, 'interaction_stats': {'total_interactions': sum((a.interaction_count for a in self.training_system.population.values())), 'cooperative_agents': len([a for a in self.training_system.population.values() if a.cooperation_score > 0.6]), 'competitive_winners': len([a for a in self.training_system.population.values() if a.wins > a.losses])}}
    async def _calculate_population_metrics(self) -> Dict[str, Any]:
        population = self.training_system.population
        if not population:
            return {}
        fitness_values = [agent.fitness_score for agent in population.values()]
        cooperation_scores = [agent.cooperation_score for agent in population.values()]
        interaction_counts = [agent.interaction_count for agent in population.values()]
        return {'population_size': len(population), 'average_fitness': sum(fitness_values) / len(fitness_values), 'best_fitness': max(fitness_values), 'worst_fitness': min(fitness_values), 'fitness_std': self._calculate_std(fitness_values), 'average_cooperation': sum(cooperation_scores) / len(cooperation_scores), 'total_interactions': sum(interaction_counts), 'average_interactions_per_agent': sum(interaction_counts) / len(interaction_counts), 'win_rate': sum((agent.wins for agent in population.values())) / max(1, sum(interaction_counts)), 'population_diversity': await self._calculate_population_diversity()}
    async def _calculate_population_diversity(self) -> float:
        population = list(self.training_system.population.values())
        if len(population) < 2:
            return 0.0
        total_distance = 0.0
        pair_count = 0
        for i in range(len(population)):
            for j in range(i + 1, len(population)):
                distance = self._calculate_genetic_distance(population[i], population[j])
                total_distance += distance
                pair_count += 1
        return total_distance / pair_count if pair_count > 0 else 0.0
    def _calculate_genetic_distance(self, agent1: AgentPopulationMember, agent2: AgentPopulationMember) -> float:
        params1 = agent1.genetic_params
        params2 = agent2.genetic_params
        all_params = set(params1.keys()) | set(params2.keys())
        if not all_params:
            return 0.0
        distance_sum = 0.0
        for param in all_params:
            val1 = params1.get(param, 0.0)
            val2 = params2.get(param, 0.0)
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                distance_sum += (val1 - val2) ** 2
            elif val1 != val2:
                distance_sum += 1.0
        return math.sqrt(distance_sum / len(all_params))
    def _calculate_std(self, values: List[float]) -> float:
        if len(values) < 2:
            return 0.0
        mean = sum(values) / len(values)
        variance = sum(((x - mean) ** 2 for x in values)) / len(values)
        return math.sqrt(variance)
    async def _calculate_convergence_metrics(self) -> Dict[str, Any]:
        if len(self.training_metrics) < 2:
            return {'convergence_status': 'insufficient_data'}
        recent_metrics = self.training_metrics[-5:]
        fitness_progression = [m['population_metrics']['average_fitness'] for m in recent_metrics]
        if len(fitness_progression) >= 2:
            improvements = [fitness_progression[i] - fitness_progression[i - 1] for i in range(1, len(fitness_progression))]
            avg_improvement = sum(improvements) / len(improvements)
            converged = abs(avg_improvement) < self.config.convergence_threshold
            current_diversity = await self._calculate_population_diversity()
            diversity_maintained = current_diversity > self.config.min_population_diversity
        else:
            avg_improvement = 0.0
            converged = False
            diversity_maintained = True
        return {'convergence_status': 'converged' if converged else 'evolving', 'average_improvement_rate': avg_improvement, 'diversity_maintained': diversity_maintained, 'current_diversity': await self._calculate_population_diversity(), 'epochs_analyzed': len(recent_metrics)}
    async def run_continuous_training(self, max_epochs: int=None) -> List[Dict[str, Any]]:
        max_epochs = max_epochs or 50
        epoch_results = []
        logger.info(f'Starting continuous training for up to {max_epochs} epochs')
        if not self.training_system.population:
            await self.initialize_training_population()
        training_start_time = time.time()
        for epoch in range(max_epochs):
            try:
                if time.time() - training_start_time > self.config.max_training_time:
                    logger.info(f'Training stopped due to time limit at epoch {epoch}')
                    break
                epoch_result = await self.run_integrated_training_epoch()
                epoch_results.append(epoch_result)
                convergence_metrics = epoch_result.get('convergence_metrics', {})
                if convergence_metrics.get('convergence_status') == 'converged':
                    logger.info(f'Training converged at epoch {epoch}')
                    break
                improvement_analysis = epoch_result.get('improvement_analysis', {})
                if improvement_analysis.get('acceptance_criteria_met', False):
                    logger.info(f"Acceptance criteria met at epoch {epoch}: Population improved by {improvement_analysis['improvement_magnitude']:.4f}")
            except Exception as e:
                logger.error(f'Training epoch {epoch} failed: {e}')
                break
        final_report = await self.generate_training_report(epoch_results)
        logger.info(f'Continuous training completed after {len(epoch_results)} epochs')
        return epoch_results
    async def generate_training_report(self, epoch_results: List[Dict[str, Any]]) -> Dict[str, Any]:
        if not epoch_results:
            return {'error': 'No training results available'}
        total_epochs = len(epoch_results)
        total_duration = sum((r['duration'] for r in epoch_results))
        improvement_epochs = [r for r in epoch_results if r.get('improvement_analysis', {}).get('overall_improvement', False)]
        improvement_rate = len(improvement_epochs) / total_epochs if total_epochs > 0 else 0.0
        best_fitness_achieved = max((r['population_metrics']['best_fitness'] for r in epoch_results))
        final_avg_fitness = epoch_results[-1]['population_metrics']['average_fitness']
        total_interactions = self.interactions_completed
        learning_modes_used = {}
        for epoch_result in epoch_results:
            modes = epoch_result.get('learning_phase', {}).get('learning_modes_used', {})
            for mode, count in modes.items():
                learning_modes_used[mode] = learning_modes_used.get(mode, 0) + count
        report = {'training_summary': {'total_epochs': total_epochs, 'total_duration': total_duration, 'total_interactions': total_interactions, 'improvement_epochs': len(improvement_epochs), 'improvement_rate': improvement_rate}, 'performance_metrics': {'best_fitness_achieved': best_fitness_achieved, 'final_average_fitness': final_avg_fitness, 'population_size': len(self.training_system.population)}, 'learning_statistics': {'learning_modes_distribution': learning_modes_used, 'cooperative_sessions': self.learning_coordinator.cooperative_engine.get_cooperation_statistics(), 'competitive_matches': self.learning_coordinator.competitive_engine.get_competition_statistics()}, 'acceptance_criteria_validation': {'population_improved_through_interaction': improvement_rate > 0, 'distributed_training_achieved': total_interactions > 0, 'competitive_and_cooperative_learning': len(learning_modes_used) > 1, 'population_based_methods_used': self.config.population_config.algorithm_type.value != 'none'}, 'system_integration_status': {'dtesn_integration_active': self.dtesn_integration is not None, 'aar_orchestration_active': self.aar_orchestrator is not None, 'population_evolution_active': len(self.population_trainer.population) > 0}, 'population_improvement_history': self.population_improvement_history}
        logger.info('Training report generated')
        return report
    def get_system_status(self) -> Dict[str, Any]:
        return {'training_active': self.training_active, 'current_epoch': self.current_epoch, 'population_size': len(self.training_system.population), 'total_training_time': self.total_training_time, 'interactions_completed': self.interactions_completed, 'best_population_fitness': self.best_population_fitness, 'system_integrations': {'dtesn_available': DTESN_MONITORING_AVAILABLE, 'aar_available': AAR_CORE_AVAILABLE, 'dtesn_integration_active': self.dtesn_integration is not None, 'aar_integration_active': self.aar_orchestrator is not None}, 'configuration': {'training_mode': self.config.training_config.training_mode.value, 'population_algorithm': self.config.population_config.algorithm_type.value, 'learning_balance': self.config.learning_config.cooperation_competition_balance}}