"""
AAR Core Orchestrator

Central orchestration system for agent-arena-relation management.
Integrates with Aphrodite Engine and Echo-Self Evolution Engine.
"""

from typing import Dict, List, Any, Optional
import asyncio
import logging
import uuid
from dataclasses import dataclass

from ..agents.agent_manager import AgentManager, AgentCapabilities, AgentStatus
from ..arena.simulation_engine import SimulationEngine, ArenaType
from ..relations.relation_graph import RelationGraph

# Import social cognition extensions
try:
    from ..agents.social_cognition_manager import SocialCognitionManager
    from ..relations.communication_protocols import CommunicationProtocols
    from ..orchestration.collaborative_solver import CollaborativeProblemSolver
    SOCIAL_COGNITION_AVAILABLE = True
except ImportError as e:
    logger.warning(f"Social cognition components not available: {e}")
    SOCIAL_COGNITION_AVAILABLE = False

logger = logging.getLogger(__name__)


@dataclass
class AARConfig:
    """Configuration for AAR orchestration system."""
    max_concurrent_agents: int = 1000
    arena_simulation_enabled: bool = True
    relation_graph_depth: int = 3
    resource_allocation_strategy: str = "adaptive"
    agent_lifecycle_timeout: int = 300  # seconds
    performance_monitoring_interval: int = 10  # seconds
    # Social cognition configuration
    social_cognition_enabled: bool = True
    max_shared_resources: int = 1000
    max_concurrent_problems: int = 100
    communication_protocols_enabled: bool = True


class AARCoreOrchestrator:
    """Central orchestration system for agent-arena-relation management."""
    
    def __init__(self, config: AARConfig):
        self.config = config
        
        # Initialize core components
        self.agent_manager = AgentManager(config.max_concurrent_agents)
        self.simulation_engine = SimulationEngine() if config.arena_simulation_enabled else None
        self.relation_graph = RelationGraph(config.relation_graph_depth)
        
        # Initialize social cognition components
        self.social_cognition_manager = None
        self.communication_protocols = None
        self.collaborative_solver = None
        
        if SOCIAL_COGNITION_AVAILABLE and config.social_cognition_enabled:
            self.social_cognition_manager = SocialCognitionManager(config.max_shared_resources)
            logger.info("Social Cognition Manager initialized")
            
            if config.communication_protocols_enabled:
                self.communication_protocols = CommunicationProtocols()
                logger.info("Communication Protocols initialized")
            
            self.collaborative_solver = CollaborativeProblemSolver(config.max_concurrent_problems)
            logger.info("Collaborative Problem Solver initialized")
        else:
            logger.info("Social cognition extensions disabled or unavailable")
        
        # Integration points
        self.aphrodite_engine = None
        self.dtesn_kernel = None
        self.echo_self_engine = None
        
        # Performance metrics
        self.performance_stats = {
            'total_requests': 0,
            'active_agents_count': 0,
            'arena_utilization': 0.0,
            'avg_response_time': 0.0,
            'error_rate': 0.0
        }
        
        logger.info(f"AAR Core Orchestrator initialized with config: {config}")
    
    def set_aphrodite_integration(self, aphrodite_engine):
        """Set Aphrodite Engine integration."""
        self.aphrodite_engine = aphrodite_engine
        logger.info("Aphrodite Engine integration enabled")
    
    def set_dtesn_integration(self, dtesn_kernel):
        """Set DTESN kernel integration."""
        self.dtesn_kernel = dtesn_kernel
        logger.info("DTESN kernel integration enabled")
    
    def set_echo_self_integration(self, echo_self_engine):
        """Set Echo-Self evolution engine integration."""
        self.echo_self_engine = echo_self_engine
        logger.info("Echo-Self evolution engine integration enabled")
    
    def enable_echo_self_integration(self, echo_self_engine):
        """Enable integration with Echo-Self evolution engine (alias)."""
        self.set_echo_self_integration(echo_self_engine)
    
    async def orchestrate_inference(self, 
                                      request: Dict[str, Any]) -> Dict[str, Any]:
        """Orchestrate inference through agent-arena system with social cognition."""
        try:
            self.performance_stats['total_requests'] += 1
            start_time = asyncio.get_event_loop().time()
            
            # Step 1: Allocate appropriate agents for request
            allocated_agents = await self._allocate_agents(request)
            
            if not allocated_agents:
                return {'error': 'No agents available for request'}
            
            # Step 2: Register agents for social cognition (if enabled)
            if self.social_cognition_manager:
                await self._register_agents_for_social_cognition(allocated_agents)
            
            # Step 3: Create or select virtual arena if simulation enabled
            arena_id = None
            if self.simulation_engine:
                arena_id = await self._get_arena(request.get('context', {}))
            
            # Step 4: Check if this requires collaborative problem solving
            if self._requires_collaboration(request) and self.collaborative_solver:
                return await self._handle_collaborative_request(request, allocated_agents, arena_id, start_time)
            
            # Step 5: Execute distributed inference (original logic)
            results = []
            for agent_id in allocated_agents:
                # Get agent data
                agent_data = self.agent_manager.get_agent_status(agent_id)
                if not agent_data:
                    continue
                
                # Update agent with current membrane states
                await self._sync_agent_membranes(agent_data)
                
                # Execute in virtual arena or directly
                if arena_id and self.simulation_engine:
                    agent_result = await self.simulation_engine.execute_agent_in_arena(
                        arena_id, agent_data, request
                    )
                else:
                    # Execute directly through agent manager
                    agent_result = await self.agent_manager.process_agent_request(
                        agent_id, request
                    )
                
                results.append(agent_result)
            
            # Step 6: Aggregate results through relation graph
            final_result = await self.relation_graph.aggregate_results(results)
            
            # Step 7: Update relationships based on performance
            await self._update_relations(allocated_agents, final_result)
            
            # Update performance metrics
            end_time = asyncio.get_event_loop().time()
            response_time = end_time - start_time
            await self._update_performance_stats(response_time, success=True)
            
            # Add orchestration metadata
            final_result.update({
                'orchestration_meta': {
                    'agents_used': len(allocated_agents),
                    'arena_id': arena_id,
                    'processing_time': response_time,
                    'request_id': request.get('request_id', 'unknown'),
                    'social_cognition_enabled': self.social_cognition_manager is not None,
                    'collaboration_used': False
                }
            })
            
            return final_result
            
        except Exception as e:
            logger.error(f"Error in orchestrate_inference: {e}")
            await self._update_performance_stats(0.0, success=False)
            return {'error': str(e)}
    
    async def _allocate_agents(self, request: Dict[str, Any]) -> List[str]:
        """Allocate appropriate agents for request."""
        # Determine required agent count and capabilities
        agent_count = self._calculate_required_agents(request)
        
        # Use agent manager to allocate agents
        allocated_agents = await self.agent_manager.allocate_agents(request, agent_count)
        
        logger.debug(f"Allocated {len(allocated_agents)} agents for request")
        return allocated_agents
    
    def _calculate_required_agents(self, request: Dict[str, Any]) -> int:
        """Calculate number of agents required for request."""
        # Basic heuristic - in practice this would be more sophisticated
        base_agents = 1
        
        # Adjust based on request complexity
        if 'complex_reasoning' in request.get('features', []):
            base_agents += 2
        
        if 'collaboration' in request.get('features', []):
            base_agents += 2  # Collaboration requires multiple agents
            
        if 'multi_modal' in request.get('features', []):
            base_agents += 1
        
        if request.get('priority', 'normal') == 'high':
            base_agents += 1
        
        # Check for explicit minimum agent requirement
        min_agents = request.get('required_capabilities', {}).get('min_agents', 1)
        base_agents = max(base_agents, min_agents)
        
        # Check context requirements
        context = request.get('context', {})
        if context.get('interaction_type') == 'complex_collaboration':
            base_agents = max(base_agents, 3)
        
        return min(base_agents, 10)  # Cap at reasonable limit
    
    async def _get_arena(self, context: Dict[str, Any]) -> str:
        """Create or select virtual arena."""
        if not self.simulation_engine:
            return None
        
        # Determine arena type from context
        arena_type_name = context.get('arena_type', 'GENERAL')
        arena_type = ArenaType.GENERAL
        
        try:
            arena_type = ArenaType(arena_type_name.lower())
        except ValueError:
            logger.warning(f"Unknown arena type {arena_type_name}, using GENERAL")
        
        # Get or create arena
        arena_id = await self.simulation_engine.get_or_create_arena(context, arena_type)
        
        return arena_id
    
    async def _sync_agent_membranes(self, agent_data: Dict[str, Any]) -> None:
        """Update agent with current membrane states."""
        if self.dtesn_kernel:
            # Get current membrane states from DTESN kernel
            membrane_states = await self._get_dtesn_membrane_states()
            agent_data['membrane_states'] = membrane_states
    
    async def _get_dtesn_membrane_states(self) -> Dict[str, Any]:
        """Get current DTESN membrane states."""
        # Placeholder for DTESN integration
        return {
            'hierarchy_depth': 4,
            'active_membranes': [],
            'reservoir_dynamics': {},
            'evolution_state': {}
        }
    
    async def _update_relations(self, agent_ids: List[str], result: Dict[str, Any]) -> None:
        """Update relationships based on performance."""
        if len(agent_ids) < 2:
            return
        
        # Create agent data for relation graph
        agents = [{'id': agent_id} for agent_id in agent_ids]
        
        # Extract performance score from result
        performance_score = result.get('consensus_confidence', 0.5)
        
        # Update relationships through relation graph
        await self.relation_graph.update_relationships(agents, performance_score)
    
    async def _update_performance_stats(self, response_time: float, success: bool) -> None:
        """Update system performance statistics."""
        # Update average response time
        total_requests = self.performance_stats['total_requests']
        current_avg = self.performance_stats['avg_response_time']
        
        new_avg = ((current_avg * (total_requests - 1)) + response_time) / total_requests
        self.performance_stats['avg_response_time'] = new_avg
        
        # Update error rate
        if not success:
            errors = self.performance_stats.get('total_errors', 0) + 1
            self.performance_stats['total_errors'] = errors
            self.performance_stats['error_rate'] = errors / total_requests
        
        # Update active agents count
        if self.agent_manager:
            agent_stats = self.agent_manager.get_system_stats()
            self.performance_stats['active_agents_count'] = agent_stats['agent_counts']['active']
        
        # Update arena utilization
        if self.simulation_engine:
            arenas = self.simulation_engine.list_arenas()
            active_arenas = len([a for a in arenas if a['active_sessions'] > 0])
            total_arenas = len(arenas)
            self.performance_stats['arena_utilization'] = active_arenas / max(total_arenas, 1)
    
    async def get_orchestration_stats(self) -> Dict[str, Any]:
        """Get current orchestration statistics."""
        agent_stats = self.agent_manager.get_system_stats() if self.agent_manager else {}
        simulation_stats = self.simulation_engine.get_system_stats() if self.simulation_engine else {}
        relation_stats = self.relation_graph.get_graph_stats() if self.relation_graph else {}
        
        # Get social cognition stats
        social_cognition_stats = {}
        communication_stats = {}
        collaborative_solver_stats = {}
        
        if self.social_cognition_manager:
            social_cognition_stats = self.social_cognition_manager.get_social_cognition_stats()
            
        if self.communication_protocols:
            communication_stats = self.communication_protocols.get_communication_stats()
            
        if self.collaborative_solver:
            collaborative_solver_stats = self.collaborative_solver.get_solver_stats()
        
        # Extract active agents count for top-level access
        active_agents_count = 0
        if agent_stats and 'agent_counts' in agent_stats:
            active_agents_count = agent_stats['agent_counts'].get('active', 0)
        
        return {
            'performance_stats': self.performance_stats,
            'config': self.config,
            'active_agents_count': active_agents_count,  # Add for test compatibility
            'integration_status': {
                'aphrodite_engine': self.aphrodite_engine is not None,
                'dtesn_kernel': self.dtesn_kernel is not None,
                'echo_self_engine': self.echo_self_engine is not None,
                'social_cognition_manager': self.social_cognition_manager is not None,
                'communication_protocols': self.communication_protocols is not None,
                'collaborative_solver': self.collaborative_solver is not None
            },
            'component_stats': {
                'agents': agent_stats,
                'simulation': simulation_stats,
                'relations': relation_stats,
                'social_cognition': social_cognition_stats,
                'communication': communication_stats,
                'collaborative_solver': collaborative_solver_stats
            },
            'system_health': await self._calculate_system_health()
        }
    
    async def _calculate_system_health(self) -> Dict[str, Any]:
        """Calculate overall system health metrics."""
        health_score = 1.0
        
        # Adjust based on error rate
        error_rate = self.performance_stats.get('error_rate', 0.0)
        health_score -= error_rate
        
        # Adjust based on agent manager health
        if self.agent_manager:
            agent_health = self.agent_manager.get_system_stats()['health_status']
            health_score *= agent_health['overall_score']
        
        return {
            'overall_score': max(0.0, min(1.0, health_score)),
            'components_healthy': {
                'agent_manager': self.agent_manager is not None,
                'simulation_engine': self.simulation_engine is not None,
                'relation_graph': self.relation_graph is not None
            },
            'error_rate': error_rate,
            'status': 'healthy' if health_score > 0.8 else 'degraded' if health_score > 0.5 else 'critical'
        }
    
    async def run_agent_evaluation(self, agent_data: Dict[str, Any]) -> Dict[str, Any]:
        """Run agent evaluation in virtual arena for evolution engine."""
        try:
            # Spawn agent for evaluation if not already active
            agent_id = agent_data.get('id')
            if not agent_id or agent_id not in self.agent_manager.agents:
                # Create temporary agent for evaluation
                capabilities = AgentCapabilities(
                    reasoning=agent_data.get('reasoning', True),
                    multimodal=agent_data.get('multimodal', False),
                    memory_enabled=agent_data.get('memory_enabled', True),
                    learning_enabled=agent_data.get('learning_enabled', True),
                    collaboration=agent_data.get('collaboration', True)
                )
                agent_id = await self.agent_manager.spawn_agent(capabilities, context={'evaluation': True})
            
            # Create evaluation arena
            arena_id = None
            if self.simulation_engine:
                arena_context = {
                    'evaluation_mode': True,
                    'agent_count': 1,
                    'arena_type': 'evaluation'
                }
                arena_id = await self._get_arena(arena_context)
            
            # Run evaluation tasks
            evaluation_tasks = [
                {'type': 'reasoning', 'complexity': 'medium'},
                {'type': 'problem_solving', 'complexity': 'high'},
                {'type': 'adaptation', 'complexity': 'variable'}
            ]
            
            results = []
            for task in evaluation_tasks:
                task_result = await self._evaluate_agent_task(agent_id, task, arena_id)
                results.append(task_result)
            
            # Calculate overall fitness score
            fitness_score = self._calculate_agent_fitness(results)
            
            # Clean up temporary agent if created for evaluation
            if agent_data.get('temporary_agent', False):
                await self.agent_manager.terminate_agent(agent_id)
            
            return {
                'agent_id': agent_id,
                'arena_id': arena_id,
                'fitness_score': fitness_score,
                'task_results': results,
                'evaluation_timestamp': asyncio.get_event_loop().time()
            }
            
        except Exception as e:
            logger.error(f"Error during agent evaluation: {e}")
            return {
                'agent_id': agent_data.get('id'),
                'fitness_score': 0.0,
                'error': str(e),
                'evaluation_timestamp': asyncio.get_event_loop().time()
            }
    
    async def _evaluate_agent_task(self, agent_id: str, task: Dict[str, Any], arena_id: Optional[str] = None) -> Dict[str, Any]:
        """Evaluate agent performance on a specific task."""
        start_time = asyncio.get_event_loop().time()
        
        # Simulate task execution in arena
        if arena_id and self.simulation_engine:
            arena_result = await self._run_arena_task(agent_id, task, arena_id)
        else:
            # Basic task execution without arena
            arena_result = await self._run_basic_task(agent_id, task)
        
        end_time = asyncio.get_event_loop().time()
        execution_time = end_time - start_time
        
        return {
            'task_type': task['type'],
            'complexity': task['complexity'],
            'execution_time': execution_time,
            'success_rate': arena_result.get('success_rate', 0.5),
            'performance_score': arena_result.get('performance_score', 0.5),
            'arena_interactions': arena_result.get('interactions', [])
        }
    
    async def _run_arena_task(self, agent_id: str, task: Dict[str, Any], arena_id: str) -> Dict[str, Any]:
        """Run task in virtual arena environment."""
        # Simulate arena-based task execution
        await asyncio.sleep(0.01)  # Simulate processing time
        
        # Mock arena interaction results based on task complexity
        base_performance = 0.6
        complexity_factor = {'low': 1.2, 'medium': 1.0, 'high': 0.8, 'variable': 0.9}.get(task['complexity'], 1.0)
        
        performance_score = min(1.0, base_performance * complexity_factor)
        success_rate = max(0.1, performance_score - 0.1)
        
        return {
            'success_rate': success_rate,
            'performance_score': performance_score,
            'interactions': [
                {'type': 'environment_analysis', 'score': performance_score},
                {'type': 'task_execution', 'score': success_rate},
                {'type': 'adaptation', 'score': min(1.0, performance_score + 0.1)}
            ]
        }
    
    async def _run_basic_task(self, agent_id: str, task: Dict[str, Any]) -> Dict[str, Any]:
        """Run basic task without arena simulation."""
        await asyncio.sleep(0.005)  # Simulate processing time
        
        # Mock basic task results
        base_performance = 0.5
        complexity_factor = {'low': 1.1, 'medium': 1.0, 'high': 0.9, 'variable': 0.95}.get(task['complexity'], 1.0)
        
        performance_score = min(1.0, base_performance * complexity_factor)
        
        return {
            'success_rate': performance_score,
            'performance_score': performance_score,
            'interactions': []
        }
    
    def _calculate_agent_fitness(self, task_results: List[Dict[str, Any]]) -> float:
        """Calculate overall agent fitness from task results."""
        if not task_results:
            return 0.0
        
        # Weighted fitness calculation
        total_score = 0.0
        total_weight = 0.0
        
        task_weights = {
            'reasoning': 0.4,
            'problem_solving': 0.4,
            'adaptation': 0.2
        }
        
        for result in task_results:
            task_type = result['task_type']
            weight = task_weights.get(task_type, 0.3)
            performance = result['performance_score']
            
            # Adjust for execution efficiency
            time_penalty = min(0.1, result['execution_time'] * 0.01)
            adjusted_performance = max(0.0, performance - time_penalty)
            
            total_score += adjusted_performance * weight
            total_weight += weight
        
        return total_score / total_weight if total_weight > 0 else 0.0
    
    async def update_agent_configurations(self, agent_configs: List[Dict[str, Any]]) -> None:
        """Update agent configurations after evolution."""
        # First, clean up old agents to make room for evolved ones
        active_count = len([a for a in self.agent_manager.agents.values() 
                           if a.status not in [AgentStatus.TERMINATED, AgentStatus.ERROR]])
        
        # Calculate available capacity
        available_capacity = self.config.max_concurrent_agents - active_count
        
        # Limit the number of new agents to available capacity
        max_new_agents = min(len(agent_configs), available_capacity)
        
        if max_new_agents < len(agent_configs):
            logger.warning(f"Limited new agents to {max_new_agents} due to capacity constraints")
            agent_configs = agent_configs[:max_new_agents]
        
        for config in agent_configs:
            agent_id = config.get('id')
            if agent_id and agent_id in self.agent_manager.agents:
                # Update agent with evolved parameters
                await self._apply_evolved_config(agent_id, config)
            elif not config.get('temporary_agent', False):  # Only create permanent agents
                # Spawn new evolved agent
                capabilities = self._config_to_capabilities(config)
                context = {'evolved': True, 'generation': config.get('generation', 0)}
                try:
                    new_agent_id = await self.agent_manager.spawn_agent(capabilities, context)
                    logger.info(f"Spawned evolved agent {new_agent_id} from generation {config.get('generation', 0)}")
                except Exception as e:
                    logger.warning(f"Failed to spawn evolved agent: {e}")
                    break  # Stop spawning if we hit capacity
    
    async def _apply_evolved_config(self, agent_id: str, config: Dict[str, Any]) -> None:
        """Apply evolved configuration to existing agent."""
        agent = self.agent_manager.agents.get(agent_id)
        if agent:
            # Update agent parameters
            evolution_data = {
                'new_capabilities': config.get('capabilities', {}),
                'learning_parameters': config.get('learning_params', {}),
                'performance_targets': config.get('targets', {})
            }
            await self.agent_manager.evolve_agent(agent_id, evolution_data)
    
    def _config_to_capabilities(self, config: Dict[str, Any]) -> AgentCapabilities:
        """Convert evolution config to agent capabilities."""
        cap_data = config.get('capabilities', {})
        return AgentCapabilities(
            reasoning=cap_data.get('reasoning', True),
            multimodal=cap_data.get('multimodal', False),
            memory_enabled=cap_data.get('memory_enabled', True),
            learning_enabled=cap_data.get('learning_enabled', True),
            collaboration=cap_data.get('collaboration', True),
            specialized_domains=cap_data.get('domains', []),
            max_context_length=cap_data.get('context_length', 4096),
            processing_power=cap_data.get('processing_power', 1.0)
        )
    
    async def shutdown(self) -> None:
        """Graceful shutdown of orchestration system."""
        logger.info("Shutting down AAR Core Orchestrator...")
        
        # Shutdown social cognition components first
        if self.collaborative_solver:
            await self.collaborative_solver.shutdown()
        
        if self.communication_protocols:
            await self.communication_protocols.shutdown()
            
        if self.social_cognition_manager:
            await self.social_cognition_manager.shutdown()
        
        # Shutdown core components
        if self.agent_manager:
            await self.agent_manager.shutdown()
        
        if self.simulation_engine:
            await self.simulation_engine.shutdown()
        
        if self.relation_graph:
            await self.relation_graph.shutdown()
        
        logger.info("AAR Core Orchestrator shutdown complete")
    
    # Social Cognition Helper Methods
    
    def _requires_collaboration(self, request: Dict[str, Any]) -> bool:
        """Determine if request requires collaborative problem solving."""
        if not self.collaborative_solver:
            return False
            
        # Check for explicit collaboration requirement
        if request.get('collaboration_required', False):
            return True
        
        # Check for collaboration indicators in features
        features = request.get('features', [])
        collaboration_indicators = ['collaboration', 'complex_reasoning', 'distributed_problem', 'multi_agent']
        
        if any(indicator in features for indicator in collaboration_indicators):
            return True
        
        # Check context for collaboration hints
        context = request.get('context', {})
        if context.get('problem_complexity', 'low') in ['high', 'very_high']:
            return True
            
        return False
    
    async def _register_agents_for_social_cognition(self, agent_ids: List[str]) -> None:
        """Register agents for social cognition if enabled."""
        if not self.social_cognition_manager:
            return
        
        for agent_id in agent_ids:
            agent = self.agent_manager.agents.get(agent_id)
            if agent and hasattr(agent, 'cognitive_profile'):
                await self.social_cognition_manager.register_agent(agent_id, agent.cognitive_profile)
                
                # Also register for communication protocols
                if self.communication_protocols:
                    await self.communication_protocols.register_agent(agent_id)
    
    async def _handle_collaborative_request(self, 
                                          request: Dict[str, Any], 
                                          allocated_agents: List[str], 
                                          arena_id: Optional[str],
                                          start_time: float) -> Dict[str, Any]:
        """Handle request that requires collaborative problem solving."""
        try:
            # Import here to avoid circular imports
            from ..orchestration.collaborative_solver import ProblemDefinition, SolutionStrategy
            
            # Create problem definition
            problem = ProblemDefinition(
                problem_id=f"prob_{uuid.uuid4().hex[:8]}",
                problem_type=self._determine_problem_type(request),
                title=request.get('title', 'Collaborative Problem'),
                description=request.get('description', 'Complex problem requiring collaboration'),
                objectives=request.get('objectives', ['Solve the problem effectively']),
                constraints=request.get('constraints', {}),
                success_criteria=request.get('success_criteria', {'quality': 0.8}),
                complexity_level=request.get('context', {}).get('problem_complexity', 'medium'),
                required_capabilities=request.get('required_capabilities', {}).get('capabilities', []),
                input_data=request.get('input_data', {}),
                context=request.get('context', {})
            )
            
            # Determine strategy
            strategy = SolutionStrategy.CONSENSUS
            if 'strategy' in request:
                try:
                    strategy = SolutionStrategy(request['strategy'])
                except ValueError:
                    pass  # Use default
            
            # Start collaborative problem solving
            session_id = await self.collaborative_solver.initiate_collaborative_problem(
                problem, allocated_agents, allocated_agents[0], strategy
            )
            
            # Get agent capabilities for task assignment
            agent_capabilities = {}
            for agent_id in allocated_agents:
                agent = self.agent_manager.agents.get(agent_id)
                if agent and hasattr(agent, 'capabilities'):
                    caps = agent.capabilities
                    agent_capabilities[agent_id] = []
                    
                    if caps.reasoning:
                        agent_capabilities[agent_id].extend(['reasoning', 'logical_analysis'])
                    if caps.multimodal:
                        agent_capabilities[agent_id].extend(['multimodal', 'pattern_recognition'])
                    if caps.learning_enabled:
                        agent_capabilities[agent_id].extend(['learning', 'adaptation'])
                    if caps.collaboration:
                        agent_capabilities[agent_id].extend(['collaboration', 'communication'])
                    
                    # Add specialized domains
                    agent_capabilities[agent_id].extend(caps.specialized_domains)
            
            # Assign tasks to agents
            assignments = await self.collaborative_solver.assign_tasks_to_agents(session_id, agent_capabilities)
            
            # Simulate task execution and solution submission
            # In a real implementation, this would involve actual task processing
            await asyncio.sleep(0.1)  # Simulate processing time
            
            # Submit mock solutions for demonstration
            session = self.collaborative_solver.active_sessions.get(session_id)
            if session:
                for agent_id, task_ids in assignments.items():
                    for task_id in task_ids:
                        solution_data = {
                            'solution_approach': f'Approach from {agent_id}',
                            'results': {'status': 'completed', 'quality': 0.8},
                            'reasoning': f'Agent {agent_id} completed task {task_id} using collaborative approach'
                        }
                        
                        await self.collaborative_solver.submit_task_solution(
                            session_id, task_id, agent_id, solution_data, 0.8
                        )
            
            # Check for completion (might already be completed if all tasks were submitted)
            final_result = {'collaborative_solution': True}
            
            if session_id not in self.collaborative_solver.active_sessions:
                # Problem was completed
                completed_sessions = [s for s in self.collaborative_solver.completed_sessions 
                                    if s.session_id == session_id]
                if completed_sessions:
                    completed_session = completed_sessions[-1]
                    final_result = {
                        'collaborative_solution': True,
                        'problem_id': problem.problem_id,
                        'session_id': session_id,
                        'final_solution': completed_session.final_solution,
                        'participating_agents': allocated_agents,
                        'collaboration_metrics': completed_session.collaboration_metrics
                    }
            else:
                # Problem still in progress
                problem_status = self.collaborative_solver.get_problem_status(session_id)
                final_result = {
                    'collaborative_solution': True,
                    'problem_id': problem.problem_id,
                    'session_id': session_id,
                    'status': 'in_progress',
                    'progress': problem_status['progress'] if problem_status else {},
                    'participating_agents': allocated_agents
                }
            
            # Update performance metrics
            end_time = asyncio.get_event_loop().time()
            response_time = end_time - start_time
            await self._update_performance_stats(response_time, success=True)
            
            # Add orchestration metadata
            final_result.update({
                'orchestration_meta': {
                    'agents_used': len(allocated_agents),
                    'arena_id': arena_id,
                    'processing_time': response_time,
                    'request_id': request.get('request_id', 'unknown'),
                    'social_cognition_enabled': True,
                    'collaboration_used': True,
                    'problem_type': problem.problem_type.value if hasattr(problem.problem_type, 'value') else str(problem.problem_type)
                }
            })
            
            return final_result
            
        except Exception as e:
            logger.error(f"Error in collaborative request handling: {e}")
            return {'error': f'Collaborative processing failed: {str(e)}'}
    
    def _determine_problem_type(self, request: Dict[str, Any]):
        """Determine the type of problem for collaborative solving."""
        # Import here to avoid circular imports
        try:
            from ..orchestration.collaborative_solver import ProblemType
        except ImportError:
            return 'reasoning'  # Fallback string
        
        # Check explicit problem type
        problem_type_str = request.get('problem_type', '').lower()
        
        try:
            return ProblemType(problem_type_str)
        except (ValueError, AttributeError):
            pass
        
        # Infer from features
        features = request.get('features', [])
        
        if any(f in features for f in ['optimization', 'minimize', 'maximize']):
            return ProblemType.OPTIMIZATION
        elif any(f in features for f in ['classification', 'categorize', 'classify']):
            return ProblemType.CLASSIFICATION
        elif any(f in features for f in ['planning', 'plan', 'strategy']):
            return ProblemType.PLANNING
        elif any(f in features for f in ['search', 'find', 'locate']):
            return ProblemType.SEARCH
        elif any(f in features for f in ['reasoning', 'logic', 'inference']):
            return ProblemType.REASONING
        elif any(f in features for f in ['creative', 'synthesis', 'innovation']):
            return ProblemType.CREATIVE_SYNTHESIS
        elif any(f in features for f in ['data_analysis', 'analytics', 'statistics']):
            return ProblemType.DATA_ANALYSIS
        elif any(f in features for f in ['simulation', 'modeling', 'simulate']):
            return ProblemType.SIMULATION
        else:
            return ProblemType.REASONING  # Default fallback