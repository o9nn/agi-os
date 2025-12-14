"""
Cooperative and Competitive Learning Mechanisms

Implements advanced learning mechanisms for multi-agent systems including
cooperative learning, competitive learning, and hybrid approaches.

This module supports Task 4.2.3 requirements for competitive and cooperative learning.
"""

import asyncio
import logging
import time
import uuid
import random
import math
from typing import Dict, List, Any, Optional, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict, deque

logger = logging.getLogger(__name__)


class LearningMode(Enum):
    """Types of learning interactions."""
    COOPERATIVE = "cooperative"
    COMPETITIVE = "competitive" 
    IMITATIVE = "imitative"
    ADVERSARIAL = "adversarial"
    COLLABORATIVE = "collaborative"
    PEER_TO_PEER = "peer_to_peer"


class InteractionOutcome(Enum):
    """Outcomes of agent interactions."""
    WIN = "win"
    LOSS = "loss"
    DRAW = "draw"
    COLLABORATION_SUCCESS = "collaboration_success"
    COLLABORATION_FAILURE = "collaboration_failure"
    MUTUAL_BENEFIT = "mutual_benefit"
    KNOWLEDGE_TRANSFER = "knowledge_transfer"


@dataclass
class LearningInteraction:
    """Record of a learning interaction between agents."""
    interaction_id: str
    participants: List[str]  # agent_ids
    learning_mode: LearningMode
    outcome: InteractionOutcome
    performance_metrics: Dict[str, float]
    knowledge_gained: Dict[str, Any]
    duration: float
    timestamp: float = field(default_factory=time.time)
    context: Dict[str, Any] = field(default_factory=dict)


@dataclass
class CooperativeLearningSession:
    """A cooperative learning session between multiple agents."""
    session_id: str
    participants: List[str]
    objective: str
    shared_knowledge: Dict[str, Any] = field(default_factory=dict)
    individual_contributions: Dict[str, float] = field(default_factory=dict)
    collective_performance: float = 0.0
    synergy_metrics: Dict[str, float] = field(default_factory=dict)
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None


@dataclass 
class CompetitiveLearningMatch:
    """A competitive learning match between agents."""
    match_id: str
    competitors: List[str]
    match_type: str
    winner: Optional[str] = None
    scores: Dict[str, float] = field(default_factory=dict)
    strategies_observed: Dict[str, List[str]] = field(default_factory=dict)
    skill_improvements: Dict[str, float] = field(default_factory=dict)
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None


@dataclass
class LearningConfiguration:
    """Configuration for cooperative and competitive learning."""
    # Cooperative learning parameters
    cooperation_reward_factor: float = 1.2
    knowledge_sharing_rate: float = 0.3
    collective_bonus_threshold: float = 0.8
    min_cooperation_group_size: int = 2
    max_cooperation_group_size: int = 5
    
    # Competitive learning parameters
    competition_intensity: float = 1.0
    skill_adaptation_rate: float = 0.2
    strategy_learning_rate: float = 0.1
    ranking_update_factor: float = 0.15
    
    # Hybrid learning parameters
    cooperation_competition_balance: float = 0.5  # 0 = all competitive, 1 = all cooperative
    mode_switching_probability: float = 0.1
    interaction_history_length: int = 100
    
    # Knowledge transfer parameters
    imitation_learning_rate: float = 0.25
    expert_identification_threshold: float = 0.75
    knowledge_decay_rate: float = 0.05


class CooperativeLearningEngine:
    """Engine for managing cooperative learning between agents."""
    
    def __init__(self, config: LearningConfiguration):
        self.config = config
        self.active_sessions: Dict[str, CooperativeLearningSession] = {}
        self.completed_sessions: List[CooperativeLearningSession] = []
        self.shared_knowledge_repository: Dict[str, Any] = {}
        self.agent_cooperation_history: Dict[str, List[str]] = defaultdict(list)
        
        logger.info("Initialized CooperativeLearningEngine")

    async def create_cooperation_session(self, 
                                       participants: List[str], 
                                       objective: str,
                                       context: Dict[str, Any] = None) -> CooperativeLearningSession:
        """Create a new cooperative learning session."""
        if len(participants) < self.config.min_cooperation_group_size:
            raise ValueError(f"Need at least {self.config.min_cooperation_group_size} participants")
        
        if len(participants) > self.config.max_cooperation_group_size:
            participants = participants[:self.config.max_cooperation_group_size]
        
        session = CooperativeLearningSession(
            session_id=f"coop_{uuid.uuid4().hex[:8]}",
            participants=participants,
            objective=objective
        )
        
        if context:
            session.shared_knowledge.update(context)
        
        self.active_sessions[session.session_id] = session
        
        # Update cooperation history
        for participant in participants:
            self.agent_cooperation_history[participant].extend(
                [p for p in participants if p != participant]
            )
        
        logger.info(f"Created cooperation session {session.session_id} with {len(participants)} participants")
        return session

    async def execute_cooperative_learning(self, 
                                         session: CooperativeLearningSession,
                                         agent_capabilities: Dict[str, Dict[str, float]],
                                         task_executor: Callable) -> Dict[str, Any]:
        """Execute a cooperative learning session."""
        start_time = time.time()
        
        # Initialize individual contributions
        for participant in session.participants:
            session.individual_contributions[participant] = 0.0
        
        # Execute cooperative task
        task_results = await self._execute_cooperative_task(
            session, agent_capabilities, task_executor
        )
        
        # Calculate performance metrics
        collective_performance = await self._calculate_collective_performance(
            session, task_results
        )
        
        session.collective_performance = collective_performance
        
        # Calculate synergy metrics
        session.synergy_metrics = await self._calculate_synergy_metrics(
            session, agent_capabilities, task_results
        )
        
        # Update shared knowledge repository
        await self._update_shared_knowledge(session, task_results)
        
        # Apply cooperation rewards
        learning_gains = await self._apply_cooperation_rewards(
            session, collective_performance
        )
        
        session.end_time = time.time()
        
        # Move to completed sessions
        if session.session_id in self.active_sessions:
            del self.active_sessions[session.session_id]
        self.completed_sessions.append(session)
        
        results = {
            'session_id': session.session_id,
            'collective_performance': collective_performance,
            'individual_contributions': session.individual_contributions.copy(),
            'synergy_metrics': session.synergy_metrics.copy(),
            'learning_gains': learning_gains,
            'duration': session.end_time - session.start_time,
            'knowledge_created': len(task_results.get('new_knowledge', {}))
        }
        
        logger.info(f"Completed cooperative learning session: {results}")
        return results

    async def _execute_cooperative_task(self,
                                      session: CooperativeLearningSession,
                                      agent_capabilities: Dict[str, Dict[str, float]],
                                      task_executor: Callable) -> Dict[str, Any]:
        """Execute the cooperative task with all participants."""
        # Divide task based on agent capabilities
        task_assignments = await self._assign_cooperative_tasks(
            session.participants, agent_capabilities, session.objective
        )
        
        # Execute individual contributions
        individual_results = {}
        for participant, assigned_tasks in task_assignments.items():
            try:
                if asyncio.iscoroutinefunction(task_executor):
                    result = await task_executor(participant, assigned_tasks, session.shared_knowledge)
                else:
                    result = task_executor(participant, assigned_tasks, session.shared_knowledge)
                
                individual_results[participant] = result
                
                # Calculate individual contribution score
                contribution_score = self._calculate_contribution_score(result)
                session.individual_contributions[participant] = contribution_score
                
            except Exception as e:
                logger.warning(f"Task execution failed for {participant}: {e}")
                individual_results[participant] = {'success': False, 'error': str(e)}
                session.individual_contributions[participant] = 0.0
        
        return {
            'individual_results': individual_results,
            'task_assignments': task_assignments,
            'collaboration_metrics': self._calculate_collaboration_metrics(individual_results)
        }

    async def _assign_cooperative_tasks(self,
                                      participants: List[str],
                                      agent_capabilities: Dict[str, Dict[str, float]],
                                      objective: str) -> Dict[str, List[str]]:
        """Assign cooperative tasks based on agent capabilities."""
        # Simple capability-based assignment
        task_assignments = defaultdict(list)
        
        # Define task types based on objective
        if "problem_solving" in objective.lower():
            available_tasks = ["analysis", "synthesis", "evaluation", "implementation"]
        elif "optimization" in objective.lower():
            available_tasks = ["exploration", "exploitation", "validation", "refinement"]
        elif "learning" in objective.lower():
            available_tasks = ["data_collection", "pattern_recognition", "knowledge_integration", "testing"]
        else:
            available_tasks = ["research", "planning", "execution", "validation"]
        
        # Assign tasks based on capabilities
        for participant in participants:
            capabilities = agent_capabilities.get(participant, {})
            
            # Simple matching of tasks to best capabilities
            participant_tasks = []
            for task in available_tasks:
                capability_match = 0.0
                
                if task in ["analysis", "research"]:
                    capability_match = capabilities.get("reasoning", 0.5)
                elif task in ["synthesis", "planning"]: 
                    capability_match = capabilities.get("creativity", 0.5)
                elif task in ["implementation", "execution"]:
                    capability_match = capabilities.get("action", 0.5)
                elif task in ["evaluation", "validation"]:
                    capability_match = capabilities.get("analysis", 0.5)
                else:
                    capability_match = sum(capabilities.values()) / len(capabilities) if capabilities else 0.5
                
                if capability_match > 0.6:  # Assign if capability is above threshold
                    participant_tasks.append(task)
            
            task_assignments[participant] = participant_tasks or ["general_support"]
        
        return dict(task_assignments)

    def _calculate_contribution_score(self, result: Dict[str, Any]) -> float:
        """Calculate an individual agent's contribution score."""
        if not isinstance(result, dict):
            return 0.0
        
        score = 0.0
        
        # Success factor
        if result.get('success', False):
            score += 0.4
        
        # Quality metrics
        quality = result.get('quality', 0.5)
        score += 0.3 * quality
        
        # Efficiency metrics  
        efficiency = result.get('efficiency', 0.5)
        score += 0.2 * efficiency
        
        # Innovation metrics
        innovation = result.get('innovation', 0.0)
        score += 0.1 * innovation
        
        return min(1.0, score)

    def _calculate_collaboration_metrics(self, individual_results: Dict[str, Any]) -> Dict[str, float]:
        """Calculate collaboration quality metrics."""
        successful_agents = sum(1 for result in individual_results.values() 
                              if result.get('success', False))
        
        total_agents = len(individual_results)
        success_rate = successful_agents / total_agents if total_agents > 0 else 0.0
        
        # Calculate coordination quality
        coordination_scores = []
        for result in individual_results.values():
            coord_score = result.get('coordination_quality', 0.5)
            coordination_scores.append(coord_score)
        
        avg_coordination = sum(coordination_scores) / len(coordination_scores) if coordination_scores else 0.5
        
        return {
            'success_rate': success_rate,
            'coordination_quality': avg_coordination,
            'participation_rate': 1.0  # All participants attempted the task
        }

    async def _calculate_collective_performance(self,
                                              session: CooperativeLearningSession,
                                              task_results: Dict[str, Any]) -> float:
        """Calculate the collective performance of the cooperation session."""
        individual_contributions = list(session.individual_contributions.values())
        
        if not individual_contributions:
            return 0.0
        
        # Base performance as average of individual contributions
        base_performance = sum(individual_contributions) / len(individual_contributions)
        
        # Collaboration bonus
        collaboration_metrics = task_results.get('collaboration_metrics', {})
        coordination_bonus = collaboration_metrics.get('coordination_quality', 0.5) * 0.2
        
        # Synergy bonus (non-linear combination effect)
        synergy_factor = 1.0
        if len(individual_contributions) > 1:
            # Bonus for having diverse contributions
            contribution_variance = math.sqrt(
                sum((c - base_performance)**2 for c in individual_contributions) / len(individual_contributions)
            )
            diversity_bonus = min(0.1, contribution_variance * 0.5)
            synergy_factor += diversity_bonus
        
        # Group size bonus (diminishing returns)
        group_size = len(session.participants)
        size_bonus = math.log(group_size) * 0.05 if group_size > 1 else 0.0
        
        collective_performance = base_performance * synergy_factor + coordination_bonus + size_bonus
        
        return min(1.0, collective_performance)

    async def _calculate_synergy_metrics(self,
                                       session: CooperativeLearningSession,
                                       agent_capabilities: Dict[str, Dict[str, float]],
                                       task_results: Dict[str, Any]) -> Dict[str, float]:
        """Calculate synergy metrics for the cooperation session."""
        participants = session.participants
        
        # Capability complementarity
        capability_diversity = self._calculate_capability_diversity(participants, agent_capabilities)
        
        # Communication effectiveness
        collaboration_metrics = task_results.get('collaboration_metrics', {})
        communication_quality = collaboration_metrics.get('coordination_quality', 0.5)
        
        # Knowledge integration quality
        knowledge_integration = self._calculate_knowledge_integration_quality(
            session, task_results
        )
        
        # Emergent behaviors (performance beyond sum of parts)
        expected_performance = sum(session.individual_contributions.values())
        actual_performance = session.collective_performance * len(participants)
        emergence_factor = max(0.0, (actual_performance - expected_performance) / expected_performance) \
                          if expected_performance > 0 else 0.0
        
        return {
            'capability_diversity': capability_diversity,
            'communication_quality': communication_quality, 
            'knowledge_integration': knowledge_integration,
            'emergence_factor': emergence_factor,
            'overall_synergy': (capability_diversity + communication_quality + 
                              knowledge_integration + emergence_factor) / 4.0
        }

    def _calculate_capability_diversity(self,
                                      participants: List[str],
                                      agent_capabilities: Dict[str, Dict[str, float]]) -> float:
        """Calculate diversity of capabilities across participants."""
        if len(participants) < 2:
            return 0.0
        
        # Get all capability types
        all_capabilities = set()
        for participant in participants:
            caps = agent_capabilities.get(participant, {})
            all_capabilities.update(caps.keys())
        
        if not all_capabilities:
            return 0.0
        
        # Calculate pairwise capability distances
        total_distance = 0.0
        pair_count = 0
        
        for i in range(len(participants)):
            for j in range(i + 1, len(participants)):
                agent1_caps = agent_capabilities.get(participants[i], {})
                agent2_caps = agent_capabilities.get(participants[j], {})
                
                # Calculate Euclidean distance in capability space
                distance = 0.0
                for cap in all_capabilities:
                    val1 = agent1_caps.get(cap, 0.0)
                    val2 = agent2_caps.get(cap, 0.0)
                    distance += (val1 - val2) ** 2
                
                distance = math.sqrt(distance)
                total_distance += distance
                pair_count += 1
        
        return total_distance / pair_count if pair_count > 0 else 0.0

    def _calculate_knowledge_integration_quality(self,
                                               session: CooperativeLearningSession,
                                               task_results: Dict[str, Any]) -> float:
        """Calculate how well knowledge was integrated across participants."""
        individual_results = task_results.get('individual_results', {})
        
        # Check for knowledge cross-references and integration
        integration_indicators = 0
        total_possible = len(session.participants) * (len(session.participants) - 1)
        
        for participant, result in individual_results.items():
            if isinstance(result, dict):
                # Look for references to other participants' work
                referenced_participants = result.get('references_to_others', [])
                integration_indicators += len(referenced_participants)
                
                # Look for integrated solutions
                if result.get('integrated_solution', False):
                    integration_indicators += 2
        
        return min(1.0, integration_indicators / total_possible) if total_possible > 0 else 0.0

    async def _update_shared_knowledge(self, 
                                     session: CooperativeLearningSession,
                                     task_results: Dict[str, Any]):
        """Update the shared knowledge repository with new insights."""
        new_knowledge = {}
        
        # Extract knowledge from individual results
        individual_results = task_results.get('individual_results', {})
        for participant, result in individual_results.items():
            if isinstance(result, dict) and result.get('new_insights'):
                insights = result['new_insights']
                for insight_key, insight_value in insights.items():
                    knowledge_key = f"{session.objective}_{insight_key}"
                    if knowledge_key not in new_knowledge:
                        new_knowledge[knowledge_key] = {
                            'value': insight_value,
                            'contributors': [participant],
                            'session_id': session.session_id,
                            'created_at': time.time()
                        }
                    else:
                        # Multiple contributors to same insight
                        new_knowledge[knowledge_key]['contributors'].append(participant)
        
        # Update repository
        self.shared_knowledge_repository.update(new_knowledge)
        
        # Update session shared knowledge
        session.shared_knowledge.update(new_knowledge)

    async def _apply_cooperation_rewards(self,
                                       session: CooperativeLearningSession,
                                       collective_performance: float) -> Dict[str, float]:
        """Apply rewards based on cooperation success."""
        learning_gains = {}
        
        for participant in session.participants:
            individual_contribution = session.individual_contributions.get(participant, 0.0)
            
            # Base learning gain from individual contribution
            base_gain = individual_contribution * 0.5
            
            # Cooperation bonus
            if collective_performance > self.config.collective_bonus_threshold:
                cooperation_bonus = (collective_performance - self.config.collective_bonus_threshold) * \
                                  self.config.cooperation_reward_factor
                base_gain += cooperation_bonus
            
            # Synergy bonus
            synergy_score = session.synergy_metrics.get('overall_synergy', 0.0)
            synergy_bonus = synergy_score * 0.2
            
            total_gain = base_gain + synergy_bonus
            learning_gains[participant] = total_gain
        
        return learning_gains

    def get_cooperation_statistics(self) -> Dict[str, Any]:
        """Get comprehensive cooperation statistics."""
        total_sessions = len(self.completed_sessions)
        
        if total_sessions == 0:
            return {'total_sessions': 0, 'active_sessions': len(self.active_sessions)}
        
        # Calculate aggregate metrics
        avg_collective_performance = sum(s.collective_performance for s in self.completed_sessions) / total_sessions
        
        synergy_scores = []
        for session in self.completed_sessions:
            synergy = session.synergy_metrics.get('overall_synergy', 0.0)
            synergy_scores.append(synergy)
        
        avg_synergy = sum(synergy_scores) / len(synergy_scores) if synergy_scores else 0.0
        
        # Most cooperative agents
        cooperation_counts = defaultdict(int)
        for session in self.completed_sessions:
            for participant in session.participants:
                cooperation_counts[participant] += 1
        
        most_cooperative = sorted(cooperation_counts.items(), key=lambda x: x[1], reverse=True)[:5]
        
        return {
            'total_sessions': total_sessions,
            'active_sessions': len(self.active_sessions),
            'average_collective_performance': avg_collective_performance,
            'average_synergy_score': avg_synergy,
            'most_cooperative_agents': most_cooperative,
            'shared_knowledge_items': len(self.shared_knowledge_repository),
            'average_session_size': sum(len(s.participants) for s in self.completed_sessions) / total_sessions
        }


class CompetitiveLearningEngine:
    """Engine for managing competitive learning between agents."""
    
    def __init__(self, config: LearningConfiguration):
        self.config = config
        self.active_matches: Dict[str, CompetitiveLearningMatch] = {}
        self.completed_matches: List[CompetitiveLearningMatch] = []
        self.agent_rankings: Dict[str, float] = defaultdict(lambda: 1000.0)  # ELO-style ratings
        self.skill_profiles: Dict[str, Dict[str, float]] = defaultdict(dict)
        self.strategy_knowledge: Dict[str, List[str]] = defaultdict(list)
        
        logger.info("Initialized CompetitiveLearningEngine")

    async def create_competitive_match(self,
                                     competitors: List[str],
                                     match_type: str,
                                     context: Dict[str, Any] = None) -> CompetitiveLearningMatch:
        """Create a new competitive match."""
        if len(competitors) < 2:
            raise ValueError("Need at least 2 competitors")
        
        match = CompetitiveLearningMatch(
            match_id=f"match_{uuid.uuid4().hex[:8]}",
            competitors=competitors,
            match_type=match_type
        )
        
        # Initialize scores
        for competitor in competitors:
            match.scores[competitor] = 0.0
            match.strategies_observed[competitor] = []
            match.skill_improvements[competitor] = 0.0
        
        self.active_matches[match.match_id] = match
        
        logger.info(f"Created competitive match {match.match_id} with {len(competitors)} competitors")
        return match

    async def execute_competitive_learning(self,
                                         match: CompetitiveLearningMatch,
                                         agent_strategies: Dict[str, Dict[str, Any]],
                                         competition_executor: Callable) -> Dict[str, Any]:
        """Execute a competitive learning match."""
        start_time = time.time()
        
        # Execute the competition
        competition_results = await self._execute_competition(
            match, agent_strategies, competition_executor
        )
        
        # Determine winner and update scores
        winner, final_scores = await self._determine_winner(match, competition_results)
        match.winner = winner
        match.scores.update(final_scores)
        
        # Learn from competition
        learning_results = await self._process_competitive_learning(
            match, agent_strategies, competition_results
        )
        
        # Update rankings
        await self._update_rankings(match)
        
        # Record strategy observations
        await self._record_strategy_observations(match, agent_strategies)
        
        match.end_time = time.time()
        
        # Move to completed matches
        if match.match_id in self.active_matches:
            del self.active_matches[match.match_id]
        self.completed_matches.append(match)
        
        results = {
            'match_id': match.match_id,
            'winner': winner,
            'final_scores': final_scores.copy(),
            'learning_improvements': {k: v for k, v in match.skill_improvements.items()},
            'ranking_changes': await self._calculate_ranking_changes(match),
            'duration': match.end_time - match.start_time,
            'strategies_learned': sum(len(strategies) for strategies in match.strategies_observed.values())
        }
        
        logger.info(f"Completed competitive match: {results}")
        return results

    async def _execute_competition(self,
                                 match: CompetitiveLearningMatch,
                                 agent_strategies: Dict[str, Dict[str, Any]], 
                                 competition_executor: Callable) -> Dict[str, Any]:
        """Execute the competitive task."""
        try:
            if asyncio.iscoroutinefunction(competition_executor):
                results = await competition_executor(match.competitors, agent_strategies, match.match_type)
            else:
                results = competition_executor(match.competitors, agent_strategies, match.match_type)
            
            return results
            
        except Exception as e:
            logger.error(f"Competition execution failed: {e}")
            # Return default results
            return {
                'scores': {competitor: 0.0 for competitor in match.competitors},
                'performance_metrics': {},
                'error': str(e)
            }

    async def _determine_winner(self,
                              match: CompetitiveLearningMatch,
                              competition_results: Dict[str, Any]) -> Tuple[str, Dict[str, float]]:
        """Determine the winner and final scores."""
        scores = competition_results.get('scores', {})
        
        # Ensure all competitors have scores
        for competitor in match.competitors:
            if competitor not in scores:
                scores[competitor] = 0.0
        
        # Find winner (highest score)
        winner = max(scores.keys(), key=lambda k: scores[k])
        
        return winner, scores

    async def _process_competitive_learning(self,
                                          match: CompetitiveLearningMatch,
                                          agent_strategies: Dict[str, Dict[str, Any]],
                                          competition_results: Dict[str, Any]) -> Dict[str, Any]:
        """Process learning from the competitive match."""
        learning_results = {}
        
        for competitor in match.competitors:
            competitor_learning = {}
            
            # Analyze performance vs strategy
            competitor_score = match.scores.get(competitor, 0.0)
            competitor_strategy = agent_strategies.get(competitor, {})
            
            # Learn from successful strategies of opponents
            if competitor != match.winner and match.winner:
                winner_strategy = agent_strategies.get(match.winner, {})
                learned_strategies = await self._learn_from_winner_strategy(
                    competitor, competitor_strategy, winner_strategy
                )
                competitor_learning['learned_strategies'] = learned_strategies
            
            # Skill adaptation based on performance
            skill_improvement = await self._calculate_skill_improvement(
                competitor, competitor_score, competition_results
            )
            match.skill_improvements[competitor] = skill_improvement
            competitor_learning['skill_improvement'] = skill_improvement
            
            # Strategy refinement
            strategy_refinement = await self._refine_strategy(
                competitor, competitor_strategy, competitor_score
            )
            competitor_learning['strategy_refinement'] = strategy_refinement
            
            learning_results[competitor] = competitor_learning
        
        return learning_results

    async def _learn_from_winner_strategy(self,
                                        learner: str,
                                        learner_strategy: Dict[str, Any],
                                        winner_strategy: Dict[str, Any]) -> List[str]:
        """Learn successful strategies from the winner."""
        learned_strategies = []
        
        for strategy_key, strategy_value in winner_strategy.items():
            if strategy_key not in learner_strategy or \
               learner_strategy[strategy_key] != strategy_value:
                
                # Learn this strategy with some probability
                if random.random() < self.config.strategy_learning_rate:
                    learned_strategies.append(f"{strategy_key}={strategy_value}")
                    
                    # Update learner's strategy knowledge
                    strategy_description = f"{strategy_key}:{strategy_value}"
                    if strategy_description not in self.strategy_knowledge[learner]:
                        self.strategy_knowledge[learner].append(strategy_description)
        
        return learned_strategies

    async def _calculate_skill_improvement(self,
                                         agent: str,
                                         score: float,
                                         competition_results: Dict[str, Any]) -> float:
        """Calculate skill improvement based on competition performance."""
        performance_metrics = competition_results.get('performance_metrics', {})
        agent_metrics = performance_metrics.get(agent, {})
        
        # Base improvement from score
        score_improvement = score * self.config.skill_adaptation_rate
        
        # Additional improvement from specific performance metrics
        metric_improvement = 0.0
        for metric_name, metric_value in agent_metrics.items():
            if isinstance(metric_value, (int, float)) and metric_value > 0.5:
                metric_improvement += (metric_value - 0.5) * 0.1
        
        total_improvement = score_improvement + metric_improvement
        
        # Update skill profile
        current_skills = self.skill_profiles[agent]
        skill_type = f"competitive_{competition_results.get('competition_type', 'general')}"
        current_skill = current_skills.get(skill_type, 0.5)
        updated_skill = min(1.0, current_skill + total_improvement)
        current_skills[skill_type] = updated_skill
        
        return total_improvement

    async def _refine_strategy(self,
                             agent: str,
                             current_strategy: Dict[str, Any],
                             performance_score: float) -> Dict[str, Any]:
        """Refine agent strategy based on performance."""
        refined_strategy = current_strategy.copy()
        
        # If performance was poor, try different strategy parameters
        if performance_score < 0.5:
            for key, value in refined_strategy.items():
                if isinstance(value, (int, float)) and random.random() < 0.3:
                    # Apply random perturbation
                    if isinstance(value, float):
                        perturbation = random.gauss(0, abs(value) * 0.1 or 0.1)
                        refined_strategy[key] = max(0, value + perturbation)
                    elif isinstance(value, int):
                        perturbation = random.randint(-1, 1)
                        refined_strategy[key] = max(0, value + perturbation)
        
        return refined_strategy

    async def _update_rankings(self, match: CompetitiveLearningMatch):
        """Update ELO-style rankings based on match results."""
        if not match.winner or len(match.competitors) != 2:
            return  # Only update for 1v1 matches for simplicity
        
        winner = match.winner
        loser = [c for c in match.competitors if c != winner][0]
        
        winner_rating = self.agent_rankings[winner]
        loser_rating = self.agent_rankings[loser]
        
        # ELO rating calculation
        expected_winner = 1 / (1 + 10**((loser_rating - winner_rating) / 400))
        expected_loser = 1 - expected_winner
        
        k_factor = 32 * self.config.ranking_update_factor
        
        new_winner_rating = winner_rating + k_factor * (1 - expected_winner)
        new_loser_rating = loser_rating + k_factor * (0 - expected_loser)
        
        self.agent_rankings[winner] = new_winner_rating
        self.agent_rankings[loser] = new_loser_rating

    async def _record_strategy_observations(self,
                                          match: CompetitiveLearningMatch,
                                          agent_strategies: Dict[str, Dict[str, Any]]):
        """Record strategy observations for future learning."""
        for competitor in match.competitors:
            observed_strategies = []
            
            # Observe strategies of other competitors
            for other_competitor in match.competitors:
                if other_competitor != competitor:
                    other_strategy = agent_strategies.get(other_competitor, {})
                    for key, value in other_strategy.items():
                        strategy_desc = f"{other_competitor}_{key}:{value}"
                        observed_strategies.append(strategy_desc)
            
            match.strategies_observed[competitor] = observed_strategies

    async def _calculate_ranking_changes(self, match: CompetitiveLearningMatch) -> Dict[str, float]:
        """Calculate how rankings changed after the match."""
        # This is a simplified calculation - in reality, you'd track before/after
        changes = {}
        
        if match.winner:
            changes[match.winner] = 10.0 * self.config.ranking_update_factor
            
            for competitor in match.competitors:
                if competitor != match.winner:
                    changes[competitor] = -5.0 * self.config.ranking_update_factor
        
        return changes

    def get_competition_statistics(self) -> Dict[str, Any]:
        """Get comprehensive competition statistics."""
        total_matches = len(self.completed_matches)
        
        if total_matches == 0:
            return {
                'total_matches': 0, 
                'active_matches': len(self.active_matches),
                'rankings': dict(self.agent_rankings)
            }
        
        # Win rates
        win_counts = defaultdict(int)
        participation_counts = defaultdict(int)
        
        for match in self.completed_matches:
            for competitor in match.competitors:
                participation_counts[competitor] += 1
                if competitor == match.winner:
                    win_counts[competitor] += 1
        
        win_rates = {agent: wins / participation_counts[agent] 
                    for agent, wins in win_counts.items() 
                    if participation_counts[agent] > 0}
        
        # Top performers
        top_performers = sorted(self.agent_rankings.items(), 
                              key=lambda x: x[1], 
                              reverse=True)[:5]
        
        return {
            'total_matches': total_matches,
            'active_matches': len(self.active_matches),
            'win_rates': win_rates,
            'rankings': dict(self.agent_rankings),
            'top_performers': top_performers,
            'skill_profiles_count': len(self.skill_profiles),
            'average_match_duration': sum(m.end_time - m.start_time for m in self.completed_matches 
                                        if m.end_time) / total_matches if total_matches > 0 else 0.0
        }


class HybridLearningCoordinator:
    """Coordinates both cooperative and competitive learning modes."""
    
    def __init__(self, config: LearningConfiguration):
        self.config = config
        self.cooperative_engine = CooperativeLearningEngine(config)
        self.competitive_engine = CompetitiveLearningEngine(config)
        self.interaction_history: deque = deque(maxlen=config.interaction_history_length)
        
        logger.info("Initialized HybridLearningCoordinator")

    async def coordinate_learning_interaction(self,
                                            participants: List[str],
                                            context: Dict[str, Any],
                                            agent_capabilities: Dict[str, Dict[str, float]]) -> Dict[str, Any]:
        """Coordinate a learning interaction using optimal mode selection."""
        # Determine learning mode
        selected_mode = await self._select_learning_mode(participants, context, agent_capabilities)
        
        interaction = LearningInteraction(
            interaction_id=f"hybrid_{uuid.uuid4().hex[:8]}",
            participants=participants,
            learning_mode=selected_mode,
            outcome=InteractionOutcome.MUTUAL_BENEFIT,  # Will be updated
            performance_metrics={},
            knowledge_gained={},
            duration=0.0
        )
        
        start_time = time.time()
        
        try:
            if selected_mode == LearningMode.COOPERATIVE:
                results = await self._execute_cooperative_interaction(
                    participants, context, agent_capabilities, interaction
                )
            elif selected_mode == LearningMode.COMPETITIVE:
                results = await self._execute_competitive_interaction(
                    participants, context, agent_capabilities, interaction
                )
            else:
                results = await self._execute_hybrid_interaction(
                    participants, context, agent_capabilities, interaction
                )
            
            interaction.duration = time.time() - start_time
            interaction.performance_metrics = results.get('performance_metrics', {})
            interaction.knowledge_gained = results.get('knowledge_gained', {})
            interaction.outcome = self._determine_interaction_outcome(results)
            
            # Record interaction
            self.interaction_history.append(interaction)
            
            return {
                'interaction_id': interaction.interaction_id,
                'learning_mode': selected_mode.value,
                'outcome': interaction.outcome.value,
                'results': results,
                'duration': interaction.duration
            }
            
        except Exception as e:
            logger.error(f"Learning interaction failed: {e}")
            interaction.duration = time.time() - start_time
            interaction.outcome = InteractionOutcome.COLLABORATION_FAILURE
            self.interaction_history.append(interaction)
            
            return {
                'interaction_id': interaction.interaction_id,
                'learning_mode': selected_mode.value,
                'outcome': interaction.outcome.value,
                'error': str(e),
                'duration': interaction.duration
            }

    async def _select_learning_mode(self,
                                  participants: List[str],
                                  context: Dict[str, Any],
                                  agent_capabilities: Dict[str, Dict[str, float]]) -> LearningMode:
        """Select the optimal learning mode for the interaction."""
        # Analyze context for mode hints
        task_type = context.get('task_type', 'general')
        cooperation_indicators = ['collaborate', 'team', 'shared', 'together']
        competition_indicators = ['compete', 'versus', 'against', 'tournament']
        
        cooperation_score = sum(1 for indicator in cooperation_indicators 
                              if indicator in task_type.lower())
        competition_score = sum(1 for indicator in competition_indicators 
                              if indicator in task_type.lower())
        
        # Consider agent capabilities for complementarity vs similarity
        capability_similarity = self._calculate_capability_similarity(participants, agent_capabilities)
        
        # High similarity suggests competition, low similarity suggests cooperation
        if capability_similarity > 0.8:
            competition_score += 2
        elif capability_similarity < 0.4:
            cooperation_score += 2
        
        # Consider recent interaction history
        recent_modes = [interaction.learning_mode for interaction 
                       in list(self.interaction_history)[-10:]]
        
        cooperative_recent = recent_modes.count(LearningMode.COOPERATIVE)
        competitive_recent = recent_modes.count(LearningMode.COMPETITIVE)
        
        # Balance recent modes
        if cooperative_recent > competitive_recent + 2:
            competition_score += 1
        elif competitive_recent > cooperative_recent + 2:
            cooperation_score += 1
        
        # Apply configuration balance
        balance_factor = self.config.cooperation_competition_balance
        final_cooperation_score = cooperation_score + balance_factor * 3
        final_competition_score = competition_score + (1 - balance_factor) * 3
        
        # Mode switching for exploration
        if random.random() < self.config.mode_switching_probability:
            return random.choice([LearningMode.COOPERATIVE, LearningMode.COMPETITIVE])
        
        # Select based on scores
        if final_cooperation_score > final_competition_score:
            return LearningMode.COOPERATIVE
        elif final_competition_score > final_cooperation_score:
            return LearningMode.COMPETITIVE
        else:
            return LearningMode.COLLABORATIVE  # Hybrid approach

    def _calculate_capability_similarity(self,
                                       participants: List[str],
                                       agent_capabilities: Dict[str, Dict[str, float]]) -> float:
        """Calculate similarity of capabilities across participants."""
        if len(participants) < 2:
            return 0.0
        
        similarities = []
        
        for i in range(len(participants)):
            for j in range(i + 1, len(participants)):
                caps1 = agent_capabilities.get(participants[i], {})
                caps2 = agent_capabilities.get(participants[j], {})
                
                # Calculate cosine similarity
                similarity = self._cosine_similarity(caps1, caps2)
                similarities.append(similarity)
        
        return sum(similarities) / len(similarities) if similarities else 0.0

    def _cosine_similarity(self, caps1: Dict[str, float], caps2: Dict[str, float]) -> float:
        """Calculate cosine similarity between two capability vectors."""
        all_keys = set(caps1.keys()) | set(caps2.keys())
        
        if not all_keys:
            return 0.0
        
        vec1 = [caps1.get(key, 0.0) for key in all_keys]
        vec2 = [caps2.get(key, 0.0) for key in all_keys]
        
        dot_product = sum(a * b for a, b in zip(vec1, vec2))
        magnitude1 = math.sqrt(sum(a * a for a in vec1))
        magnitude2 = math.sqrt(sum(b * b for b in vec2))
        
        if magnitude1 == 0 or magnitude2 == 0:
            return 0.0
        
        return dot_product / (magnitude1 * magnitude2)

    async def _execute_cooperative_interaction(self,
                                             participants: List[str],
                                             context: Dict[str, Any],
                                             agent_capabilities: Dict[str, Dict[str, float]],
                                             interaction: LearningInteraction) -> Dict[str, Any]:
        """Execute a cooperative learning interaction."""
        objective = context.get('objective', 'cooperative_learning_task')
        
        session = await self.cooperative_engine.create_cooperation_session(
            participants, objective, context
        )
        
        # Simple task executor for demonstration
        async def cooperative_task_executor(agent_id, tasks, shared_knowledge):
            return {
                'success': True,
                'quality': random.uniform(0.6, 0.9),
                'efficiency': random.uniform(0.5, 0.8),
                'innovation': random.uniform(0.0, 0.3),
                'coordination_quality': random.uniform(0.7, 0.95),
                'new_insights': {f'insight_{len(tasks)}': f'learned from {len(tasks)} tasks'}
            }
        
        results = await self.cooperative_engine.execute_cooperative_learning(
            session, agent_capabilities, cooperative_task_executor
        )
        
        return {
            'cooperation_results': results,
            'performance_metrics': {
                'collective_performance': results['collective_performance'],
                'synergy_score': results.get('synergy_metrics', {}).get('overall_synergy', 0.0)
            },
            'knowledge_gained': {
                'shared_insights': results.get('knowledge_created', 0),
                'collaboration_skills': 0.1
            }
        }

    async def _execute_competitive_interaction(self,
                                             participants: List[str],
                                             context: Dict[str, Any],
                                             agent_capabilities: Dict[str, Dict[str, float]],
                                             interaction: LearningInteraction) -> Dict[str, Any]:
        """Execute a competitive learning interaction."""
        match_type = context.get('match_type', 'skill_competition')
        
        match = await self.competitive_engine.create_competitive_match(
            participants, match_type, context
        )
        
        # Generate agent strategies based on capabilities
        agent_strategies = {}
        for participant in participants:
            caps = agent_capabilities.get(participant, {})
            strategies = {
                'aggression_level': caps.get('competitive', 0.5),
                'risk_tolerance': caps.get('exploration', 0.5),
                'adaptation_speed': caps.get('learning', 0.5)
            }
            agent_strategies[participant] = strategies
        
        # Simple competition executor for demonstration
        async def competition_executor(competitors, strategies, match_type):
            scores = {}
            for competitor in competitors:
                strategy = strategies.get(competitor, {})
                base_score = sum(strategy.values()) / len(strategy) if strategy else 0.5
                noise = random.gauss(0, 0.1)
                scores[competitor] = max(0, base_score + noise)
            
            return {
                'scores': scores,
                'performance_metrics': {
                    comp: {'reaction_time': random.uniform(0.1, 0.5), 
                          'accuracy': random.uniform(0.6, 0.95)}
                    for comp in competitors
                },
                'competition_type': match_type
            }
        
        results = await self.competitive_engine.execute_competitive_learning(
            match, agent_strategies, competition_executor
        )
        
        return {
            'competition_results': results,
            'performance_metrics': {
                'winner_score': max(results['final_scores'].values()),
                'score_spread': max(results['final_scores'].values()) - min(results['final_scores'].values())
            },
            'knowledge_gained': {
                'competitive_skills': sum(results['learning_improvements'].values()) / len(results['learning_improvements']),
                'strategic_knowledge': results['strategies_learned']
            }
        }

    async def _execute_hybrid_interaction(self,
                                        participants: List[str],
                                        context: Dict[str, Any],
                                        agent_capabilities: Dict[str, Dict[str, float]],
                                        interaction: LearningInteraction) -> Dict[str, Any]:
        """Execute a hybrid learning interaction combining cooperation and competition."""
        # Split into cooperative and competitive phases
        cooperative_results = await self._execute_cooperative_interaction(
            participants, context, agent_capabilities, interaction
        )
        
        competitive_results = await self._execute_competitive_interaction(
            participants, context, agent_capabilities, interaction
        )
        
        # Combine results
        combined_performance = (
            cooperative_results['performance_metrics'].get('collective_performance', 0.0) * 0.6 +
            competitive_results['performance_metrics'].get('winner_score', 0.0) * 0.4
        )
        
        combined_knowledge = {}
        combined_knowledge.update(cooperative_results.get('knowledge_gained', {}))
        for key, value in competitive_results.get('knowledge_gained', {}).items():
            if key in combined_knowledge:
                combined_knowledge[key] = (combined_knowledge[key] + value) / 2
            else:
                combined_knowledge[key] = value
        
        return {
            'hybrid_results': {
                'cooperative_phase': cooperative_results,
                'competitive_phase': competitive_results
            },
            'performance_metrics': {
                'combined_performance': combined_performance,
                'cooperation_score': cooperative_results['performance_metrics'].get('collective_performance', 0.0),
                'competition_score': competitive_results['performance_metrics'].get('winner_score', 0.0)
            },
            'knowledge_gained': combined_knowledge
        }

    def _determine_interaction_outcome(self, results: Dict[str, Any]) -> InteractionOutcome:
        """Determine the outcome of the interaction based on results."""
        if 'error' in results:
            return InteractionOutcome.COLLABORATION_FAILURE
        
        performance_metrics = results.get('performance_metrics', {})
        
        if 'winner_score' in performance_metrics:
            # Competitive interaction
            if performance_metrics['winner_score'] > 0.7:
                return InteractionOutcome.WIN
            else:
                return InteractionOutcome.DRAW
        
        elif 'collective_performance' in performance_metrics:
            # Cooperative interaction
            if performance_metrics['collective_performance'] > 0.8:
                return InteractionOutcome.COLLABORATION_SUCCESS
            elif performance_metrics['collective_performance'] > 0.6:
                return InteractionOutcome.MUTUAL_BENEFIT
            else:
                return InteractionOutcome.COLLABORATION_FAILURE
        
        else:
            return InteractionOutcome.MUTUAL_BENEFIT

    def get_hybrid_statistics(self) -> Dict[str, Any]:
        """Get comprehensive statistics for hybrid learning system."""
        coop_stats = self.cooperative_engine.get_cooperation_statistics()
        comp_stats = self.competitive_engine.get_competition_statistics()
        
        # Interaction history analysis
        if self.interaction_history:
            mode_counts = defaultdict(int)
            outcome_counts = defaultdict(int)
            
            for interaction in self.interaction_history:
                mode_counts[interaction.learning_mode.value] += 1
                outcome_counts[interaction.outcome.value] += 1
            
            avg_duration = sum(i.duration for i in self.interaction_history) / len(self.interaction_history)
        else:
            mode_counts = {}
            outcome_counts = {}
            avg_duration = 0.0
        
        return {
            'cooperative_statistics': coop_stats,
            'competitive_statistics': comp_stats,
            'interaction_history': {
                'total_interactions': len(self.interaction_history),
                'mode_distribution': dict(mode_counts),
                'outcome_distribution': dict(outcome_counts),
                'average_duration': avg_duration
            },
            'system_configuration': {
                'cooperation_competition_balance': self.config.cooperation_competition_balance,
                'mode_switching_probability': self.config.mode_switching_probability
            }
        }