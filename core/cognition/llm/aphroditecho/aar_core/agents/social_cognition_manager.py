"""
Social Cognition Manager

Manages multi-agent shared cognition, enabling agents to share cognitive resources,
knowledge, and collaborate on complex problem-solving tasks.

This component implements the social cognition extensions required for Task 2.3.2
of the Deep Tree Echo development roadmap.
"""

import logging
import time
import uuid
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class SharedCognitionType(Enum):
    """Types of shared cognition mechanisms."""
    WORKING_MEMORY = "working_memory"
    KNOWLEDGE_BASE = "knowledge_base"
    PROCESSING_POOL = "processing_pool"
    ATTENTION_FOCUS = "attention_focus"
    DECISION_CONTEXT = "decision_context"


class CognitionSharingMode(Enum):
    """Modes of sharing cognitive resources."""
    BROADCAST = "broadcast"  # Share with all connected agents
    TARGETED = "targeted"    # Share with specific agents
    POOLED = "pooled"       # Pool resources for collective access
    HIERARCHICAL = "hierarchical"  # Share based on hierarchy


@dataclass
class SharedCognitionResource:
    """Represents a shared cognitive resource."""
    resource_id: str
    cognition_type: SharedCognitionType
    owner_agent_id: str
    data: Dict[str, Any]
    access_permissions: Dict[str, str] = field(default_factory=dict)  # agent_id -> permission_level
    sharing_mode: CognitionSharingMode = CognitionSharingMode.BROADCAST
    created_at: float = field(default_factory=time.time)
    last_updated: float = field(default_factory=time.time)
    access_count: int = 0
    collaborative_updates: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class CognitionSharingMetrics:
    """Metrics for shared cognition performance."""
    resources_shared: int = 0
    resources_accessed: int = 0
    collaboration_events: int = 0
    consensus_decisions: int = 0
    knowledge_transfers: int = 0
    collective_problem_solves: int = 0
    avg_sharing_latency: float = 0.0
    collaboration_effectiveness: float = 0.0


class SocialCognitionManager:
    """Manages multi-agent shared cognition and collaborative problem solving."""
    
    def __init__(self, max_shared_resources: int = 1000):
        self.max_shared_resources = max_shared_resources
        
        # Shared cognitive resources
        self.shared_resources: Dict[str, SharedCognitionResource] = {}
        self.agent_cognitive_profiles: Dict[str, Dict[str, Any]] = {}
        
        # Collaboration tracking
        self.active_collaborations: Dict[str, Dict[str, Any]] = {}
        self.collaboration_history: List[Dict[str, Any]] = []
        
        # Performance metrics
        self.metrics = CognitionSharingMetrics()
        
        # Configuration
        self.sharing_policies = {
            'max_resource_lifetime': 3600.0,  # 1 hour
            'max_collaborative_updates': 100,
            'trust_threshold': 0.3,
            'consensus_threshold': 0.7
        }
        
        logger.info(f"Social Cognition Manager initialized with capacity: {max_shared_resources}")
    
    async def register_agent(self, agent_id: str, cognitive_profile: Dict[str, Any]) -> None:
        """Register an agent with their cognitive profile for social cognition."""
        profile = {
            'agent_id': agent_id,
            'cognitive_capabilities': cognitive_profile.get('capabilities', {}),
            'sharing_preferences': cognitive_profile.get('sharing_preferences', {}),
            'collaboration_history': [],
            'trust_scores': {},
            'specializations': cognitive_profile.get('specializations', []),
            'working_memory_capacity': cognitive_profile.get('memory_capacity', 1000),
            'processing_bandwidth': cognitive_profile.get('processing_bandwidth', 1.0),
            'registered_at': time.time()
        }
        
        self.agent_cognitive_profiles[agent_id] = profile
        logger.debug(f"Registered agent {agent_id} for social cognition")
    
    async def share_cognition(self, 
                            agent_id: str, 
                            cognition_type: SharedCognitionType,
                            data: Dict[str, Any],
                            sharing_mode: CognitionSharingMode = CognitionSharingMode.BROADCAST,
                            target_agents: Optional[List[str]] = None) -> str:
        """Share cognitive resources with other agents."""
        
        # Check capacity
        if len(self.shared_resources) >= self.max_shared_resources:
            await self._cleanup_expired_resources()
            
            if len(self.shared_resources) >= self.max_shared_resources:
                raise RuntimeError("Maximum shared cognitive resources reached")
        
        # Create shared resource
        resource_id = f"cogres_{uuid.uuid4().hex[:8]}"
        
        # Set access permissions based on sharing mode
        access_permissions = {}
        if sharing_mode == CognitionSharingMode.TARGETED and target_agents:
            for target_id in target_agents:
                access_permissions[target_id] = "read_write"
        elif sharing_mode == CognitionSharingMode.BROADCAST:
            # Default broadcast permissions - all agents can read
            access_permissions['*'] = "read"
        elif sharing_mode == CognitionSharingMode.POOLED:
            access_permissions['*'] = "read_write"
        
        shared_resource = SharedCognitionResource(
            resource_id=resource_id,
            cognition_type=cognition_type,
            owner_agent_id=agent_id,
            data=data.copy(),
            access_permissions=access_permissions,
            sharing_mode=sharing_mode
        )
        
        self.shared_resources[resource_id] = shared_resource
        self.metrics.resources_shared += 1
        
        logger.info(f"Agent {agent_id} shared {cognition_type.value} resource {resource_id} in {sharing_mode.value} mode")
        return resource_id
    
    async def access_shared_cognition(self, 
                                    agent_id: str, 
                                    resource_id: str,
                                    access_mode: str = "read") -> Optional[Dict[str, Any]]:
        """Access shared cognitive resource."""
        if resource_id not in self.shared_resources:
            logger.warning(f"Shared resource {resource_id} not found")
            return None
        
        resource = self.shared_resources[resource_id]
        
        # Check permissions
        if not self._check_access_permission(agent_id, resource, access_mode):
            logger.warning(f"Agent {agent_id} denied {access_mode} access to resource {resource_id}")
            return None
        
        # Update access tracking
        resource.access_count += 1
        resource.last_updated = time.time()
        self.metrics.resources_accessed += 1
        
        # Return copy of resource data
        access_data = {
            'resource_id': resource_id,
            'cognition_type': resource.cognition_type.value,
            'owner_agent_id': resource.owner_agent_id,
            'data': resource.data.copy(),
            'sharing_mode': resource.sharing_mode.value,
            'access_timestamp': time.time()
        }
        
        logger.debug(f"Agent {agent_id} accessed shared resource {resource_id}")
        return access_data
    
    async def update_shared_cognition(self,
                                    agent_id: str,
                                    resource_id: str,
                                    updates: Dict[str, Any]) -> bool:
        """Collaboratively update shared cognitive resource."""
        if resource_id not in self.shared_resources:
            return False
        
        resource = self.shared_resources[resource_id]
        
        # Check write permissions
        if not self._check_access_permission(agent_id, resource, "write"):
            logger.warning(f"Agent {agent_id} denied write access to resource {resource_id}")
            return False
        
        # Record collaborative update
        update_record = {
            'agent_id': agent_id,
            'timestamp': time.time(),
            'updates': updates.copy(),
            'update_id': uuid.uuid4().hex[:8]
        }
        
        # Apply updates to resource data
        resource.data.update(updates)
        resource.collaborative_updates.append(update_record)
        resource.last_updated = time.time()
        
        # Limit update history
        if len(resource.collaborative_updates) > self.sharing_policies['max_collaborative_updates']:
            resource.collaborative_updates = resource.collaborative_updates[-self.sharing_policies['max_collaborative_updates']:]
        
        self.metrics.collaboration_events += 1
        
        logger.debug(f"Agent {agent_id} updated shared resource {resource_id}")
        return True
    
    async def initiate_collaborative_problem_solving(self,
                                                   initiator_agent_id: str,
                                                   problem_definition: Dict[str, Any],
                                                   required_agents: Optional[List[str]] = None) -> str:
        """Initiate collaborative problem-solving session."""
        
        collaboration_id = f"collab_{uuid.uuid4().hex[:8]}"
        
        # Determine participating agents
        if required_agents:
            participants = required_agents.copy()
        else:
            # Auto-select agents based on problem requirements and agent capabilities
            participants = await self._select_collaborative_agents(problem_definition)
        
        # Ensure initiator is included
        if initiator_agent_id not in participants:
            participants.append(initiator_agent_id)
        
        collaboration_session = {
            'collaboration_id': collaboration_id,
            'initiator': initiator_agent_id,
            'participants': participants,
            'problem_definition': problem_definition,
            'status': 'active',
            'created_at': time.time(),
            'phases': [],
            'shared_resources': [],
            'solutions': [],
            'consensus_level': 0.0,
            'collaboration_metrics': {
                'messages_exchanged': 0,
                'resources_shared': 0,
                'decisions_made': 0,
                'consensus_attempts': 0
            }
        }
        
        self.active_collaborations[collaboration_id] = collaboration_session
        
        # Create shared working space for collaboration
        workspace_id = await self.share_cognition(
            initiator_agent_id,
            SharedCognitionType.WORKING_MEMORY,
            {
                'collaboration_id': collaboration_id,
                'problem_space': problem_definition,
                'solutions_workspace': {},
                'decision_history': [],
                'participant_contributions': {}
            },
            CognitionSharingMode.POOLED,
            participants
        )
        
        collaboration_session['shared_resources'].append(workspace_id)
        
        logger.info(f"Initiated collaborative problem solving {collaboration_id} with {len(participants)} agents")
        return collaboration_id
    
    async def contribute_to_collaboration(self,
                                        agent_id: str,
                                        collaboration_id: str,
                                        contribution: Dict[str, Any]) -> bool:
        """Contribute to an active collaborative problem-solving session."""
        
        if collaboration_id not in self.active_collaborations:
            logger.warning(f"Collaboration {collaboration_id} not found")
            return False
        
        collaboration = self.active_collaborations[collaboration_id]
        
        # Check if agent is a participant
        if agent_id not in collaboration['participants']:
            logger.warning(f"Agent {agent_id} not a participant in collaboration {collaboration_id}")
            return False
        
        # Process contribution
        contribution_record = {
            'agent_id': agent_id,
            'timestamp': time.time(),
            'contribution_type': contribution.get('type', 'general'),
            'content': contribution.get('content', {}),
            'confidence': contribution.get('confidence', 0.5),
            'contribution_id': uuid.uuid4().hex[:8]
        }
        
        # Add to collaboration history
        if 'contributions' not in collaboration:
            collaboration['contributions'] = []
        collaboration['contributions'].append(contribution_record)
        
        # Update shared workspace if available
        if collaboration['shared_resources']:
            workspace_id = collaboration['shared_resources'][0]  # Primary workspace
            workspace_data = await self.access_shared_cognition(agent_id, workspace_id, "read")
            
            if workspace_data:
                # Update workspace with contribution
                workspace_updates = {
                    f'participant_contributions.{agent_id}': contribution_record,
                    f'solutions_workspace.{contribution_record["contribution_id"]}': contribution.get('content', {})
                }
                await self.update_shared_cognition(agent_id, workspace_id, workspace_updates)
        
        # Update collaboration metrics
        collaboration['collaboration_metrics']['messages_exchanged'] += 1
        
        logger.debug(f"Agent {agent_id} contributed to collaboration {collaboration_id}")
        return True
    
    async def build_consensus(self,
                            collaboration_id: str,
                            decision_options: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Build consensus among participating agents for decision-making."""
        
        if collaboration_id not in self.active_collaborations:
            return {'error': 'Collaboration not found'}
        
        collaboration = self.active_collaborations[collaboration_id]
        participants = collaboration['participants']
        
        # Collect preferences from each agent
        agent_preferences = {}
        for agent_id in participants:
            # Get agent's cognitive profile for decision-making preferences
            profile = self.agent_cognitive_profiles.get(agent_id, {})
            
            # Simulate preference calculation based on agent capabilities and history
            preferences = await self._calculate_agent_preferences(agent_id, decision_options, profile)
            agent_preferences[agent_id] = preferences
        
        # Calculate weighted consensus
        consensus_result = await self._calculate_weighted_consensus(agent_preferences, decision_options)
        
        # Update collaboration with consensus result
        collaboration['collaboration_metrics']['consensus_attempts'] += 1
        collaboration['collaboration_metrics']['decisions_made'] += 1
        collaboration['consensus_level'] = consensus_result['consensus_strength']
        
        # Record decision in collaboration history
        decision_record = {
            'decision_id': uuid.uuid4().hex[:8],
            'timestamp': time.time(),
            'options_considered': len(decision_options),
            'agent_preferences': agent_preferences,
            'consensus_result': consensus_result,
            'participants': participants
        }
        
        if 'decisions' not in collaboration:
            collaboration['decisions'] = []
        collaboration['decisions'].append(decision_record)
        
        self.metrics.consensus_decisions += 1
        
        logger.info(f"Built consensus for collaboration {collaboration_id}: {consensus_result['selected_option']}")
        return consensus_result
    
    async def _select_collaborative_agents(self, problem_definition: Dict[str, Any]) -> List[str]:
        """Select agents for collaborative problem solving based on capabilities."""
        required_capabilities = problem_definition.get('required_capabilities', [])
        problem_complexity = problem_definition.get('complexity', 'medium')
        
        # Determine optimal number of agents based on complexity
        agent_count = {'low': 2, 'medium': 3, 'high': 5, 'very_high': 7}.get(problem_complexity, 3)
        
        # Score agents based on capability match
        agent_scores = []
        
        for agent_id, profile in self.agent_cognitive_profiles.items():
            score = 0.0
            
            # Score based on capability match
            agent_capabilities = profile.get('cognitive_capabilities', {})
            for req_cap in required_capabilities:
                if agent_capabilities.get(req_cap, False):
                    score += 1.0
            
            # Score based on specializations
            specializations = profile.get('specializations', [])
            problem_domain = problem_definition.get('domain', '')
            if problem_domain in specializations:
                score += 0.5
            
            # Score based on collaboration history (prefer agents with good track record)
            collaboration_history = profile.get('collaboration_history', [])
            if collaboration_history:
                avg_success = sum(h.get('success_rate', 0.5) for h in collaboration_history) / len(collaboration_history)
                score += avg_success * 0.3
            
            agent_scores.append((agent_id, score))
        
        # Select top-scoring agents
        agent_scores.sort(key=lambda x: x[1], reverse=True)
        selected_agents = [agent_id for agent_id, _ in agent_scores[:agent_count]]
        
        return selected_agents
    
    async def _calculate_agent_preferences(self, 
                                         agent_id: str, 
                                         options: List[Dict[str, Any]], 
                                         profile: Dict[str, Any]) -> Dict[str, float]:
        """Calculate agent preferences for decision options."""
        preferences = {}
        
        # Get agent capabilities and specializations
        capabilities = profile.get('cognitive_capabilities', {})
        specializations = profile.get('specializations', [])
        
        for i, option in enumerate(options):
            option_id = option.get('id', f'option_{i}')
            
            # Base preference
            preference_score = 0.5
            
            # Adjust based on agent capabilities
            required_caps = option.get('required_capabilities', [])
            capability_match = sum(1 for cap in required_caps if capabilities.get(cap, False))
            capability_ratio = capability_match / max(len(required_caps), 1)
            preference_score += capability_ratio * 0.3
            
            # Adjust based on specializations
            option_domain = option.get('domain', '')
            if option_domain in specializations:
                preference_score += 0.2
            
            # Adjust based on estimated effort vs agent processing capacity
            estimated_effort = option.get('estimated_effort', 1.0)
            agent_bandwidth = profile.get('processing_bandwidth', 1.0)
            if estimated_effort <= agent_bandwidth:
                preference_score += 0.1
            else:
                preference_score -= 0.1
            
            preferences[option_id] = min(1.0, max(0.0, preference_score))
        
        return preferences
    
    async def _calculate_weighted_consensus(self, 
                                          agent_preferences: Dict[str, Dict[str, float]], 
                                          options: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate weighted consensus from agent preferences."""
        
        if not agent_preferences or not options:
            return {'error': 'No preferences or options provided'}
        
        # Calculate weighted scores for each option
        option_scores = {}
        len(agent_preferences)
        
        for i, option in enumerate(options):
            option_id = option.get('id', f'option_{i}')
            
            # Sum preferences across all agents
            total_preference = 0.0
            agent_count = 0
            
            for agent_id, preferences in agent_preferences.items():
                if option_id in preferences:
                    # Weight by agent trust score (if available)
                    agent_profile = self.agent_cognitive_profiles.get(agent_id, {})
                    trust_scores = agent_profile.get('trust_scores', {})
                    avg_trust = sum(trust_scores.values()) / max(len(trust_scores), 1) if trust_scores else 0.5
                    
                    weighted_preference = preferences[option_id] * (0.5 + avg_trust * 0.5)
                    total_preference += weighted_preference
                    agent_count += 1
            
            if agent_count > 0:
                option_scores[option_id] = total_preference / agent_count
            else:
                option_scores[option_id] = 0.0
        
        # Find highest scoring option
        if not option_scores:
            return {'error': 'No valid option scores calculated'}
        
        best_option_id = max(option_scores, key=option_scores.get)
        best_score = option_scores[best_option_id]
        
        # Calculate consensus strength (how much agents agree)
        preference_variance = []
        for agent_prefs in agent_preferences.values():
            if best_option_id in agent_prefs:
                preference_variance.append(agent_prefs[best_option_id])
        
        if preference_variance:
            consensus_strength = 1.0 - (max(preference_variance) - min(preference_variance))
            consensus_strength = max(0.0, min(1.0, consensus_strength))
        else:
            consensus_strength = 0.0
        
        return {
            'selected_option': best_option_id,
            'consensus_score': best_score,
            'consensus_strength': consensus_strength,
            'option_scores': option_scores,
            'participating_agents': list(agent_preferences.keys()),
            'meets_threshold': consensus_strength >= self.sharing_policies['consensus_threshold']
        }
    
    def _check_access_permission(self, 
                               agent_id: str, 
                               resource: SharedCognitionResource, 
                               access_mode: str) -> bool:
        """Check if agent has permission to access resource."""
        
        # Owner always has full access
        if agent_id == resource.owner_agent_id:
            return True
        
        # Check specific permissions
        if agent_id in resource.access_permissions:
            permission_level = resource.access_permissions[agent_id]
            if access_mode == "read" and permission_level in ["read", "read_write"]:
                return True
            elif access_mode == "write" and permission_level == "read_write":
                return True
        
        # Check wildcard permissions
        if '*' in resource.access_permissions:
            permission_level = resource.access_permissions['*']
            if access_mode == "read" and permission_level in ["read", "read_write"]:
                return True
            elif access_mode == "write" and permission_level == "read_write":
                return True
        
        # Check trust-based access for broadcast mode
        if resource.sharing_mode == CognitionSharingMode.BROADCAST:
            owner_profile = self.agent_cognitive_profiles.get(resource.owner_agent_id, {})
            trust_scores = owner_profile.get('trust_scores', {})
            agent_trust = trust_scores.get(agent_id, 0.3)  # Default moderate trust
            
            if agent_trust >= self.sharing_policies['trust_threshold']:
                return access_mode == "read"  # Trust allows reading
        
        return False
    
    async def _cleanup_expired_resources(self) -> None:
        """Clean up expired shared cognitive resources."""
        current_time = time.time()
        max_lifetime = self.sharing_policies['max_resource_lifetime']
        
        expired_resources = []
        for resource_id, resource in self.shared_resources.items():
            if current_time - resource.created_at > max_lifetime:
                expired_resources.append(resource_id)
        
        for resource_id in expired_resources:
            del self.shared_resources[resource_id]
            logger.debug(f"Cleaned up expired resource {resource_id}")
    
    async def finalize_collaboration(self, collaboration_id: str) -> Dict[str, Any]:
        """Finalize collaborative problem-solving session."""
        
        if collaboration_id not in self.active_collaborations:
            return {'error': 'Collaboration not found'}
        
        collaboration = self.active_collaborations[collaboration_id]
        
        # Calculate final metrics
        final_metrics = collaboration['collaboration_metrics'].copy()
        final_metrics['duration'] = time.time() - collaboration['created_at']
        final_metrics['participants_count'] = len(collaboration['participants'])
        final_metrics['final_consensus_level'] = collaboration.get('consensus_level', 0.0)
        
        # Archive collaboration
        archived_collaboration = collaboration.copy()
        archived_collaboration['status'] = 'completed'
        archived_collaboration['finalized_at'] = time.time()
        archived_collaboration['final_metrics'] = final_metrics
        
        self.collaboration_history.append(archived_collaboration)
        
        # Update agent collaboration histories
        for agent_id in collaboration['participants']:
            if agent_id in self.agent_cognitive_profiles:
                profile = self.agent_cognitive_profiles[agent_id]
                if 'collaboration_history' not in profile:
                    profile['collaboration_history'] = []
                
                collab_summary = {
                    'collaboration_id': collaboration_id,
                    'role': 'initiator' if agent_id == collaboration['initiator'] else 'participant',
                    'success_rate': final_metrics['final_consensus_level'],
                    'duration': final_metrics['duration'],
                    'contributions': len([c for c in collaboration.get('contributions', []) if c['agent_id'] == agent_id])
                }
                profile['collaboration_history'].append(collab_summary)
        
        # Clean up shared resources
        for resource_id in collaboration.get('shared_resources', []):
            if resource_id in self.shared_resources:
                del self.shared_resources[resource_id]
        
        # Remove from active collaborations
        del self.active_collaborations[collaboration_id]
        
        self.metrics.collective_problem_solves += 1
        
        logger.info(f"Finalized collaboration {collaboration_id} with {final_metrics['participants_count']} agents")
        return {
            'collaboration_id': collaboration_id,
            'final_metrics': final_metrics,
            'status': 'completed',
            'success': final_metrics['final_consensus_level'] >= self.sharing_policies['consensus_threshold']
        }
    
    def get_social_cognition_stats(self) -> Dict[str, Any]:
        """Get comprehensive social cognition statistics."""
        
        # Calculate resource distribution by type
        resource_distribution = {}
        for resource in self.shared_resources.values():
            cognition_type = resource.cognition_type.value
            resource_distribution[cognition_type] = resource_distribution.get(cognition_type, 0) + 1
        
        # Calculate collaboration effectiveness
        if self.collaboration_history:
            total_effectiveness = sum(
                collab.get('final_metrics', {}).get('final_consensus_level', 0.0) 
                for collab in self.collaboration_history
            )
            avg_effectiveness = total_effectiveness / len(self.collaboration_history)
        else:
            avg_effectiveness = 0.0
        
        self.metrics.collaboration_effectiveness = avg_effectiveness
        
        return {
            'metrics': {
                'resources_shared': self.metrics.resources_shared,
                'resources_accessed': self.metrics.resources_accessed,
                'collaboration_events': self.metrics.collaboration_events,
                'consensus_decisions': self.metrics.consensus_decisions,
                'knowledge_transfers': self.metrics.knowledge_transfers,
                'collective_problem_solves': self.metrics.collective_problem_solves,
                'collaboration_effectiveness': self.metrics.collaboration_effectiveness
            },
            'current_state': {
                'active_shared_resources': len(self.shared_resources),
                'registered_agents': len(self.agent_cognitive_profiles),
                'active_collaborations': len(self.active_collaborations),
                'completed_collaborations': len(self.collaboration_history)
            },
            'resource_distribution': resource_distribution,
            'system_capacity': {
                'max_shared_resources': self.max_shared_resources,
                'resource_utilization': len(self.shared_resources) / self.max_shared_resources
            }
        }
    
    async def shutdown(self) -> None:
        """Gracefully shutdown social cognition manager."""
        logger.info("Shutting down Social Cognition Manager...")
        
        # Finalize any active collaborations
        active_collab_ids = list(self.active_collaborations.keys())
        for collaboration_id in active_collab_ids:
            await self.finalize_collaboration(collaboration_id)
        
        # Clear resources
        self.shared_resources.clear()
        self.agent_cognitive_profiles.clear()
        self.collaboration_history.clear()
        
        logger.info("Social Cognition Manager shutdown complete")