"""
Cognitive Network Tool for Agent Zero Networks

This module implements a comprehensive cognitive network tool that provides
9 core methods for cognitive grammar operations, network coordination,
multi-agent task coordination, and broadcast messaging capabilities.

This tool seamlessly integrates with existing Agent Zero infrastructure
while providing enhanced cognitive communication capabilities.
"""

import json
import logging
import time
import asyncio
from typing import Dict, List, Optional, Any, Union, Tuple
from dataclasses import dataclass, asdict
from collections import defaultdict
import uuid

from helpers.cognitive_grammar import (
    CognitiveGrammarFramework, 
    CognitiveMessage, 
    CommunicativeIntent,
    CognitiveFrame,
    CognitiveRole
)
from helpers.distributed_network import (
    DistributedNetworkRegistry,
    NetworkAgent,
    AgentCapability,
    NetworkTopology,
    AgentStatus,
    CommunicationProtocol
)

logger = logging.getLogger(__name__)


@dataclass
class TaskCoordinationRequest:
    """Request for task coordination between agents."""
    task_id: str
    task_description: str
    required_capabilities: List[str]
    max_partners: int
    coordination_type: str
    deadline: Optional[str] = None
    priority: str = "normal"
    resource_requirements: Optional[Dict[str, Any]] = None


@dataclass
class CoordinationResponse:
    """Response from task coordination."""
    success: bool
    assigned_agents: List[str]
    coordination_plan: Dict[str, Any]
    estimated_completion: Optional[str] = None
    error_message: Optional[str] = None


@dataclass
class BroadcastMessage:
    """Message for network-wide broadcasting."""
    message_id: str
    sender_id: str
    content: Dict[str, Any]
    target_capabilities: Optional[List[str]] = None
    priority: str = "normal"
    ttl: int = 3  # Time to live (hops)


class CognitiveNetworkTool:
    """
    Main tool class providing 9 core methods for cognitive network operations.
    
    Core Methods:
    1. send_cognitive_message
    2. coordinate_with_agents
    3. discover_network_agents
    4. broadcast_to_network
    5. query_agent_capabilities
    6. update_agent_status
    7. negotiate_resources
    8. monitor_network_health
    9. reconfigure_network_topology
    """

    def __init__(self, 
                 agent_id: str,
                 network_registry: Optional[DistributedNetworkRegistry] = None,
                 grammar_framework: Optional[CognitiveGrammarFramework] = None):
        self.agent_id = agent_id
        self.network_registry = network_registry or DistributedNetworkRegistry(agent_id)
        self.grammar_framework = grammar_framework or CognitiveGrammarFramework()
        
        # Internal state
        self.active_coordinations: Dict[str, TaskCoordinationRequest] = {}
        self.message_history: List[CognitiveMessage] = []
        self.broadcast_cache: Dict[str, BroadcastMessage] = {}
        
        logger.info(f"Initialized cognitive network tool for agent {agent_id}")

    # Core Method 1: Send Cognitive Message
    def send_cognitive_message(self, 
                             target_agent_id: str,
                             intent: Union[str, CommunicativeIntent],
                             frame: Union[str, CognitiveFrame],
                             content: Dict[str, Any],
                             roles: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Send a structured cognitive message to another agent.
        
        Args:
            target_agent_id: ID of the target agent
            intent: Communicative intent (request, inform, coordinate, etc.)
            frame: Cognitive frame (task_delegation, information_sharing, etc.)
            content: Message content
            roles: Cognitive roles mapping
            
        Returns:
            Dict containing success status and message details
        """
        try:
            # Convert string parameters to enums if needed
            if isinstance(intent, str):
                intent = CommunicativeIntent(intent)
            if isinstance(frame, str):
                frame = CognitiveFrame(frame)
                
            # Convert roles to CognitiveRole enums
            cognitive_roles = {}
            if roles:
                for role_key, value in roles.items():
                    if isinstance(role_key, str):
                        try:
                            cognitive_role = CognitiveRole(role_key)
                            cognitive_roles[cognitive_role] = value
                        except ValueError:
                            logger.warning(f"Unknown cognitive role: {role_key}")
                    else:
                        cognitive_roles[role_key] = value
            
            # Create cognitive message
            message = self.grammar_framework.create_message(
                intent=intent,
                frame=frame,
                agent_id=self.agent_id,
                content=content,
                roles=cognitive_roles
            )
            
            # Store in history
            self.message_history.append(message)
            
            # Simulate message sending (in a real implementation, this would use network protocols)
            natural_language = message.to_natural_language()
            
            result = {
                "success": True,
                "message_id": message.message_id,
                "target_agent": target_agent_id,
                "natural_language": natural_language,
                "sent_at": message.timestamp,
                "message_json": message.to_json()
            }
            
            logger.info(f"Sent cognitive message to {target_agent_id}: {natural_language}")
            return result
            
        except Exception as e:
            logger.error(f"Error sending cognitive message: {e}")
            return {
                "success": False,
                "error": str(e),
                "target_agent": target_agent_id
            }

    # Core Method 2: Coordinate with Agents
    def coordinate_with_agents(self,
                             task_description: str,
                             required_capabilities: List[str],
                             max_partners: int = 3,
                             coordination_type: str = "synchronous",
                             deadline: Optional[str] = None,
                             priority: str = "normal") -> Dict[str, Any]:
        """
        Coordinate a task with other agents based on capability matching.
        
        Args:
            task_description: Description of the task to coordinate
            required_capabilities: List of required capabilities
            max_partners: Maximum number of partner agents
            coordination_type: Type of coordination (synchronous/asynchronous)
            deadline: Task deadline
            priority: Task priority
            
        Returns:
            Dict containing coordination results
        """
        try:
            task_id = str(uuid.uuid4())
            
            # Create coordination request
            coord_request = TaskCoordinationRequest(
                task_id=task_id,
                task_description=task_description,
                required_capabilities=required_capabilities,
                max_partners=max_partners,
                coordination_type=coordination_type,
                deadline=deadline,
                priority=priority
            )
            
            # Discover suitable agents
            suitable_agents = self.network_registry.discover_agents(
                capabilities=required_capabilities,
                max_agents=max_partners
            )
            
            if not suitable_agents:
                return {
                    "success": False,
                    "task_id": task_id,
                    "error": "No suitable agents found",
                    "required_capabilities": required_capabilities
                }
            
            # Create coordination messages for each agent
            coordination_messages = []
            assigned_agents = []
            
            for agent in suitable_agents:
                # Create coordination message
                coord_message = self.grammar_framework.create_coordination_message(
                    agent_id=self.agent_id,
                    task=task_description,
                    participants=[agent.agent_id],
                    coordination_type=coordination_type
                )
                
                # Send coordination request
                message_result = self.send_cognitive_message(
                    target_agent_id=agent.agent_id,
                    intent=CommunicativeIntent.COORDINATE,
                    frame=CognitiveFrame.COORDINATION,
                    content={
                        "task_id": task_id,
                        "task_description": task_description,
                        "coordination_type": coordination_type,
                        "deadline": deadline,
                        "priority": priority
                    },
                    roles={
                        "agent": self.agent_id,
                        "patient": agent.agent_id
                    }
                )
                
                if message_result["success"]:
                    coordination_messages.append(message_result)
                    assigned_agents.append(agent.agent_id)
            
            # Store active coordination
            self.active_coordinations[task_id] = coord_request
            
            # Create coordination plan
            coordination_plan = {
                "task_id": task_id,
                "coordinator": self.agent_id,
                "participants": assigned_agents,
                "task_description": task_description,
                "coordination_type": coordination_type,
                "created_at": time.time(),
                "status": "active"
            }
            
            result = {
                "success": True,
                "task_id": task_id,
                "assigned_agents": assigned_agents,
                "coordination_plan": coordination_plan,
                "messages_sent": len(coordination_messages),
                "coordination_messages": coordination_messages
            }
            
            logger.info(f"Coordinated task {task_id} with {len(assigned_agents)} agents")
            return result
            
        except Exception as e:
            logger.error(f"Error coordinating with agents: {e}")
            return {
                "success": False,
                "error": str(e),
                "task_description": task_description
            }

    # Core Method 3: Discover Network Agents
    def discover_network_agents(self,
                               capabilities: Optional[List[str]] = None,
                               max_agents: int = 10,
                               include_inactive: bool = False) -> Dict[str, Any]:
        """
        Discover agents in the network based on capabilities.
        
        Args:
            capabilities: Required capabilities (None for all agents)
            max_agents: Maximum number of agents to return
            include_inactive: Whether to include inactive agents
            
        Returns:
            Dict containing discovered agents and network information
        """
        try:
            if capabilities:
                discovered_agents = self.network_registry.discover_agents(
                    capabilities=capabilities,
                    max_agents=max_agents
                )
            else:
                # Get all agents
                all_agents = list(self.network_registry.agents.values())
                if not include_inactive:
                    all_agents = [a for a in all_agents if a.status == AgentStatus.ACTIVE]
                
                # Sort by compatibility score
                all_agents.sort(key=lambda a: a.cognitive_compatibility_score, reverse=True)
                discovered_agents = all_agents[:max_agents]
            
            # Convert to serializable format
            agents_info = []
            for agent in discovered_agents:
                agent_info = {
                    "agent_id": agent.agent_id,
                    "hostname": agent.hostname,
                    "port": agent.port,
                    "capabilities": [cap.name for cap in agent.capabilities],
                    "status": agent.status.value,
                    "compatibility_score": agent.cognitive_compatibility_score,
                    "protocols": [p.value for p in agent.protocols],
                    "metadata": agent.metadata,
                    "load_metrics": agent.load_metrics
                }
                agents_info.append(agent_info)
            
            # Get network statistics
            network_stats = self.network_registry.get_network_stats()
            
            result = {
                "success": True,
                "discovered_agents": agents_info,
                "total_discovered": len(agents_info),
                "search_criteria": {
                    "capabilities": capabilities,
                    "max_agents": max_agents,
                    "include_inactive": include_inactive
                },
                "network_stats": {
                    "total_agents": network_stats.total_agents,
                    "active_agents": network_stats.active_agents,
                    "avg_compatibility": network_stats.cognitive_compatibility_avg
                }
            }
            
            logger.info(f"Discovered {len(agents_info)} agents in network")
            return result
            
        except Exception as e:
            logger.error(f"Error discovering network agents: {e}")
            return {
                "success": False,
                "error": str(e),
                "capabilities": capabilities
            }

    # Core Method 4: Broadcast to Network
    def broadcast_to_network(self,
                           message_content: Dict[str, Any],
                           target_capabilities: Optional[List[str]] = None,
                           priority: str = "normal",
                           ttl: int = 3) -> Dict[str, Any]:
        """
        Broadcast a message to all or specific agents in the network.
        
        Args:
            message_content: Content to broadcast
            target_capabilities: Target agents with specific capabilities
            priority: Message priority
            ttl: Time to live (maximum hops)
            
        Returns:
            Dict containing broadcast results
        """
        try:
            broadcast_id = str(uuid.uuid4())
            
            # Create broadcast message
            broadcast_msg = BroadcastMessage(
                message_id=broadcast_id,
                sender_id=self.agent_id,
                content=message_content,
                target_capabilities=target_capabilities,
                priority=priority,
                ttl=ttl
            )
            
            # Store in cache to prevent loops
            self.broadcast_cache[broadcast_id] = broadcast_msg
            
            # Determine target agents
            if target_capabilities:
                target_agents = self.network_registry.discover_agents(
                    capabilities=target_capabilities,
                    max_agents=100  # Large number to get all matching agents
                )
            else:
                # Broadcast to all active agents
                target_agents = [
                    agent for agent in self.network_registry.agents.values()
                    if agent.status == AgentStatus.ACTIVE and agent.agent_id != self.agent_id
                ]
            
            # Send broadcast messages
            broadcast_results = []
            successful_broadcasts = 0
            
            for agent in target_agents:
                # Create broadcast message
                broadcast_content = {
                    "broadcast_id": broadcast_id,
                    "original_sender": self.agent_id,
                    "content": message_content,
                    "priority": priority,
                    "ttl": ttl,
                    "broadcast_time": time.time()
                }
                
                # Send as inform message
                result = self.send_cognitive_message(
                    target_agent_id=agent.agent_id,
                    intent=CommunicativeIntent.INFORM,
                    frame=CognitiveFrame.INFORMATION_SHARING,
                    content=broadcast_content,
                    roles={
                        "agent": self.agent_id,
                        "patient": agent.agent_id
                    }
                )
                
                broadcast_results.append({
                    "target_agent": agent.agent_id,
                    "success": result["success"],
                    "message_id": result.get("message_id"),
                    "error": result.get("error")
                })
                
                if result["success"]:
                    successful_broadcasts += 1
            
            result = {
                "success": True,
                "broadcast_id": broadcast_id,
                "total_targets": len(target_agents),
                "successful_broadcasts": successful_broadcasts,
                "failed_broadcasts": len(target_agents) - successful_broadcasts,
                "broadcast_results": broadcast_results,
                "target_capabilities": target_capabilities,
                "priority": priority
            }
            
            logger.info(f"Broadcast {broadcast_id} sent to {successful_broadcasts}/{len(target_agents)} agents")
            return result
            
        except Exception as e:
            logger.error(f"Error broadcasting to network: {e}")
            return {
                "success": False,
                "error": str(e),
                "message_content": message_content
            }

    # Core Method 5: Query Agent Capabilities
    def query_agent_capabilities(self,
                               target_agent_id: Optional[str] = None,
                               capability_filter: Optional[List[str]] = None) -> Dict[str, Any]:
        """
        Query capabilities of specific agent or all agents.
        
        Args:
            target_agent_id: Specific agent to query (None for all agents)
            capability_filter: Filter results by capability names
            
        Returns:
            Dict containing capability information
        """
        try:
            if target_agent_id:
                # Query specific agent
                if target_agent_id not in self.network_registry.agents:
                    return {
                        "success": False,
                        "error": f"Agent {target_agent_id} not found",
                        "target_agent": target_agent_id
                    }
                
                agent = self.network_registry.agents[target_agent_id]
                capabilities = agent.capabilities
                
                # Apply filter if specified
                if capability_filter:
                    capabilities = [
                        cap for cap in capabilities
                        if cap.name in capability_filter
                    ]
                
                # Send query message
                query_result = self.send_cognitive_message(
                    target_agent_id=target_agent_id,
                    intent=CommunicativeIntent.QUERY,
                    frame=CognitiveFrame.CAPABILITY_NEGOTIATION,
                    content={
                        "query_type": "capabilities",
                        "capability_filter": capability_filter
                    },
                    roles={
                        "agent": self.agent_id,
                        "patient": target_agent_id
                    }
                )
                
                result = {
                    "success": True,
                    "target_agent": target_agent_id,
                    "capabilities": [
                        cap.name if hasattr(cap, 'name') else str(cap) 
                        for cap in capabilities
                    ],
                    "agent_status": agent.status.value,
                    "compatibility_score": agent.cognitive_compatibility_score,
                    "query_message": query_result
                }
                
            else:
                # Query all agents
                all_capabilities = {}
                capability_summary = defaultdict(list)
                
                for agent_id, agent in self.network_registry.agents.items():
                    agent_capabilities = agent.capabilities
                    
                    # Apply filter if specified
                    if capability_filter:
                        agent_capabilities = [
                            cap for cap in agent_capabilities
                            if cap.name in capability_filter
                        ]
                    
                    all_capabilities[agent_id] = {
                        "capabilities": [
                            cap.name if hasattr(cap, 'name') else str(cap) 
                            for cap in agent_capabilities
                        ],
                        "status": agent.status.value,
                        "compatibility_score": agent.cognitive_compatibility_score
                    }
                    
                    # Build summary
                    for cap in agent_capabilities:
                        capability_summary[cap.name].append(agent_id)
                
                result = {
                    "success": True,
                    "query_type": "all_agents",
                    "all_capabilities": all_capabilities,
                    "capability_summary": dict(capability_summary),
                    "total_agents": len(all_capabilities),
                    "capability_filter": capability_filter
                }
            
            logger.info(f"Queried capabilities for {target_agent_id or 'all agents'}")
            return result
            
        except Exception as e:
            logger.error(f"Error querying agent capabilities: {e}")
            return {
                "success": False,
                "error": str(e),
                "target_agent": target_agent_id
            }

    # Core Method 6: Update Agent Status
    def update_agent_status(self,
                          new_status: Union[str, AgentStatus],
                          metadata: Optional[Dict[str, Any]] = None,
                          load_metrics: Optional[Dict[str, float]] = None) -> Dict[str, Any]:
        """
        Update this agent's status in the network.
        
        Args:
            new_status: New status for the agent
            metadata: Additional metadata to update
            load_metrics: Current load metrics
            
        Returns:
            Dict containing update results
        """
        try:
            # Convert string to enum if needed
            if isinstance(new_status, str):
                new_status = AgentStatus(new_status)
            
            # Update status in registry
            status_updated = self.network_registry.update_agent_status(
                agent_id=self.agent_id,
                status=new_status,
                metadata=metadata
            )
            
            # Update load metrics if provided
            metrics_updated = True
            if load_metrics:
                metrics_updated = self.network_registry.update_agent_load_metrics(
                    agent_id=self.agent_id,
                    load_metrics=load_metrics
                )
            
            # Broadcast status update to network
            status_broadcast = self.broadcast_to_network(
                message_content={
                    "status_update": {
                        "agent_id": self.agent_id,
                        "new_status": new_status.value,
                        "metadata": metadata,
                        "load_metrics": load_metrics,
                        "timestamp": time.time()
                    }
                },
                priority="high"
            )
            
            result = {
                "success": status_updated and metrics_updated,
                "agent_id": self.agent_id,
                "new_status": new_status.value,
                "metadata_updated": metadata is not None,
                "load_metrics_updated": load_metrics is not None,
                "broadcast_result": status_broadcast
            }
            
            logger.info(f"Updated agent {self.agent_id} status to {new_status.value}")
            return result
            
        except Exception as e:
            logger.error(f"Error updating agent status: {e}")
            return {
                "success": False,
                "error": str(e),
                "agent_id": self.agent_id
            }

    # Core Method 7: Negotiate Resources
    def negotiate_resources(self,
                          resource_requirements: Dict[str, Any],
                          target_agents: Optional[List[str]] = None,
                          negotiation_timeout: float = 30.0) -> Dict[str, Any]:
        """
        Negotiate resource allocation with other agents.
        
        Args:
            resource_requirements: Required resources specification
            target_agents: Specific agents to negotiate with (None for auto-discovery)
            negotiation_timeout: Maximum time to wait for responses
            
        Returns:
            Dict containing negotiation results
        """
        try:
            negotiation_id = str(uuid.uuid4())
            
            # Determine target agents for negotiation
            if target_agents:
                negotiation_targets = [
                    agent for agent in self.network_registry.agents.values()
                    if agent.agent_id in target_agents and agent.status == AgentStatus.ACTIVE
                ]
            else:
                # Auto-discover agents with resource management capabilities
                negotiation_targets = self.network_registry.discover_agents(
                    capabilities=["resource_management", "coordination"],
                    max_agents=5
                )
            
            if not negotiation_targets:
                return {
                    "success": False,
                    "error": "No suitable agents found for resource negotiation",
                    "resource_requirements": resource_requirements
                }
            
            # Send negotiation requests
            negotiation_results = []
            
            for agent in negotiation_targets:
                # Create negotiation message
                negotiation_content = {
                    "negotiation_id": negotiation_id,
                    "requester": self.agent_id,
                    "resource_requirements": resource_requirements,
                    "negotiation_timeout": negotiation_timeout,
                    "timestamp": time.time()
                }
                
                # Send negotiation request
                result = self.send_cognitive_message(
                    target_agent_id=agent.agent_id,
                    intent=CommunicativeIntent.NEGOTIATE,
                    frame=CognitiveFrame.RESOURCE_ALLOCATION,
                    content=negotiation_content,
                    roles={
                        "agent": self.agent_id,
                        "patient": agent.agent_id
                    }
                )
                
                negotiation_results.append({
                    "target_agent": agent.agent_id,
                    "request_sent": result["success"],
                    "message_id": result.get("message_id"),
                    "error": result.get("error")
                })
            
            # Simulate negotiation process (in real implementation, this would be asynchronous)
            successful_negotiations = sum(1 for r in negotiation_results if r["request_sent"])
            
            result = {
                "success": successful_negotiations > 0,
                "negotiation_id": negotiation_id,
                "resource_requirements": resource_requirements,
                "target_agents": [agent.agent_id for agent in negotiation_targets],
                "negotiation_requests_sent": successful_negotiations,
                "negotiation_results": negotiation_results,
                "status": "pending" if successful_negotiations > 0 else "failed"
            }
            
            logger.info(f"Initiated resource negotiation {negotiation_id} with {successful_negotiations} agents")
            return result
            
        except Exception as e:
            logger.error(f"Error negotiating resources: {e}")
            return {
                "success": False,
                "error": str(e),
                "resource_requirements": resource_requirements
            }

    # Core Method 8: Monitor Network Health
    def monitor_network_health(self, detailed: bool = False) -> Dict[str, Any]:
        """
        Monitor overall network health and performance.
        
        Args:
            detailed: Whether to include detailed metrics
            
        Returns:
            Dict containing network health information
        """
        try:
            # Get network statistics
            network_stats = self.network_registry.get_network_stats()
            
            # Get topology information
            topology_info = self.network_registry.get_network_topology()
            
            # Calculate health metrics
            agent_health = "healthy" if network_stats.active_agents > 0 else "critical"
            
            connectivity_health = "healthy"
            if network_stats.total_connections == 0:
                connectivity_health = "critical"
            elif network_stats.total_connections < network_stats.active_agents:
                connectivity_health = "warning"
            
            performance_health = "healthy"
            if network_stats.error_rate > 0.1:  # > 10% error rate
                performance_health = "critical"
            elif network_stats.error_rate > 0.05:  # > 5% error rate
                performance_health = "warning"
            
            compatibility_health = "healthy"
            if network_stats.cognitive_compatibility_avg < 0.5:
                compatibility_health = "critical"
            elif network_stats.cognitive_compatibility_avg < 0.7:
                compatibility_health = "warning"
            
            # Overall health assessment
            health_scores = {
                "healthy": 3,
                "warning": 2,
                "critical": 1
            }
            
            overall_score = (
                health_scores[agent_health] +
                health_scores[connectivity_health] +
                health_scores[performance_health] +
                health_scores[compatibility_health]
            ) / 4
            
            if overall_score >= 2.5:
                overall_health = "healthy"
            elif overall_score >= 1.5:
                overall_health = "warning"
            else:
                overall_health = "critical"
            
            # Basic health report
            health_report = {
                "success": True,
                "overall_health": overall_health,
                "health_score": overall_score,
                "component_health": {
                    "agents": agent_health,
                    "connectivity": connectivity_health,
                    "performance": performance_health,
                    "compatibility": compatibility_health
                },
                "network_stats": {
                    "total_agents": network_stats.total_agents,
                    "active_agents": network_stats.active_agents,
                    "total_connections": network_stats.total_connections,
                    "error_rate": network_stats.error_rate,
                    "avg_compatibility": network_stats.cognitive_compatibility_avg
                },
                "topology": {
                    "type": topology_info.get("topology_type"),
                    "efficiency": network_stats.topology_efficiency
                },
                "timestamp": time.time()
            }
            
            # Add detailed metrics if requested
            if detailed:
                health_report["detailed_metrics"] = {
                    "avg_latency": network_stats.avg_latency,
                    "total_bandwidth": network_stats.total_bandwidth,
                    "message_throughput": network_stats.message_throughput,
                    "topology_info": topology_info,
                    "agent_details": [
                        {
                            "agent_id": agent.agent_id,
                            "status": agent.status.value,
                            "compatibility_score": agent.cognitive_compatibility_score,
                            "load_metrics": agent.load_metrics
                        }
                        for agent in self.network_registry.agents.values()
                    ]
                }
            
            logger.info(f"Network health: {overall_health} (score: {overall_score:.2f})")
            return health_report
            
        except Exception as e:
            logger.error(f"Error monitoring network health: {e}")
            return {
                "success": False,
                "error": str(e),
                "overall_health": "unknown"
            }

    # Core Method 9: Reconfigure Network Topology
    def reconfigure_network_topology(self,
                                   new_topology: Union[str, NetworkTopology],
                                   force: bool = False) -> Dict[str, Any]:
        """
        Reconfigure the network topology.
        
        Args:
            new_topology: New topology type
            force: Whether to force reconfiguration
            
        Returns:
            Dict containing reconfiguration results
        """
        try:
            # Convert string to enum if needed
            if isinstance(new_topology, str):
                new_topology = NetworkTopology(new_topology)
            
            current_topology = self.network_registry.topology
            
            # Check if change is needed
            if current_topology == new_topology and not force:
                return {
                    "success": True,
                    "message": "Topology already matches requested configuration",
                    "current_topology": current_topology.value,
                    "requested_topology": new_topology.value
                }
            
            # Get current network state
            pre_reconfig_stats = self.network_registry.get_network_stats()
            
            # Broadcast topology change notification
            topology_change_broadcast = self.broadcast_to_network(
                message_content={
                    "topology_change": {
                        "initiator": self.agent_id,
                        "old_topology": current_topology.value,
                        "new_topology": new_topology.value,
                        "timestamp": time.time()
                    }
                },
                priority="high"
            )
            
            # Perform reconfiguration
            reconfiguration_success = self.network_registry.reconfigure_topology(new_topology)
            
            if not reconfiguration_success:
                return {
                    "success": False,
                    "error": "Failed to reconfigure network topology",
                    "current_topology": current_topology.value,
                    "requested_topology": new_topology.value
                }
            
            # Get post-reconfiguration state
            post_reconfig_stats = self.network_registry.get_network_stats()
            
            # Calculate impact
            connection_change = post_reconfig_stats.total_connections - pre_reconfig_stats.total_connections
            efficiency_change = post_reconfig_stats.topology_efficiency - pre_reconfig_stats.topology_efficiency
            
            result = {
                "success": True,
                "topology_change": {
                    "from": current_topology.value,
                    "to": new_topology.value
                },
                "impact": {
                    "connection_change": connection_change,
                    "efficiency_change": efficiency_change,
                    "new_total_connections": post_reconfig_stats.total_connections,
                    "new_efficiency": post_reconfig_stats.topology_efficiency
                },
                "broadcast_result": topology_change_broadcast,
                "reconfiguration_time": time.time()
            }
            
            logger.info(f"Reconfigured network topology from {current_topology.value} to {new_topology.value}")
            return result
            
        except Exception as e:
            logger.error(f"Error reconfiguring network topology: {e}")
            return {
                "success": False,
                "error": str(e),
                "requested_topology": str(new_topology) if new_topology else None
            }

    # Additional utility methods
    def get_tool_status(self) -> Dict[str, Any]:
        """Get current status of the cognitive network tool."""
        return {
            "agent_id": self.agent_id,
            "active_coordinations": len(self.active_coordinations),
            "message_history_size": len(self.message_history),
            "broadcast_cache_size": len(self.broadcast_cache),
            "network_registry_active": self.network_registry._running if hasattr(self.network_registry, '_running') else True,
            "timestamp": time.time()
        }

    def clear_history(self, older_than_hours: float = 24.0) -> Dict[str, Any]:
        """Clear old messages and coordination history."""
        try:
            current_time = time.time()
            cutoff_time = current_time - (older_than_hours * 3600)
            
            # Clear old messages
            old_message_count = len(self.message_history)
            self.message_history = [
                msg for msg in self.message_history
                if msg.timestamp and msg.timestamp > cutoff_time
            ]
            messages_cleared = old_message_count - len(self.message_history)
            
            # Clear old broadcasts
            old_broadcast_count = len(self.broadcast_cache)
            # Note: broadcast_cache doesn't have timestamps in this simple implementation
            # In a real implementation, you'd filter by timestamp
            broadcasts_cleared = 0
            
            # Clear completed coordinations
            old_coord_count = len(self.active_coordinations)
            # In a real implementation, you'd check coordination status and clear completed ones
            coordinations_cleared = 0
            
            return {
                "success": True,
                "messages_cleared": messages_cleared,
                "broadcasts_cleared": broadcasts_cleared,
                "coordinations_cleared": coordinations_cleared,
                "cutoff_hours": older_than_hours
            }
            
        except Exception as e:
            logger.error(f"Error clearing history: {e}")
            return {
                "success": False,
                "error": str(e)
            }


# Integration with Agent Zero infrastructure
class AgentZeroIntegration:
    """Integration layer for Agent Zero compatibility."""
    
    @staticmethod
    def enhanced_call_subordinate(cognitive_tool: CognitiveNetworkTool,
                                subordinate_id: str,
                                task: str,
                                cognitive_intent: str = "delegate",
                                coordination_type: str = "synchronous") -> Dict[str, Any]:
        """
        Enhanced call_subordinate with cognitive grammar integration.
        Maintains backward compatibility while adding cognitive features.
        """
        try:
            # Use cognitive network tool for enhanced communication
            result = cognitive_tool.send_cognitive_message(
                target_agent_id=subordinate_id,
                intent=cognitive_intent,
                frame="task_delegation",
                content={
                    "task": task,
                    "delegation_type": "subordinate_call",
                    "coordination_type": coordination_type
                },
                roles={
                    "agent": cognitive_tool.agent_id,
                    "patient": subordinate_id
                }
            )
            
            # Return in Agent Zero compatible format
            return {
                "success": result["success"],
                "subordinate_id": subordinate_id,
                "task": task,
                "cognitive_message": result.get("natural_language"),
                "message_id": result.get("message_id"),
                "error": result.get("error")
            }
            
        except Exception as e:
            logger.error(f"Error in enhanced call_subordinate: {e}")
            return {
                "success": False,
                "error": str(e),
                "subordinate_id": subordinate_id,
                "task": task
            }


# Example usage and testing functions
def example_usage():
    """Demonstrate the cognitive network tool."""
    print("=== Cognitive Network Tool Example ===\n")
    
    # Initialize components
    from collections import defaultdict
    registry = DistributedNetworkRegistry("main_node", NetworkTopology.MESH)
    registry.start()
    
    try:
        # Create cognitive network tool
        cognitive_tool = CognitiveNetworkTool("agent_001", registry)
        
        # Register some test agents
        test_agents = [
            NetworkAgent(
                agent_id="agent_002",
                hostname="localhost",
                port=8081,
                capabilities=[
                    AgentCapability("computation", "1.0", "Mathematical computation", {}, {}),
                    AgentCapability("planning", "1.0", "Task planning", {}, {})
                ],
                status=AgentStatus.ACTIVE,
                protocols=[CommunicationProtocol.HTTP],
                metadata={"type": "compute_agent"},
                last_heartbeat=time.time(),
                join_timestamp=time.time()
            ),
            NetworkAgent(
                agent_id="agent_003",
                hostname="localhost",
                port=8082,
                capabilities=[
                    AgentCapability("coordination", "1.0", "Agent coordination", {}, {}),
                    AgentCapability("planning", "1.0", "Task planning", {}, {})
                ],
                status=AgentStatus.ACTIVE,
                protocols=[CommunicationProtocol.WEBSOCKET],
                metadata={"type": "coordination_agent"},
                last_heartbeat=time.time(),
                join_timestamp=time.time()
            )
        ]
        
        for agent in test_agents:
            registry.register_agent(agent)
        
        print("1. Testing send_cognitive_message:")
        message_result = cognitive_tool.send_cognitive_message(
            target_agent_id="agent_002",
            intent="request",
            frame="task_delegation",
            content={"task": "Calculate fibonacci sequence"},
            roles={"agent": "agent_001", "patient": "agent_002"}
        )
        print(f"   Success: {message_result['success']}")
        print(f"   Natural language: {message_result.get('natural_language')}\n")
        
        print("2. Testing coordinate_with_agents:")
        coordination_result = cognitive_tool.coordinate_with_agents(
            task_description="Implement distributed microservices architecture",
            required_capabilities=["computation", "planning"],
            max_partners=2
        )
        print(f"   Success: {coordination_result['success']}")
        print(f"   Assigned agents: {coordination_result.get('assigned_agents')}\n")
        
        print("3. Testing discover_network_agents:")
        discovery_result = cognitive_tool.discover_network_agents(
            capabilities=["planning"],
            max_agents=5
        )
        print(f"   Success: {discovery_result['success']}")
        print(f"   Discovered: {discovery_result.get('total_discovered')} agents\n")
        
        print("4. Testing broadcast_to_network:")
        broadcast_result = cognitive_tool.broadcast_to_network(
            message_content={"announcement": "System maintenance in 1 hour"},
            priority="high"
        )
        print(f"   Success: {broadcast_result['success']}")
        print(f"   Successful broadcasts: {broadcast_result.get('successful_broadcasts')}\n")
        
        print("5. Testing monitor_network_health:")
        health_result = cognitive_tool.monitor_network_health(detailed=True)
        print(f"   Success: {health_result['success']}")
        print(f"   Overall health: {health_result.get('overall_health')}")
        print(f"   Health score: {health_result.get('health_score'):.2f}\n")
        
        print("6. Testing reconfigure_network_topology:")
        reconfig_result = cognitive_tool.reconfigure_network_topology("star")
        print(f"   Success: {reconfig_result['success']}")
        print(f"   Topology change: {reconfig_result.get('topology_change')}\n")
        
    finally:
        registry.stop()


if __name__ == "__main__":
    example_usage()