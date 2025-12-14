#!/usr/bin/env python3
"""
Interactive demonstration of the Distributed Cognition Infrastructure.

This script showcases the complete functionality of the cognitive grammar framework,
distributed network registry, and cognitive network tool working together.
"""

import time
import json
import sys
import os

# Add the python directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from helpers.cognitive_grammar import CognitiveGrammarFramework, CommunicativeIntent, CognitiveFrame
from helpers.distributed_network import (
    DistributedNetworkRegistry, NetworkAgent, AgentCapability, 
    NetworkTopology, AgentStatus, CommunicationProtocol
)
from tools.cognitive_network import CognitiveNetworkTool


def print_section(title):
    """Print a formatted section header."""
    print("\n" + "="*60)
    print(f" {title}")
    print("="*60)


def print_subsection(title):
    """Print a formatted subsection header."""
    print(f"\n--- {title} ---")


def create_sample_agents():
    """Create sample agents for demonstration."""
    agents = []
    
    # Agent 1: Computation specialist
    agent1 = NetworkAgent(
        agent_id="compute_agent_001",
        hostname="compute-node-1.local",
        port=8080,
        capabilities=[
            AgentCapability("computation", "2.0", "High-performance mathematical computation", 
                          {"precision": "double", "vectorization": True}, 
                          {"cpu_cores": 8, "memory_gb": 16}),
            AgentCapability("data_processing", "1.5", "Large-scale data processing", 
                          {"batch_size": 10000}, 
                          {"storage_gb": 500}),
            AgentCapability("machine_learning", "1.0", "ML model training and inference", 
                          {"frameworks": ["tensorflow", "pytorch"]}, 
                          {"gpu_memory_gb": 24})
        ],
        status=AgentStatus.ACTIVE,
        protocols=[CommunicationProtocol.HTTP, CommunicationProtocol.INFERNO_9P],
        metadata={
            "type": "computation_specialist",
            "specialization": "numerical_computing",
            "region": "us-west",
            "version": "2.1.0"
        },
        last_heartbeat=time.time(),
        join_timestamp=time.time()
    )
    agents.append(agent1)
    
    # Agent 2: Coordination and planning specialist
    agent2 = NetworkAgent(
        agent_id="coord_agent_002",
        hostname="coord-node-1.local",
        port=8081,
        capabilities=[
            AgentCapability("coordination", "2.5", "Multi-agent coordination and orchestration", 
                          {"max_agents": 50, "protocols": ["http", "websocket"]}, 
                          {"cpu_cores": 4, "memory_gb": 8}),
            AgentCapability("planning", "2.0", "Task planning and optimization", 
                          {"algorithms": ["a_star", "genetic", "simulated_annealing"]}, 
                          {"cpu_cores": 4}),
            AgentCapability("resource_management", "1.8", "Resource allocation and monitoring", 
                          {"tracking_metrics": 100}, 
                          {"memory_gb": 8})
        ],
        status=AgentStatus.ACTIVE,
        protocols=[CommunicationProtocol.WEBSOCKET, CommunicationProtocol.INFERNO_9P],
        metadata={
            "type": "coordination_specialist",
            "specialization": "task_orchestration", 
            "region": "us-east",
            "version": "1.9.0"
        },
        last_heartbeat=time.time(),
        join_timestamp=time.time()
    )
    agents.append(agent2)
    
    # Agent 3: Data and analytics specialist
    agent3 = NetworkAgent(
        agent_id="data_agent_003",
        hostname="data-node-1.local",
        port=8082,
        capabilities=[
            AgentCapability("data_analysis", "2.2", "Advanced data analytics and visualization", 
                          {"techniques": ["regression", "clustering", "classification"]}, 
                          {"memory_gb": 32, "storage_gb": 1000}),
            AgentCapability("database_management", "1.9", "Database operations and optimization", 
                          {"databases": ["postgresql", "mongodb", "elasticsearch"]}, 
                          {"storage_gb": 2000}),
            AgentCapability("reporting", "1.5", "Automated report generation", 
                          {"formats": ["pdf", "html", "json"]}, 
                          {"cpu_cores": 2})
        ],
        status=AgentStatus.ACTIVE,
        protocols=[CommunicationProtocol.HTTP, CommunicationProtocol.TCP],
        metadata={
            "type": "data_specialist",
            "specialization": "analytics_and_storage",
            "region": "eu-west",
            "version": "2.0.1"
        },
        last_heartbeat=time.time(),
        join_timestamp=time.time()
    )
    agents.append(agent3)
    
    return agents


def demonstrate_cognitive_grammar():
    """Demonstrate cognitive grammar framework."""
    print_section("COGNITIVE GRAMMAR FRAMEWORK DEMONSTRATION")
    
    framework = CognitiveGrammarFramework()
    
    print_subsection("Creating Different Types of Cognitive Messages")
    
    # Task delegation message
    task_msg = framework.create_task_delegation_message(
        agent_id="coord_agent_002",
        task_description="Implement distributed microservices architecture for e-commerce platform",
        assignee="compute_agent_001",
        deadline="2024-02-15",
        priority="high"
    )
    
    print("Task Delegation Message:")
    print(f"  Natural Language: {task_msg.to_natural_language()}")
    print(f"  Intent: {task_msg.intent.value}")
    print(f"  Frame: {task_msg.frame.value}")
    print(f"  Roles: {list(task_msg.roles.keys())}")
    
    # Information sharing message
    info_msg = framework.create_information_sharing_message(
        agent_id="data_agent_003",
        topic="system performance metrics",
        information="CPU utilization: 68%, Memory usage: 45%, Network throughput: 2.3 Gbps",
        recipient="coord_agent_002"
    )
    
    print(f"\nInformation Sharing Message:")
    print(f"  Natural Language: {info_msg.to_natural_language()}")
    
    # Coordination message
    coord_msg = framework.create_coordination_message(
        agent_id="coord_agent_002",
        task="distributed database migration",
        participants=["compute_agent_001", "data_agent_003"],
        coordination_type="synchronous"
    )
    
    print(f"\nCoordination Message:")
    print(f"  Natural Language: {coord_msg.to_natural_language()}")
    
    print_subsection("JSON Serialization and Deserialization")
    
    # Demonstrate JSON serialization
    json_str = task_msg.to_json()
    print("Serialized Task Message (first 200 chars):")
    print(f"  {json_str[:200]}...")
    
    # Demonstrate deserialization
    restored_msg = framework.create_message.__class__.from_json(json_str)
    print(f"\nDeserialization successful: {restored_msg.agent_id == task_msg.agent_id}")
    print(f"Message validation: {framework.validate_message(restored_msg)}")
    
    print_subsection("Natural Language Parsing")
    
    # Parse natural language into cognitive messages
    test_phrases = [
        "Please complete the data analysis task by tomorrow",
        "The system is reporting high CPU usage",
        "Let's coordinate the deployment across all regions",
        "What capabilities are available for machine learning?",
        "I confirm the task has been completed successfully"
    ]
    
    for phrase in test_phrases:
        parsed_msg = framework.parse_natural_language(phrase, "demo_agent")
        if parsed_msg:
            print(f"  '{phrase}' -> Intent: {parsed_msg.intent.value}, Frame: {parsed_msg.frame.value}")


def demonstrate_distributed_network():
    """Demonstrate distributed network registry."""
    print_section("DISTRIBUTED NETWORK REGISTRY DEMONSTRATION")
    
    # Create registry with mesh topology
    registry = DistributedNetworkRegistry(
        node_id="main_registry_node",
        topology=NetworkTopology.MESH,
        heartbeat_interval=30.0,
        compatibility_threshold=0.6
    )
    
    # Start the registry
    registry.start()
    
    try:
        print_subsection("Agent Registration and Discovery")
        
        # Create and register sample agents
        agents = create_sample_agents()
        
        for agent in agents:
            result = registry.register_agent(agent)
            print(f"Registered {agent.agent_id}: Success = {result}, "
                  f"Compatibility = {agent.cognitive_compatibility_score:.2f}")
        
        print(f"\nTotal agents registered: {len(registry.agents)}")
        
        print_subsection("Capability-Based Agent Discovery")
        
        # Discover agents with specific capabilities
        discovery_tests = [
            (["computation"], "Computation specialists"),
            (["coordination", "planning"], "Coordination and planning agents"),
            (["data_analysis"], "Data analysis specialists"),
            (["machine_learning", "data_processing"], "ML and data processing agents")
        ]
        
        for capabilities, description in discovery_tests:
            discovered = registry.discover_agents(capabilities, max_agents=5)
            print(f"\n{description}:")
            for agent in discovered:
                agent_caps = [cap.name for cap in agent.capabilities]
                print(f"  - {agent.agent_id}: {agent_caps}")
        
        print_subsection("Network Topology Management")
        
        # Show current topology
        topology_info = registry.get_network_topology()
        print(f"Current topology: {topology_info['topology_type']}")
        print(f"Total connections: {len(topology_info['connections'])}")
        
        # Demonstrate topology reconfiguration
        topologies_to_test = [NetworkTopology.STAR, NetworkTopology.RING, NetworkTopology.TREE]
        
        for new_topology in topologies_to_test:
            print(f"\nReconfiguring to {new_topology.value} topology...")
            success = registry.reconfigure_topology(new_topology)
            if success:
                new_info = registry.get_network_topology()
                print(f"  New connections: {len(new_info['connections'])}")
        
        print_subsection("Network Statistics and Health")
        
        # Get comprehensive network statistics
        stats = registry.get_network_stats()
        print(f"Network Statistics:")
        print(f"  Total agents: {stats.total_agents}")
        print(f"  Active agents: {stats.active_agents}")
        print(f"  Total connections: {stats.total_connections}")
        print(f"  Average latency: {stats.avg_latency:.1f}ms")
        print(f"  Total bandwidth: {stats.total_bandwidth:.1f} Mbps")
        print(f"  Error rate: {stats.error_rate:.1%}")
        print(f"  Topology efficiency: {stats.topology_efficiency:.2f}")
        print(f"  Average compatibility: {stats.cognitive_compatibility_avg:.2f}")
        
        print_subsection("Agent Status Updates")
        
        # Update agent statuses
        status_updates = [
            ("compute_agent_001", AgentStatus.BUSY, {"current_task": "training_ml_model"}),
            ("data_agent_003", AgentStatus.ACTIVE, {"current_task": "idle"})
        ]
        
        for agent_id, status, metadata in status_updates:
            success = registry.update_agent_status(agent_id, status, metadata)
            print(f"Updated {agent_id} status to {status.value}: {success}")
        
        # Update load metrics
        load_updates = [
            ("compute_agent_001", {"cpu_usage": 95.5, "memory_usage": 78.2, "task_queue_length": 5}),
            ("coord_agent_002", {"cpu_usage": 45.0, "memory_usage": 52.1, "task_queue_length": 2})
        ]
        
        for agent_id, metrics in load_updates:
            success = registry.update_agent_load_metrics(agent_id, metrics)
            print(f"Updated {agent_id} load metrics: {success}")
        
    finally:
        registry.stop()


def demonstrate_cognitive_network_tool():
    """Demonstrate cognitive network tool functionality."""
    print_section("COGNITIVE NETWORK TOOL DEMONSTRATION")
    
    # Create registry and tool
    registry = DistributedNetworkRegistry("tool_demo_node", NetworkTopology.MESH)
    registry.start()
    
    try:
        # Register sample agents
        agents = create_sample_agents()
        for agent in agents:
            registry.register_agent(agent)
        
        # Create cognitive network tool
        cognitive_tool = CognitiveNetworkTool("coordinator_agent", registry)
        
        print_subsection("Core Method 1: Send Cognitive Message")
        
        message_result = cognitive_tool.send_cognitive_message(
            target_agent_id="compute_agent_001",
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            content={
                "task": "Optimize neural network training algorithms",
                "expected_duration": "4 hours",
                "resources_required": ["GPU", "high_memory"]
            },
            roles={"agent": "coordinator_agent", "patient": "compute_agent_001"}
        )
        
        print(f"Message sent successfully: {message_result['success']}")
        print(f"Natural language: {message_result['natural_language']}")
        
        print_subsection("Core Method 2: Coordinate with Agents")
        
        coordination_result = cognitive_tool.coordinate_with_agents(
            task_description="Implement distributed machine learning pipeline",
            required_capabilities=["computation", "data_processing", "coordination"],
            max_partners=3,
            coordination_type="asynchronous",
            deadline="2024-02-28",
            priority="high"
        )
        
        print(f"Coordination successful: {coordination_result['success']}")
        print(f"Task ID: {coordination_result.get('task_id')}")
        print(f"Assigned agents: {coordination_result.get('assigned_agents')}")
        
        print_subsection("Core Method 3: Discover Network Agents")
        
        discovery_result = cognitive_tool.discover_network_agents(
            capabilities=["machine_learning", "data_analysis"],
            max_agents=10,
            include_inactive=False
        )
        
        print(f"Discovery successful: {discovery_result['success']}")
        print(f"Agents discovered: {discovery_result['total_discovered']}")
        for agent_info in discovery_result['discovered_agents']:
            print(f"  - {agent_info['agent_id']}: {agent_info['capabilities']}")
        
        print_subsection("Core Method 4: Broadcast to Network")
        
        broadcast_result = cognitive_tool.broadcast_to_network(
            message_content={
                "announcement": "Scheduled maintenance window: 2024-01-20 02:00-04:00 UTC",
                "action_required": "Save work and prepare for temporary downtime",
                "contact": "ops-team@company.com"
            },
            priority="high",
            ttl=3
        )
        
        print(f"Broadcast successful: {broadcast_result['success']}")
        print(f"Targets reached: {broadcast_result['successful_broadcasts']}/{broadcast_result['total_targets']}")
        
        print_subsection("Core Method 5: Query Agent Capabilities")
        
        capability_result = cognitive_tool.query_agent_capabilities(
            target_agent_id="compute_agent_001"
        )
        
        print(f"Capability query successful: {capability_result['success']}")
        if capability_result['success']:
            print(f"Agent capabilities: {len(capability_result['capabilities'])}")
            for cap in capability_result['capabilities']:
                print(f"  - {cap['name']} v{cap['version']}: {cap['description']}")
        
        print_subsection("Core Method 6: Update Agent Status")
        
        status_result = cognitive_tool.update_agent_status(
            new_status=AgentStatus.BUSY,
            metadata={"current_operation": "distributed_ml_coordination"},
            load_metrics={"cpu_usage": 65.0, "memory_usage": 55.0, "task_queue_length": 3}
        )
        
        print(f"Status update successful: {status_result['success']}")
        print(f"New status: {status_result['new_status']}")
        
        print_subsection("Core Method 7: Negotiate Resources")
        
        negotiation_result = cognitive_tool.negotiate_resources(
            resource_requirements={
                "cpu_cores": 16,
                "memory_gb": 64, 
                "gpu_memory_gb": 48,
                "storage_gb": 1000,
                "network_bandwidth_mbps": 1000
            },
            negotiation_timeout=45.0
        )
        
        print(f"Resource negotiation initiated: {negotiation_result['success']}")
        if negotiation_result['success']:
            print(f"Negotiation ID: {negotiation_result['negotiation_id']}")
            print(f"Negotiation requests sent: {negotiation_result['negotiation_requests_sent']}")
        
        print_subsection("Core Method 8: Monitor Network Health")
        
        health_result = cognitive_tool.monitor_network_health(detailed=True)
        
        print(f"Health monitoring successful: {health_result['success']}")
        print(f"Overall health: {health_result['overall_health']}")
        print(f"Health score: {health_result['health_score']:.2f}/3.0")
        
        component_health = health_result['component_health']
        print("Component health:")
        for component, health in component_health.items():
            print(f"  - {component}: {health}")
        
        print_subsection("Core Method 9: Reconfigure Network Topology")
        
        reconfig_result = cognitive_tool.reconfigure_network_topology(
            new_topology=NetworkTopology.HYBRID,
            force=True
        )
        
        print(f"Topology reconfiguration successful: {reconfig_result['success']}")
        if reconfig_result['success']:
            topology_change = reconfig_result['topology_change']
            print(f"Changed from {topology_change['from']} to {topology_change['to']}")
            impact = reconfig_result['impact']
            print(f"Connection change: {impact['connection_change']}")
            print(f"Efficiency change: {impact['efficiency_change']:.3f}")
        
        print_subsection("Tool Status and Management")
        
        # Get tool status
        tool_status = cognitive_tool.get_tool_status()
        print("Tool Status:")
        print(f"  Agent ID: {tool_status['agent_id']}")
        print(f"  Active coordinations: {tool_status['active_coordinations']}")
        print(f"  Message history size: {tool_status['message_history_size']}")
        print(f"  Broadcast cache size: {tool_status['broadcast_cache_size']}")
        
        # Clear old history
        clear_result = cognitive_tool.clear_history(older_than_hours=0.1)  # Very recent for demo
        print(f"\nHistory cleanup: {clear_result['success']}")
        print(f"Messages cleared: {clear_result['messages_cleared']}")
        
    finally:
        registry.stop()


def demonstrate_integration_scenarios():
    """Demonstrate real-world integration scenarios."""
    print_section("INTEGRATION SCENARIOS")
    
    registry = DistributedNetworkRegistry("integration_demo", NetworkTopology.HYBRID)
    registry.start()
    
    try:
        # Create a diverse set of agents
        agents = create_sample_agents()
        
        # Add more specialized agents
        specialized_agents = [
            NetworkAgent(
                agent_id="security_agent_004",
                hostname="security-node.local",
                port=8083,
                capabilities=[
                    AgentCapability("security_scanning", "1.0", "Security vulnerability scanning", {}, {}),
                    AgentCapability("threat_detection", "1.2", "Real-time threat detection", {}, {}),
                    AgentCapability("compliance_monitoring", "1.0", "Regulatory compliance monitoring", {}, {})
                ],
                status=AgentStatus.ACTIVE,
                protocols=[CommunicationProtocol.INFERNO_9P, CommunicationProtocol.TCP],
                metadata={"type": "security_specialist", "clearance_level": "high"},
                last_heartbeat=time.time(),
                join_timestamp=time.time()
            ),
            NetworkAgent(
                agent_id="monitoring_agent_005",
                hostname="monitor-node.local", 
                port=8084,
                capabilities=[
                    AgentCapability("system_monitoring", "2.0", "System health monitoring", {}, {}),
                    AgentCapability("alerting", "1.5", "Intelligent alerting system", {}, {}),
                    AgentCapability("log_analysis", "1.8", "Log aggregation and analysis", {}, {})
                ],
                status=AgentStatus.ACTIVE,
                protocols=[CommunicationProtocol.HTTP, CommunicationProtocol.WEBSOCKET],
                metadata={"type": "monitoring_specialist", "scope": "global"},
                last_heartbeat=time.time(),
                join_timestamp=time.time()
            )
        ]
        
        all_agents = agents + specialized_agents
        
        # Register all agents
        for agent in all_agents:
            registry.register_agent(agent)
        
        cognitive_tool = CognitiveNetworkTool("orchestrator_agent", registry)
        
        print_subsection("Scenario 1: Multi-Agent ML Pipeline Deployment")
        
        # Coordinate ML pipeline deployment
        ml_coordination = cognitive_tool.coordinate_with_agents(
            task_description="Deploy end-to-end machine learning pipeline with monitoring and security",
            required_capabilities=["machine_learning", "data_processing", "security_scanning", "system_monitoring"],
            max_partners=4,
            coordination_type="synchronous",
            priority="critical"
        )
        
        print(f"ML Pipeline coordination: {ml_coordination['success']}")
        if ml_coordination['success']:
            print(f"Participating agents: {ml_coordination['assigned_agents']}")
        
        print_subsection("Scenario 2: Security Incident Response")
        
        # Broadcast security alert
        security_broadcast = cognitive_tool.broadcast_to_network(
            message_content={
                "alert_type": "security_incident",
                "severity": "high",
                "description": "Potential data breach detected in user authentication system",
                "response_required": "immediate",
                "incident_id": "SEC-2024-001"
            },
            target_capabilities=["security_scanning", "threat_detection", "system_monitoring"],
            priority="critical"
        )
        
        print(f"Security alert broadcast: {security_broadcast['success']}")
        
        # Coordinate incident response
        incident_response = cognitive_tool.coordinate_with_agents(
            task_description="Investigate and contain security incident SEC-2024-001",
            required_capabilities=["security_scanning", "threat_detection", "log_analysis"],
            max_partners=3,
            coordination_type="synchronous",
            priority="critical"
        )
        
        print(f"Incident response coordination: {incident_response['success']}")
        
        print_subsection("Scenario 3: System Performance Optimization")
        
        # Query system performance capabilities
        performance_agents = cognitive_tool.discover_network_agents(
            capabilities=["system_monitoring", "data_analysis", "computation"],
            max_agents=5
        )
        
        print(f"Performance optimization agents found: {performance_agents['total_discovered']}")
        
        # Negotiate resources for optimization
        optimization_resources = cognitive_tool.negotiate_resources(
            resource_requirements={
                "cpu_cores": 32,
                "memory_gb": 128,
                "storage_gb": 2000,
                "analysis_duration_hours": 24
            }
        )
        
        print(f"Resource negotiation for optimization: {optimization_resources['success']}")
        
        print_subsection("Scenario 4: Network Health Assessment")
        
        # Comprehensive health check
        health_assessment = cognitive_tool.monitor_network_health(detailed=True)
        
        print("Network Health Assessment:")
        print(f"  Overall health: {health_assessment['overall_health']}")
        print(f"  Health score: {health_assessment['health_score']:.2f}")
        
        # Show detailed metrics if available
        if 'detailed_metrics' in health_assessment:
            detailed = health_assessment['detailed_metrics']
            print(f"  Average latency: {detailed.get('avg_latency', 0):.1f}ms")
            print(f"  Total bandwidth: {detailed.get('total_bandwidth', 0):.1f} Mbps")
            print(f"  Message throughput: {detailed.get('message_throughput', 0):.1f} msg/min")
        
        print_subsection("Scenario 5: Dynamic Network Reconfiguration")
        
        # Monitor current topology efficiency
        current_stats = registry.get_network_stats()
        print(f"Current topology efficiency: {current_stats.topology_efficiency:.3f}")
        
        # Test different topologies for optimal performance
        topology_tests = [NetworkTopology.MESH, NetworkTopology.STAR, NetworkTopology.HYBRID]
        best_topology = None
        best_efficiency = 0
        
        for topology in topology_tests:
            reconfig_result = cognitive_tool.reconfigure_network_topology(topology)
            if reconfig_result['success']:
                new_efficiency = reconfig_result['impact']['new_efficiency']
                print(f"  {topology.value}: efficiency = {new_efficiency:.3f}")
                if new_efficiency > best_efficiency:
                    best_efficiency = new_efficiency
                    best_topology = topology
        
        print(f"Optimal topology: {best_topology.value if best_topology else 'unknown'}")
        
    finally:
        registry.stop()


def main():
    """Main demonstration function."""
    print("DISTRIBUTED COGNITION INFRASTRUCTURE DEMONSTRATION")
    print("Interactive showcase of cognitive grammar, distributed networking, and agent coordination")
    print("=" * 80)
    
    try:
        # Run all demonstrations
        demonstrate_cognitive_grammar()
        demonstrate_distributed_network()
        demonstrate_cognitive_network_tool()
        demonstrate_integration_scenarios()
        
        print_section("DEMONSTRATION COMPLETE")
        print("All components of the distributed cognition infrastructure have been demonstrated.")
        print("The system provides comprehensive capabilities for:")
        print("  • Cognitive grammar-based agent communication")
        print("  • Distributed network management and discovery")
        print("  • Multi-agent coordination and task delegation")
        print("  • Resource negotiation and network optimization")
        print("  • Real-time health monitoring and topology management")
        
    except KeyboardInterrupt:
        print("\n\nDemonstration interrupted by user.")
    except Exception as e:
        print(f"\n\nError during demonstration: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()