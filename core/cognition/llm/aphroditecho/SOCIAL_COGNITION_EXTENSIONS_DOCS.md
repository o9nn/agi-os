# Social Cognition Extensions Documentation

## Overview

The Social Cognition Extensions implement Task 2.3.2 of the Deep Tree Echo development roadmap, enabling multi-agent shared cognition, communication and collaboration protocols, and distributed problem solving within the AAR (Agent-Arena-Relation) orchestration system.

## Architecture

The social cognition extensions consist of four main components:

### 1. Social Cognition Manager (`aar_core/agents/social_cognition_manager.py`)

Manages multi-agent shared cognition capabilities including:

- **Shared Cognitive Resources**: Agents can share working memory, knowledge bases, processing pools, attention focus, and decision contexts
- **Cognition Sharing Modes**: Broadcast, targeted, pooled, and hierarchical sharing patterns
- **Collaborative Problem Solving**: Initiation and coordination of multi-agent problem-solving sessions
- **Consensus Building**: Weighted consensus mechanisms for group decision-making

#### Key Features:
- Resource lifecycle management with automatic cleanup
- Trust-based access control
- Performance metrics and collaboration tracking
- Agent capability profiling and matching

### 2. Communication Protocols (`aar_core/relations/communication_protocols.py`)

Implements structured communication protocols for agent collaboration:

- **Message Types**: Request/response, notifications, proposals, negotiations, agreements, coordination
- **Protocol Types**: Direct messaging, broadcast, consensus building, negotiation, task coordination, knowledge exchange, contract net protocol, auctions
- **Session Management**: Active protocol sessions with state tracking
- **Message Routing**: Intelligent routing based on protocol requirements

#### Supported Protocols:
- **Direct Messaging**: Point-to-point communication
- **Broadcast**: One-to-many information distribution
- **Consensus Building**: Structured voting and agreement processes
- **Negotiation**: Multi-round offer/counteroffer exchanges
- **Task Coordination**: Distributed task management and synchronization
- **Knowledge Exchange**: Structured knowledge sharing and validation
- **Contract Net Protocol**: Task allocation through bidding processes
- **Auction**: Competitive resource allocation

### 3. Collaborative Problem Solver (`aar_core/orchestration/collaborative_solver.py`)

Enables distributed problem-solving across multiple agents:

- **Problem Types**: Optimization, classification, planning, search, reasoning, creative synthesis, data analysis, simulation
- **Task Decomposition**: Automatic problem breakdown into manageable subtasks
- **Agent Assignment**: Intelligent task allocation based on agent capabilities
- **Solution Synthesis**: Multiple strategies for combining agent solutions (voting, weighted average, consensus, hierarchical, competitive, hybrid)

#### Problem Solving Flow:
1. Problem definition and complexity assessment
2. Task decomposition based on problem type
3. Agent capability matching and assignment
4. Distributed task execution
5. Solution collection and synthesis
6. Final result aggregation and validation

### 4. Extended Agent Capabilities

Enhanced the existing Agent class with social cognition features:

- **Cognitive Profiles**: Detailed capability descriptions and preferences
- **Resource Sharing**: Methods for sharing and accessing cognitive resources
- **Collaboration Participation**: Active participation in group problem-solving
- **Communication**: Structured messaging with trust tracking
- **Trust Networks**: Dynamic trust score management based on interaction outcomes

## Integration with AAR System

The social cognition extensions are seamlessly integrated into the existing AAR Core Orchestrator:

- **Configuration**: Social cognition features can be enabled/disabled via AARConfig
- **Automatic Registration**: Agents are automatically registered for social cognition when spawned
- **Collaborative Requests**: Requests requiring collaboration are automatically routed to the collaborative solver
- **Statistics Integration**: Social cognition metrics are included in system statistics

## Usage Examples

### Basic Agent Collaboration

```python
from aar_core.orchestration.core_orchestrator import AARCoreOrchestrator, AARConfig

# Configure with social cognition enabled
config = AARConfig(
    max_concurrent_agents=100,
    social_cognition_enabled=True,
    communication_protocols_enabled=True
)

orchestrator = AARCoreOrchestrator(config)

# Request requiring collaboration
request = {
    'title': 'Multi-objective Optimization',
    'description': 'Optimize resource allocation across multiple constraints',
    'features': ['collaboration', 'complex_reasoning', 'optimization'],
    'required_capabilities': {
        'capabilities': ['optimization', 'reasoning', 'analysis']
    },
    'collaboration_required': True
}

# Will automatically use collaborative problem solving
result = await orchestrator.orchestrate_inference(request)
```

### Direct Social Cognition Management

```python
from aar_core.agents.social_cognition_manager import SocialCognitionManager, SharedCognitionType

manager = SocialCognitionManager()

# Register agents
await manager.register_agent('agent_001', cognitive_profile)
await manager.register_agent('agent_002', cognitive_profile)

# Share cognitive resources
resource_id = await manager.share_cognition(
    'agent_001',
    SharedCognitionType.WORKING_MEMORY,
    {'problem_state': 'active', 'constraints': ['x > 0']},
    CognitionSharingMode.BROADCAST
)

# Initiate collaboration
collaboration_id = await manager.initiate_collaborative_problem_solving(
    'agent_001',
    problem_definition,
    ['agent_001', 'agent_002']
)
```

### Communication Protocols

```python
from aar_core.relations.communication_protocols import CommunicationProtocols, Message, MessageType, ProtocolType

protocols = CommunicationProtocols()

# Register agents
await protocols.register_agent('agent_001')
await protocols.register_agent('agent_002')

# Send structured message
message = Message(
    message_id='msg_001',
    sender_id='agent_001',
    recipient_id='agent_002',
    message_type=MessageType.REQUEST,
    protocol_type=ProtocolType.DIRECT_MESSAGE,
    content={'request': 'collaboration on optimization problem'}
)

await protocols.send_message(message)
```

## Performance Characteristics

### Social Cognition Manager
- **Resource Capacity**: Supports up to 1000 concurrent shared resources (configurable)
- **Agent Registration**: O(1) registration and lookup
- **Consensus Building**: Weighted voting with configurable thresholds
- **Cleanup**: Automatic resource expiration and cleanup

### Communication Protocols
- **Message Routing**: Efficient routing with O(1) agent lookup
- **Protocol Sessions**: Concurrent protocol session support
- **Message History**: Configurable history retention (default: 10,000 messages)
- **Session State**: Persistent session state tracking

### Collaborative Problem Solver
- **Concurrent Problems**: Up to 100 concurrent problem-solving sessions (configurable)
- **Task Decomposition**: Problem-type specific decomposition strategies
- **Solution Synthesis**: Multiple synthesis strategies with different performance characteristics
- **Metrics Tracking**: Comprehensive performance and collaboration metrics

## Configuration Options

### AARConfig Parameters
- `social_cognition_enabled`: Enable/disable social cognition features
- `max_shared_resources`: Maximum number of shared cognitive resources
- `max_concurrent_problems`: Maximum concurrent collaborative problem-solving sessions
- `communication_protocols_enabled`: Enable/disable structured communication protocols

### Social Cognition Manager Settings
- `max_resource_lifetime`: Maximum lifetime for shared resources (default: 1 hour)
- `max_collaborative_updates`: Maximum updates per resource (default: 100)
- `trust_threshold`: Minimum trust level for resource access (default: 0.3)
- `consensus_threshold`: Required consensus level for decisions (default: 0.7)

### Problem Solver Configuration
- `max_subtasks_per_problem`: Maximum subtasks per problem (default: 50)
- `task_timeout`: Individual task timeout (default: 5 minutes)
- `problem_timeout`: Overall problem timeout (default: 30 minutes)
- `min_solution_confidence`: Minimum confidence for solution acceptance (default: 0.3)

## Monitoring and Metrics

### Social Cognition Metrics
- Resources shared/accessed counts
- Collaboration event counts
- Consensus decision success rate
- Knowledge transfer statistics
- Collaboration effectiveness scores

### Communication Metrics
- Total messages processed
- Protocol session success rates
- Average response times
- Protocol efficiency by type

### Problem Solving Metrics
- Problems solved/failed counts
- Average solution time
- Task success rates
- Agent utilization statistics
- Solution quality metrics

## Validation and Testing

The implementation includes comprehensive validation through the `validate_social_cognition.py` script, which tests:

1. **Multi-agent Shared Cognition**: Resource sharing and access
2. **Communication Protocols**: Structured messaging and coordination
3. **Distributed Problem Solving**: Task decomposition and solution synthesis
4. **Trust Networks**: Trust development and collaboration quality
5. **System Integration**: End-to-end collaboration workflows

### Running Validation

```bash
cd /home/runner/work/aphroditecho/aphroditecho
python validate_social_cognition.py
```

## Acceptance Criteria Verification

✅ **"Agents collaborate to solve complex problems"**

Evidence of successful implementation:
- Multiple agents share cognitive resources (working memory, knowledge bases)
- Structured communication protocols enable coordination
- Distributed problem decomposition and parallel execution
- Trust networks develop through interaction history
- Consensus building for group decision-making
- Comprehensive collaboration metrics and quality tracking

## Future Enhancements

Potential areas for future development:
- Advanced consensus algorithms (Byzantine fault tolerance)
- Hierarchical collaboration structures
- Learning-based trust models
- Real-time collaboration visualization
- Integration with external collaboration tools
- Performance optimization for large-scale deployments

---

**Status**: ✅ COMPLETE - Task 2.3.2 Social Cognition Extensions successfully implemented and validated.