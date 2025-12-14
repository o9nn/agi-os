"""
Tests for Social Cognition Extensions

Tests the social cognition manager, communication protocols, and collaborative 
problem solving components that implement Task 2.3.2 requirements.
"""

import pytest
import time

from aar_core.agents.social_cognition_manager import (
    SocialCognitionManager, 
    SharedCognitionType, 
    CognitionSharingMode
)
from aar_core.relations.communication_protocols import (
    CommunicationProtocols, 
    Message, 
    MessageType, 
    ProtocolType, 
    MessagePriority
)
from aar_core.orchestration.collaborative_solver import (
    CollaborativeProblemSolver, 
    ProblemDefinition, 
    ProblemType, 
    SolutionStrategy
)


class TestSocialCognitionManager:
    """Test social cognition management functionality."""
    
    @pytest.fixture
    async def social_cognition_manager(self):
        """Create social cognition manager for testing."""
        manager = SocialCognitionManager(max_shared_resources=100)
        yield manager
        await manager.shutdown()
    
    @pytest.mark.asyncio
    async def test_agent_registration(self, social_cognition_manager):
        """Test agent registration for social cognition."""
        manager = social_cognition_manager
        
        # Register an agent
        cognitive_profile = {
            'capabilities': {'reasoning': True, 'collaboration': True},
            'specializations': ['problem_solving', 'analysis'],
            'memory_capacity': 2000,
            'processing_bandwidth': 1.5
        }
        
        await manager.register_agent('agent_001', cognitive_profile)
        
        # Check agent is registered
        assert 'agent_001' in manager.agent_cognitive_profiles
        profile = manager.agent_cognitive_profiles['agent_001']
        assert profile['agent_id'] == 'agent_001'
        assert profile['cognitive_capabilities']['reasoning'] is True
        assert 'problem_solving' in profile['specializations']
    
    @pytest.mark.asyncio
    async def test_share_cognition(self, social_cognition_manager):
        """Test sharing cognitive resources."""
        manager = social_cognition_manager
        
        # Register agents
        await manager.register_agent('agent_001', {'capabilities': {'reasoning': True}})
        await manager.register_agent('agent_002', {'capabilities': {'analysis': True}})
        
        # Share working memory
        shared_data = {
            'problem_context': 'Complex optimization problem',
            'current_state': {'variables': [1, 2, 3]},
            'constraints': ['x > 0', 'y < 10']
        }
        
        resource_id = await manager.share_cognition(
            'agent_001',
            SharedCognitionType.WORKING_MEMORY,
            shared_data,
            CognitionSharingMode.BROADCAST
        )
        
        # Verify resource was created
        assert resource_id in manager.shared_resources
        resource = manager.shared_resources[resource_id]
        assert resource.owner_agent_id == 'agent_001'
        assert resource.cognition_type == SharedCognitionType.WORKING_MEMORY
        assert resource.data['problem_context'] == 'Complex optimization problem'
    
    @pytest.mark.asyncio
    async def test_access_shared_cognition(self, social_cognition_manager):
        """Test accessing shared cognitive resources."""
        manager = social_cognition_manager
        
        # Register agents
        await manager.register_agent('agent_001', {'capabilities': {'reasoning': True}})
        await manager.register_agent('agent_002', {'capabilities': {'analysis': True}})
        
        # Share knowledge base
        shared_data = {'facts': ['fact1', 'fact2'], 'rules': ['rule1']}
        resource_id = await manager.share_cognition(
            'agent_001',
            SharedCognitionType.KNOWLEDGE_BASE,
            shared_data,
            CognitionSharingMode.POOLED
        )
        
        # Agent 2 accesses shared resource
        accessed_data = await manager.access_shared_cognition('agent_002', resource_id, 'read')
        
        # Verify access
        assert accessed_data is not None
        assert accessed_data['data']['facts'] == ['fact1', 'fact2']
        assert accessed_data['owner_agent_id'] == 'agent_001'
        assert accessed_data['cognition_type'] == 'knowledge_base'
    
    @pytest.mark.asyncio
    async def test_collaborative_problem_solving(self, social_cognition_manager):
        """Test collaborative problem solving initiation."""
        manager = social_cognition_manager
        
        # Register agents
        for i in range(3):
            await manager.register_agent(f'agent_{i:03d}', {
                'capabilities': {'reasoning': True, 'collaboration': True},
                'specializations': ['optimization']
            })
        
        # Define problem
        problem_definition = {
            'title': 'Resource Allocation Optimization',
            'description': 'Optimize allocation of limited resources',
            'complexity': 'high',
            'required_capabilities': ['optimization', 'reasoning']
        }
        
        # Initiate collaborative problem solving
        collaboration_id = await manager.initiate_collaborative_problem_solving(
            'agent_000',
            problem_definition,
            ['agent_000', 'agent_001', 'agent_002']
        )
        
        # Verify collaboration was created
        assert collaboration_id in manager.active_collaborations
        collaboration = manager.active_collaborations[collaboration_id]
        assert collaboration['initiator'] == 'agent_000'
        assert len(collaboration['participants']) == 3
        assert collaboration['status'] == 'active'
    
    @pytest.mark.asyncio
    async def test_consensus_building(self, social_cognition_manager):
        """Test consensus building among agents."""
        manager = social_cognition_manager
        
        # Register agents
        agents = ['agent_001', 'agent_002', 'agent_003']
        for agent_id in agents:
            await manager.register_agent(agent_id, {
                'capabilities': {'reasoning': True, 'decision_making': True}
            })
        
        # Create collaboration
        collaboration_id = await manager.initiate_collaborative_problem_solving(
            'agent_001',
            {'title': 'Decision Problem', 'description': 'Choose best option'},
            agents
        )
        
        # Add contributions from each agent
        contributions = [
            {'type': 'proposal', 'content': {'option': 'A', 'score': 0.8}},
            {'type': 'analysis', 'content': {'option': 'B', 'score': 0.6}},
            {'type': 'evaluation', 'content': {'option': 'A', 'score': 0.9}}
        ]
        
        for i, contribution in enumerate(contributions):
            success = await manager.contribute_to_collaboration(
                agents[i], collaboration_id, contribution
            )
            assert success is True
        
        # Build consensus
        decision_options = [
            {'id': 'option_A', 'description': 'Choose option A'},
            {'id': 'option_B', 'description': 'Choose option B'}
        ]
        
        consensus_result = await manager.build_consensus(collaboration_id, decision_options)
        
        # Verify consensus result
        assert 'selected_option' in consensus_result
        assert 'consensus_strength' in consensus_result
        assert consensus_result['participating_agents'] == agents


class TestCommunicationProtocols:
    """Test communication protocols functionality."""
    
    @pytest.fixture
    async def communication_protocols(self):
        """Create communication protocols for testing."""
        protocols = CommunicationProtocols()
        yield protocols
        await protocols.shutdown()
    
    @pytest.mark.asyncio
    async def test_agent_registration_and_messaging(self, communication_protocols):
        """Test agent registration and basic messaging."""
        protocols = communication_protocols
        
        # Register agents
        await protocols.register_agent('agent_001')
        await protocols.register_agent('agent_002')
        
        # Create and send message
        message = Message(
            message_id='msg_001',
            sender_id='agent_001',
            recipient_id='agent_002',
            message_type=MessageType.REQUEST,
            protocol_type=ProtocolType.DIRECT_MESSAGE,
            content={'request': 'Help with problem solving'},
            priority=MessagePriority.HIGH
        )
        
        success = await protocols.send_message(message)
        assert success is True
        
        # Agent 002 receives message
        received_message = await protocols.receive_message('agent_002', timeout=1.0)
        assert received_message is not None
        assert received_message.message_id == 'msg_001'
        assert received_message.sender_id == 'agent_001'
        assert received_message.content['request'] == 'Help with problem solving'
    
    @pytest.mark.asyncio
    async def test_broadcast_protocol(self, communication_protocols):
        """Test broadcast communication protocol."""
        protocols = communication_protocols
        
        # Register multiple agents
        agents = ['agent_001', 'agent_002', 'agent_003']
        for agent_id in agents:
            await protocols.register_agent(agent_id)
        
        # Send broadcast message
        broadcast_message = Message(
            message_id='broadcast_001',
            sender_id='agent_001',
            recipient_id='*',
            message_type=MessageType.NOTIFICATION,
            protocol_type=ProtocolType.BROADCAST,
            content={'announcement': 'Starting collaborative task'},
            priority=MessagePriority.NORMAL
        )
        
        success = await protocols.send_message(broadcast_message)
        assert success is True
        
        # All other agents should receive the broadcast
        for agent_id in ['agent_002', 'agent_003']:
            received = await protocols.receive_message(agent_id, timeout=1.0)
            assert received is not None
            assert received.content['announcement'] == 'Starting collaborative task'
    
    @pytest.mark.asyncio
    async def test_consensus_building_protocol(self, communication_protocols):
        """Test consensus building protocol."""
        protocols = communication_protocols
        
        # Register agents
        participants = ['agent_001', 'agent_002', 'agent_003']
        for agent_id in participants:
            await protocols.register_agent(agent_id)
        
        # Initiate consensus protocol
        session_id = await protocols.initiate_protocol(
            'agent_001',
            ProtocolType.CONSENSUS_BUILDING,
            participants,
            {
                'topic': 'Resource allocation strategy',
                'options': ['option_A', 'option_B'],
                'voting_deadline': time.time() + 300
            }
        )
        
        # Verify session was created
        assert session_id in protocols.active_sessions
        session = protocols.active_sessions[session_id]
        assert session.protocol_type == ProtocolType.CONSENSUS_BUILDING
        assert len(session.participants) == 3
        
        # Simulate participation
        success = await protocols.participate_in_protocol(
            'agent_002',
            session_id,
            'proposal',
            {'proposal': {'option': 'option_A', 'reasoning': 'Best efficiency'}}
        )
        assert success is True
    
    @pytest.mark.asyncio
    async def test_negotiation_protocol(self, communication_protocols):
        """Test negotiation protocol."""
        protocols = communication_protocols
        
        # Register agents
        negotiators = ['agent_001', 'agent_002']
        for agent_id in negotiators:
            await protocols.register_agent(agent_id)
        
        # Start negotiation
        session_id = await protocols.initiate_protocol(
            'agent_001',
            ProtocolType.NEGOTIATION,
            negotiators,
            {
                'subject': 'Task allocation agreement',
                'initial_terms': {'agent_001_tasks': 3, 'agent_002_tasks': 2}
            }
        )
        
        # Agent 001 makes offer
        success = await protocols.participate_in_protocol(
            'agent_001',
            session_id,
            'offer',
            {
                'terms': {'agent_001_tasks': 2, 'agent_002_tasks': 3, 'resources': 100},
                'validity': time.time() + 300
            }
        )
        assert success is True
        
        # Agent 002 responds with counteroffer
        success = await protocols.participate_in_protocol(
            'agent_002',
            session_id,
            'counteroffer',
            {
                'original_offer_id': 'placeholder',  # Would be actual offer ID
                'terms': {'agent_001_tasks': 2, 'agent_002_tasks': 3, 'resources': 120}
            }
        )
        assert success is True
        
        # Verify negotiation is progressing
        session = protocols.active_sessions[session_id]
        assert 'offers' in session.session_data
        assert len(session.session_data['offers']) >= 1


class TestCollaborativeProblemSolver:
    """Test collaborative problem solving functionality."""
    
    @pytest.fixture
    async def problem_solver(self):
        """Create collaborative problem solver for testing."""
        solver = CollaborativeProblemSolver(max_concurrent_problems=10)
        yield solver
        await solver.shutdown()
    
    @pytest.mark.asyncio
    async def test_problem_initiation_and_decomposition(self, problem_solver):
        """Test problem initiation and decomposition."""
        solver = problem_solver
        
        # Define problem
        problem = ProblemDefinition(
            problem_id='prob_001',
            problem_type=ProblemType.OPTIMIZATION,
            title='Resource Optimization',
            description='Optimize resource allocation across multiple constraints',
            objectives=['Maximize efficiency', 'Minimize cost'],
            constraints={'budget': 1000, 'time': 30},
            success_criteria={'efficiency': 0.8, 'cost_reduction': 0.2},
            complexity_level='high',
            required_capabilities=['optimization', 'analysis']
        )
        
        participating_agents = ['agent_001', 'agent_002', 'agent_003']
        
        # Initiate collaborative problem
        session_id = await solver.initiate_collaborative_problem(
            problem,
            participating_agents,
            'agent_001',  # coordinator
            SolutionStrategy.CONSENSUS
        )
        
        # Verify session was created
        assert session_id in solver.active_sessions
        session = solver.active_sessions[session_id]
        assert session.problem.title == 'Resource Optimization'
        assert session.problem.problem_type == ProblemType.OPTIMIZATION
        assert len(session.participating_agents) == 3
        assert session.coordinator_agent_id == 'agent_001'
        
        # Verify problem was decomposed into subtasks
        assert len(session.subtasks) > 0
        subtask_titles = [task.title for task in session.subtasks]
        # Should have optimization-specific subtasks
        assert any('Explore' in title for title in subtask_titles)
        assert any('Local' in title or 'Constraint' in title for title in subtask_titles)
    
    @pytest.mark.asyncio
    async def test_task_assignment(self, problem_solver):
        """Test task assignment to agents."""
        solver = problem_solver
        
        # Create problem with classification type (simpler decomposition)
        problem = ProblemDefinition(
            problem_id='prob_002',
            problem_type=ProblemType.CLASSIFICATION,
            title='Data Classification',
            description='Classify incoming data points',
            objectives=['High accuracy classification'],
            constraints={},
            success_criteria={'accuracy': 0.9}
        )
        
        agents = ['agent_001', 'agent_002']
        session_id = await solver.initiate_collaborative_problem(
            problem, agents, 'agent_001', SolutionStrategy.VOTING
        )
        
        # Define agent capabilities
        agent_capabilities = {
            'agent_001': ['data_processing', 'classification', 'validation'],
            'agent_002': ['feature_extraction', 'machine_learning', 'statistical_analysis']
        }
        
        # Assign tasks
        assignments = await solver.assign_tasks_to_agents(session_id, agent_capabilities)
        
        # Verify assignments
        assert isinstance(assignments, dict)
        assert 'agent_001' in assignments
        assert 'agent_002' in assignments
        
        # Check that tasks were assigned
        total_assigned = sum(len(task_list) for task_list in assignments.values())
        session = solver.active_sessions[session_id]
        assigned_tasks = [t for t in session.subtasks if t.assigned_agent_id is not None]
        assert len(assigned_tasks) == total_assigned
    
    @pytest.mark.asyncio
    async def test_solution_submission_and_synthesis(self, problem_solver):
        """Test solution submission and synthesis."""
        solver = problem_solver
        
        # Create simple search problem
        problem = ProblemDefinition(
            problem_id='prob_003',
            problem_type=ProblemType.SEARCH,
            title='Information Search',
            description='Search for relevant information',
            objectives=['Find best match'],
            constraints={},
            success_criteria={'relevance': 0.8},
            input_data={'search_space_size': 100}
        )
        
        agents = ['agent_001', 'agent_002']
        session_id = await solver.initiate_collaborative_problem(
            problem, agents, 'agent_001', SolutionStrategy.WEIGHTED_AVERAGE
        )
        
        session = solver.active_sessions[session_id]
        
        # Manually assign tasks (simplified for testing)
        if session.subtasks:
            for i, task in enumerate(session.subtasks[:2]):  # Assign first 2 tasks
                task.assigned_agent_id = agents[i % len(agents)]
                task.status = task.status.__class__.ASSIGNED
        
        # Submit solutions for assigned tasks
        assigned_tasks = [t for t in session.subtasks if t.assigned_agent_id is not None]
        
        for task in assigned_tasks:
            solution_data = {
                'results': [f'result_{task.task_id}'],
                'confidence': 0.8,
                'reasoning': f'Completed task {task.task_id} successfully'
            }
            
            success = await solver.submit_task_solution(
                session_id, task.task_id, task.assigned_agent_id, solution_data, 0.8
            )
            assert success is True
        
        # If all tasks are completed, should trigger synthesis
        # Check if session moved to completed (might still be active if not all tasks done)
        if session_id not in solver.active_sessions:
            # Session was completed and moved to completed_sessions
            assert len(solver.completed_sessions) > 0
            completed_session = solver.completed_sessions[-1]
            assert completed_session.session_id == session_id
            assert completed_session.final_solution is not None
    
    @pytest.mark.asyncio
    async def test_problem_status_tracking(self, problem_solver):
        """Test problem status tracking."""
        solver = problem_solver
        
        # Create problem
        problem = ProblemDefinition(
            problem_id='prob_004',
            problem_type=ProblemType.REASONING,
            title='Logical Reasoning',
            description='Solve logical reasoning problem',
            objectives=['Derive correct conclusion'],
            constraints={},
            success_criteria={'logical_consistency': True}
        )
        
        session_id = await solver.initiate_collaborative_problem(
            problem, ['agent_001', 'agent_002'], 'agent_001', SolutionStrategy.CONSENSUS
        )
        
        # Get problem status
        status = solver.get_problem_status(session_id)
        
        # Verify status information
        assert status is not None
        assert status['session_id'] == session_id
        assert status['problem_title'] == 'Logical Reasoning'
        assert status['problem_type'] == 'reasoning'
        assert status['status'] == 'active'
        assert len(status['participating_agents']) == 2
        assert 'progress' in status
        assert 'collaboration_metrics' in status
        
        # Check progress tracking
        progress = status['progress']
        assert 'overall_progress' in progress
        assert 'total_tasks' in progress
        assert 'completed_tasks' in progress
    
    @pytest.mark.asyncio
    async def test_solver_statistics(self, problem_solver):
        """Test solver statistics and metrics."""
        solver = problem_solver
        
        # Get initial stats
        stats = solver.get_solver_stats()
        
        # Verify stats structure
        assert 'solver_metrics' in stats
        assert 'active_sessions' in stats
        assert 'completed_sessions' in stats
        assert 'supported_problem_types' in stats
        assert 'supported_solution_strategies' in stats
        
        # Check solver metrics
        metrics = stats['solver_metrics']
        expected_metrics = [
            'problems_solved', 'problems_failed', 'avg_solution_time',
            'avg_agents_per_problem', 'task_success_rate', 
            'collaboration_efficiency', 'solution_quality_avg'
        ]
        for metric in expected_metrics:
            assert metric in metrics
        
        # Check supported types
        assert 'optimization' in stats['supported_problem_types']
        assert 'classification' in stats['supported_problem_types']
        assert 'consensus' in stats['supported_solution_strategies']
        assert 'voting' in stats['supported_solution_strategies']


@pytest.mark.asyncio
async def test_integration_social_cognition_components():
    """Test integration between social cognition components."""
    
    # Create all components
    social_manager = SocialCognitionManager(max_shared_resources=50)
    comm_protocols = CommunicationProtocols()
    problem_solver = CollaborativeProblemSolver(max_concurrent_problems=5)
    
    try:
        # Register agents in all systems
        agents = ['agent_001', 'agent_002', 'agent_003']
        
        for agent_id in agents:
            # Register for social cognition
            await social_manager.register_agent(agent_id, {
                'capabilities': {
                    'reasoning': True, 
                    'collaboration': True,
                    'optimization': True
                },
                'specializations': ['problem_solving'],
                'memory_capacity': 1000,
                'processing_bandwidth': 1.0
            })
            
            # Register for communication
            await comm_protocols.register_agent(agent_id)
        
        # 1. Start collaborative problem solving
        problem = ProblemDefinition(
            problem_id='integration_test',
            problem_type=ProblemType.OPTIMIZATION,
            title='Integration Test Problem',
            description='Test integration of social cognition components',
            objectives=['Test collaboration'],
            constraints={},
            success_criteria={'integration': True},
            required_capabilities=['reasoning', 'collaboration']
        )
        
        session_id = await problem_solver.initiate_collaborative_problem(
            problem, agents, 'agent_001', SolutionStrategy.CONSENSUS
        )
        
        # 2. Share problem context through social cognition
        problem_context = {
            'problem_id': problem.problem_id,
            'session_id': session_id,
            'current_phase': 'initialization',
            'shared_knowledge': {
                'domain_facts': ['fact1', 'fact2'],
                'constraints': problem.constraints
            }
        }
        
        resource_id = await social_manager.share_cognition(
            'agent_001',
            SharedCognitionType.WORKING_MEMORY,
            problem_context,
            CognitionSharingMode.POOLED,
            agents
        )
        
        # 3. Initiate communication protocol for coordination
        coord_session_id = await comm_protocols.initiate_protocol(
            'agent_001',
            ProtocolType.TASK_COORDINATION,
            agents,
            {
                'problem_session_id': session_id,
                'shared_resource_id': resource_id,
                'coordination_goal': 'Efficient task execution'
            }
        )
        
        # 4. Verify integration worked
        # Check problem solver has active session
        problem_status = problem_solver.get_problem_status(session_id)
        assert problem_status is not None
        assert problem_status['status'] == 'active'
        
        # Check social cognition has shared resource
        shared_resource = await social_manager.access_shared_cognition(
            'agent_002', resource_id, 'read'
        )
        assert shared_resource is not None
        assert shared_resource['data']['problem_id'] == problem.problem_id
        
        # Check communication protocol is active
        assert coord_session_id in comm_protocols.active_sessions
        comm_session = comm_protocols.active_sessions[coord_session_id]
        assert comm_session.protocol_type == ProtocolType.TASK_COORDINATION
        
        # 5. Test coordinated contribution through all systems
        # Agent 2 contributes to problem solving
        contribution = {
            'type': 'analysis',
            'content': {
                'optimization_approach': 'gradient_descent',
                'estimated_performance': 0.85
            },
            'confidence': 0.8
        }
        
        # Add contribution to social cognition collaboration
        collab_id = await social_manager.initiate_collaborative_problem_solving(
            'agent_002',
            {
                'title': 'Coordinate with problem solver',
                'description': 'Align with ongoing problem solving',
                'problem_solver_session': session_id
            },
            agents
        )
        
        success = await social_manager.contribute_to_collaboration(
            'agent_002', collab_id, contribution
        )
        assert success is True
        
        # Send coordination message
        coord_message = Message(
            message_id='coord_001',
            sender_id='agent_002',
            recipient_id='*',
            message_type=MessageType.COORDINATION,
            protocol_type=ProtocolType.TASK_COORDINATION,
            content={
                'contribution_made': True,
                'contribution_type': 'analysis',
                'next_steps': 'Await task assignment'
            },
            metadata={'session_id': coord_session_id}
        )
        
        comm_success = await comm_protocols.send_message(coord_message)
        assert comm_success is True
        
        # Verify integrated state
        social_stats = social_manager.get_social_cognition_stats()
        comm_stats = comm_protocols.get_communication_stats()
        solver_stats = problem_solver.get_solver_stats()
        
        # All systems should show activity
        assert social_stats['current_state']['active_collaborations'] > 0
        assert social_stats['current_state']['active_shared_resources'] > 0
        assert comm_stats['active_sessions']['total'] > 0
        assert solver_stats['active_sessions']['total'] > 0
        
        print("✓ Integration test passed - all social cognition components working together")
        
    finally:
        # Cleanup
        await social_manager.shutdown()
        await comm_protocols.shutdown()
        await problem_solver.shutdown()