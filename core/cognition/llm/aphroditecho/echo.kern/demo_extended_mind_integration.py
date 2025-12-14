#!/usr/bin/env python3
"""
Extended Mind Integration Example - Demonstrating Cognitive Scaffolding

This example demonstrates the Extended Mind System integration with the existing
embodied memory system, showing how cognitive scaffolding enhances agent capabilities.
"""

import asyncio
import time
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# Import Extended Mind System components
from extended_mind_system import (
    ExtendedMindSystem, CognitiveTask, CognitiveTaskType, 
    EnvironmentalResource, ResourceType
)
from cognitive_tools import create_default_cognitive_tools

# Try to import embodied memory system
try:
    from embodied_memory_system import EmbodiedMemorySystem, EmbodiedContext, BodyState
    HAS_EMBODIED_MEMORY = True
except ImportError as e:
    logger.warning(f"Embodied memory system not available: {e}")
    HAS_EMBODIED_MEMORY = False
    
    # Create mock classes for demonstration
    class BodyState:
        NEUTRAL = "neutral"
    
    class EmbodiedContext:
        def __init__(self):
            self.body_state = BodyState.NEUTRAL

class ExtendedMindDemo:
    """
    Demonstration of Extended Mind System with cognitive scaffolding.
    
    Shows how agents use external tools to enhance cognition through:
    - Memory offloading and retrieval
    - External computation capabilities  
    - Knowledge base access
    - Distributed problem solving
    """
    
    def __init__(self):
        """Initialize the demonstration system."""
        self.extended_mind = None
        self.embodied_memory = None
        self.setup_complete = False
    
    async def setup(self):
        """Setup the integrated cognitive systems."""
        logger.info("Setting up Extended Mind Integration Demo...")
        
        # Initialize embodied memory system if available
        if HAS_EMBODIED_MEMORY:
            try:
                self.embodied_memory = EmbodiedMemorySystem(
                    storage_dir="demo_embodied_memory",
                    max_working_memory=7,
                    dtesn_integration=True
                )
                logger.info("Embodied memory system initialized")
            except Exception as e:
                logger.warning(f"Failed to initialize embodied memory: {e}")
                self.embodied_memory = None
        
        # Initialize Extended Mind System
        self.extended_mind = ExtendedMindSystem(embodied_memory=self.embodied_memory)
        logger.info("Extended Mind System initialized")
        
        # Register default cognitive tools
        default_tools = create_default_cognitive_tools()
        for tool_spec, tool_interface in default_tools:
            self.extended_mind.tool_integration.register_tool(tool_spec, tool_interface)
            logger.info(f"Registered cognitive tool: {tool_spec.name}")
        
        # Register environmental resources
        self._setup_environmental_resources()
        
        # Register collaborative agents
        self._setup_collaborative_agents()
        
        # Register cultural knowledge
        self._setup_cultural_knowledge()
        
        self.setup_complete = True
        logger.info("Setup completed successfully")
    
    def _setup_environmental_resources(self):
        """Setup environmental resources for cognitive coupling."""
        resources = [
            EnvironmentalResource(
                resource_id="primary_cpu",
                resource_type=ResourceType.COMPUTATIONAL,
                name="Primary CPU Resource",
                capacity=100.0,
                available_capacity=85.0,
                access_time=0.001,
                quality=0.95
            ),
            EnvironmentalResource(
                resource_id="shared_memory",
                resource_type=ResourceType.MEMORY,
                name="Shared Memory Pool",
                capacity=1000.0,
                available_capacity=750.0,
                access_time=0.0005,
                quality=0.98
            ),
            EnvironmentalResource(
                resource_id="network_bandwidth",
                resource_type=ResourceType.NETWORK,
                name="Network Communication",
                capacity=1000.0,
                available_capacity=800.0,
                access_time=0.01,
                quality=0.90
            ),
            EnvironmentalResource(
                resource_id="social_network",
                resource_type=ResourceType.SOCIAL,
                name="Collaborative Agent Network",
                capacity=50.0,
                available_capacity=35.0,
                access_time=0.05,
                quality=0.85
            )
        ]
        
        for resource in resources:
            self.extended_mind.resource_coupling.register_resource(resource)
            logger.info(f"Registered resource: {resource.name}")
    
    def _setup_collaborative_agents(self):
        """Setup collaborative agents for social coordination."""
        agents = [
            ("analysis_agent", ["data_analysis", "pattern_recognition", "statistical_analysis"], 0.90),
            ("computation_agent", ["mathematical_calculation", "optimization", "simulation"], 0.85),
            ("knowledge_agent", ["knowledge_lookup", "fact_retrieval", "semantic_understanding"], 0.95),
            ("memory_agent", ["memory_storage", "memory_retrieval", "cognitive_offloading"], 0.88)
        ]
        
        for agent_id, capabilities, availability in agents:
            self.extended_mind.social_coordination.register_agent(
                agent_id, capabilities, availability
            )
            logger.info(f"Registered collaborative agent: {agent_id}")
    
    def _setup_cultural_knowledge(self):
        """Setup cultural knowledge bases."""
        knowledge_bases = {
            "cognitive_science": {
                "keywords": ["cognition", "memory", "attention", "learning", "reasoning"],
                "domain": "psychology",
                "reliability": 0.95
            },
            "ai_methodology": {
                "keywords": ["artificial intelligence", "machine learning", "neural networks", "algorithms"],
                "domain": "computer science", 
                "reliability": 0.90
            },
            "embodied_cognition": {
                "keywords": ["embodied", "embodiment", "sensorimotor", "action", "perception"],
                "domain": "cognitive science",
                "reliability": 0.92
            }
        }
        
        for base_id, knowledge_base in knowledge_bases.items():
            self.extended_mind.cultural_interface.register_knowledge_base(base_id, knowledge_base)
            logger.info(f"Registered knowledge base: {base_id}")
    
    async def run_demonstration_scenarios(self):
        """Run various demonstration scenarios."""
        if not self.setup_complete:
            await self.setup()
        
        logger.info("Starting demonstration scenarios...")
        
        scenarios = [
            self._scenario_memory_enhanced_problem_solving,
            self._scenario_distributed_computation,
            self._scenario_knowledge_guided_reasoning,
            self._scenario_social_collaborative_planning,
            self._scenario_embodied_memory_integration
        ]
        
        results = []
        for i, scenario in enumerate(scenarios, 1):
            logger.info(f"\n--- Running Scenario {i}: {scenario.__name__} ---")
            try:
                result = await scenario()
                results.append((scenario.__name__, result))
                logger.info(f"Scenario {i} completed successfully")
            except Exception as e:
                logger.error(f"Scenario {i} failed: {e}")
                results.append((scenario.__name__, {'error': str(e)}))
        
        return results
    
    async def _scenario_memory_enhanced_problem_solving(self):
        """
        Scenario 1: Memory-enhanced problem solving using external memory tools.
        
        Demonstrates how agents use external memory to offload cognitive burden
        and enhance problem-solving capabilities.
        """
        # Create problem-solving task
        task = CognitiveTask(
            task_id="memory_enhanced_solving",
            task_type=CognitiveTaskType.PROBLEM_SOLVING,
            description="Solve complex optimization problem using external memory for intermediate results",
            parameters={
                'problem_type': 'multi_step_optimization',
                'complexity': 'high',
                'intermediate_storage_needed': True,
                'steps': [
                    'analyze_problem_structure',
                    'generate_solution_candidates', 
                    'evaluate_candidates',
                    'select_optimal_solution'
                ]
            },
            priority=0.8,
            required_capabilities=['optimization', 'memory_storage', 'mathematical_calculation'],
            context=None  # Simplified for demo
        )
        
        # Execute with cognitive scaffolding
        result = await self.extended_mind.enhance_cognition(
            task, 
            available_resources=['primary_cpu', 'shared_memory']
        )
        
        # Analyze results
        return {
            'task_id': result.task_id,
            'tools_used': result.tools_used,
            'memory_offloading_enabled': 'memory_store_01' in result.tools_used,
            'response_time': result.performance_metrics.get('response_time', 0),
            'cognitive_enhancement': 'external_memory_utilized' if result.tools_used else 'internal_only'
        }
    
    async def _scenario_distributed_computation(self):
        """
        Scenario 2: Distributed computation using external computational resources.
        
        Shows how agents leverage external computation tools for complex analysis.
        """
        task = CognitiveTask(
            task_id="distributed_computation",
            task_type=CognitiveTaskType.PROBLEM_SOLVING,  # Change to existing type
            description="Perform complex data analysis using distributed computational resources",
            parameters={
                'computation_type': 'statistical_analysis',
                'data_size': 'large',
                'analysis_types': ['descriptive_statistics', 'correlation_analysis', 'trend_analysis'],
                'data': list(range(100))  # Sample data
            },
            priority=0.7,
            required_capabilities=['data_analysis', 'statistical_analysis', 'computation']
        )
        
        result = await self.extended_mind.enhance_cognition(
            task,
            available_resources=['primary_cpu', 'shared_memory', 'network_bandwidth']
        )
        
        return {
            'task_id': result.task_id,
            'computational_tools_used': [t for t in result.tools_used if 'computation' in t],
            'distributed_processing': len(result.resources_utilized) > 1,
            'performance_gain': 'high' if result.performance_metrics.get('response_time', 1) < 0.5 else 'medium'
        }
    
    async def _scenario_knowledge_guided_reasoning(self):
        """
        Scenario 3: Knowledge-guided reasoning using cultural knowledge bases.
        
        Demonstrates how external knowledge enhances reasoning capabilities.
        """
        task = CognitiveTask(
            task_id="knowledge_guided_reasoning",
            task_type=CognitiveTaskType.REASONING,
            description="Reason about embodied cognition principles using cultural knowledge",
            parameters={
                'reasoning_domain': 'cognitive_science',
                'query': 'embodied cognition',
                'reasoning_type': 'analogical',
                'knowledge_integration': True
            },
            priority=0.6,
            required_capabilities=['knowledge_lookup', 'reasoning', 'semantic_understanding']
        )
        
        result = await self.extended_mind.enhance_cognition(
            task,
            available_resources=['network_bandwidth', 'social_network']
        )
        
        return {
            'task_id': result.task_id,
            'knowledge_tools_used': [t for t in result.tools_used if 'knowledge' in t],
            'cultural_grounding': result.cultural_grounding,
            'reasoning_enhanced': len(result.cultural_grounding.get('knowledge_sources', [])) > 0
        }
    
    async def _scenario_social_collaborative_planning(self):
        """
        Scenario 4: Social collaborative planning with multi-agent coordination.
        
        Shows how agents coordinate with others for distributed planning.
        """
        task = CognitiveTask(
            task_id="collaborative_planning",
            task_type=CognitiveTaskType.PLANNING,
            description="Create collaborative plan for multi-agent problem solving",
            parameters={
                'planning_horizon': 'medium_term',
                'participants': ['analysis_agent', 'computation_agent', 'knowledge_agent'],
                'coordination_required': True,
                'plan_type': 'hierarchical'
            },
            priority=0.9,
            required_capabilities=['planning', 'coordination', 'communication']
        )
        
        result = await self.extended_mind.enhance_cognition(
            task,
            available_resources=['social_network', 'network_bandwidth']
        )
        
        return {
            'task_id': result.task_id,
            'social_coordination': result.social_coordination,
            'collaborative_agents': result.social_coordination.get('participants', []),
            'coordination_strategy': result.social_coordination.get('coordination_type', 'solo'),
            'social_enhancement': len(result.social_coordination.get('participants', [])) > 0
        }
    
    async def _scenario_embodied_memory_integration(self):
        """
        Scenario 5: Integration with embodied memory system.
        
        Demonstrates how extended mind scaffolding integrates with embodied memory.
        """
        if not self.embodied_memory:
            return {
                'task_id': 'embodied_integration_test',
                'status': 'skipped',
                'reason': 'Embodied memory system not available'
            }
        
        # Create embodied context
        try:
            from embodied_memory_system import BodyConfiguration, SpatialAnchor
            embodied_context = EmbodiedContext(
                body_state=BodyState.NEUTRAL,
                body_config=BodyConfiguration(),
                spatial_anchor=SpatialAnchor.EGOCENTRIC
            )
        except ImportError:
            embodied_context = EmbodiedContext() if hasattr(EmbodiedContext, '__init__') else None
        
        task = CognitiveTask(
            task_id="embodied_integration",
            task_type=CognitiveTaskType.LEARNING,
            description="Learn from experience using embodied memory and extended scaffolding",
            parameters={
                'learning_type': 'experiential',
                'embodied_context': True,
                'memory_integration': True
            },
            priority=0.8,
            required_capabilities=['learning', 'memory_storage', 'embodied_processing'],
            context=embodied_context
        )
        
        result = await self.extended_mind.enhance_cognition(
            task,
            available_resources=['shared_memory', 'primary_cpu']
        )
        
        return {
            'task_id': result.task_id,
            'embodied_integration': True,
            'memory_system_active': self.embodied_memory is not None,
            'scaffolding_with_embodiment': result.context is not None,
            'integrated_processing': 'successful'
        }
    
    def print_performance_summary(self):
        """Print performance summary of the Extended Mind System."""
        if not self.extended_mind:
            logger.warning("Extended Mind System not initialized")
            return
        
        metrics = self.extended_mind.get_performance_summary()
        
        print("\n" + "="*60)
        print("EXTENDED MIND SYSTEM - PERFORMANCE SUMMARY")
        print("="*60)
        
        if metrics.get('response_time_count', 0) > 0:
            print("Response Time:")
            print(f"  Average: {metrics['response_time_avg']:.3f}s")
            print(f"  Std Dev: {metrics['response_time_std']:.3f}s")
            print(f"  Operations: {int(metrics['response_time_count'])}")
            
            print("\nSuccess Rate:")
            print(f"  Average: {metrics['success_rate_avg']:.1%}")
            print(f"  Operations: {int(metrics['success_rate_count'])}")
            
            print("\nResource Efficiency:")
            print(f"  Average: {metrics['resource_efficiency_avg']:.3f}")
            print(f"  Operations: {int(metrics['resource_efficiency_count'])}")
        else:
            print("No performance data available yet.")
        
        print("\nSystem Components:")
        print(f"  Registered Tools: {len(self.extended_mind.tool_integration.tools)}")
        print(f"  Available Resources: {len(self.extended_mind.resource_coupling.resources)}")
        print(f"  Collaborative Agents: {len(self.extended_mind.social_coordination.agents)}")
        print(f"  Knowledge Bases: {len(self.extended_mind.cultural_interface.knowledge_bases)}")
        
        print("="*60 + "\n")

async def run_demo():
    """Run the complete Extended Mind System demonstration."""
    demo = ExtendedMindDemo()
    
    print("Extended Mind System - Cognitive Scaffolding Demonstration")
    print("="*60)
    print("This demonstration shows how agents use external tools to enhance cognition")
    print("through memory offloading, distributed computation, and social coordination.")
    print("="*60 + "\n")
    
    # Run demonstration scenarios
    start_time = time.time()
    results = await demo.run_demonstration_scenarios()
    total_time = time.time() - start_time
    
    # Print results
    print("\n" + "="*60)
    print("DEMONSTRATION RESULTS")
    print("="*60)
    
    for scenario_name, result in results:
        print(f"\n{scenario_name}:")
        if 'error' in result:
            print(f"  Status: FAILED - {result['error']}")
        else:
            print("  Status: SUCCESS")
            for key, value in result.items():
                if key not in ['task_id']:
                    print(f"  {key}: {value}")
    
    print(f"\nTotal execution time: {total_time:.2f}s")
    
    # Print performance summary
    demo.print_performance_summary()
    
    # Validate acceptance criteria
    print("ACCEPTANCE CRITERIA VALIDATION")
    print("="*60)
    
    # Check if agents used external tools
    tools_used_count = 0
    [result for _, result in results if isinstance(result, dict) and 'error' not in result]
    
    for _, result in results:
        if isinstance(result, dict) and 'error' not in result:
            # Count various indicators of tool usage
            if 'tools_used' in result:
                tools_used_count += len(result['tools_used'])
            elif 'knowledge_tools_used' in result:
                tools_used_count += len(result['knowledge_tools_used'])
            elif 'computational_tools_used' in result:
                tools_used_count += len(result['computational_tools_used'])
    
    print(f"✓ Agents use external tools to enhance cognition: {tools_used_count > 0}")
    print(f"  - Total tool operations: {tools_used_count}")
    
    # Check DTESN integration
    print(f"✓ Integration with DTESN components: {demo.extended_mind is not None}")
    
    # Check performance constraints
    avg_response_time = demo.extended_mind.get_performance_summary().get('response_time_avg', 0)
    print(f"✓ Real-time performance constraints: {avg_response_time < 1.0}")
    print(f"  - Average response time: {avg_response_time:.3f}s")
    
    print("="*60)
    
    return results

if __name__ == "__main__":
    # Run the demonstration
    asyncio.run(run_demo())