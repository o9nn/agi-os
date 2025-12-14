"""
Collaborative Problem Solver

Implements distributed problem-solving capabilities that enable multiple agents
to work together on complex problems through task decomposition, parallel execution,
and solution synthesis.

This component supports the distributed problem solving requirements for Task 2.3.2
of the Deep Tree Echo development roadmap.
"""

import asyncio
import logging
import time
import uuid
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field
from enum import Enum
import math

logger = logging.getLogger(__name__)


class ProblemType(Enum):
    """Types of problems that can be solved collaboratively."""
    OPTIMIZATION = "optimization"
    CLASSIFICATION = "classification"
    PLANNING = "planning"
    SEARCH = "search"
    REASONING = "reasoning"
    CREATIVE_SYNTHESIS = "creative_synthesis"
    DATA_ANALYSIS = "data_analysis"
    SIMULATION = "simulation"


class TaskStatus(Enum):
    """Status of individual tasks in problem solving."""
    PENDING = "pending"
    ASSIGNED = "assigned"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class SolutionStrategy(Enum):
    """Strategies for combining solutions from multiple agents."""
    VOTING = "voting"
    WEIGHTED_AVERAGE = "weighted_average"
    CONSENSUS = "consensus"
    HIERARCHICAL = "hierarchical"
    COMPETITIVE = "competitive"
    HYBRID = "hybrid"


@dataclass
class ProblemDefinition:
    """Definition of a problem to be solved collaboratively."""
    problem_id: str
    problem_type: ProblemType
    title: str
    description: str
    objectives: List[str]
    constraints: Dict[str, Any]
    success_criteria: Dict[str, Any]
    complexity_level: str = "medium"  # low, medium, high, very_high
    estimated_duration: float = 0.0
    required_capabilities: List[str] = field(default_factory=list)
    input_data: Dict[str, Any] = field(default_factory=dict)
    context: Dict[str, Any] = field(default_factory=dict)


@dataclass
class SubTask:
    """Individual sub-task within a collaborative problem."""
    task_id: str
    parent_problem_id: str
    title: str
    description: str
    assigned_agent_id: Optional[str] = None
    status: TaskStatus = TaskStatus.PENDING
    priority: int = 1  # 1 = highest, 5 = lowest
    estimated_effort: float = 1.0
    dependencies: List[str] = field(default_factory=list)  # Other task IDs this depends on
    required_capabilities: List[str] = field(default_factory=list)
    input_data: Dict[str, Any] = field(default_factory=dict)
    output_data: Dict[str, Any] = field(default_factory=dict)
    progress: float = 0.0
    created_at: float = field(default_factory=time.time)
    started_at: Optional[float] = None
    completed_at: Optional[float] = None


@dataclass
class Solution:
    """Solution or partial solution from an agent."""
    solution_id: str
    problem_id: str
    agent_id: str
    solution_data: Dict[str, Any]
    confidence: float
    quality_score: float = 0.0
    reasoning: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)
    created_at: float = field(default_factory=time.time)


@dataclass
class CollaborativeProblemSession:
    """Active collaborative problem-solving session."""
    session_id: str
    problem: ProblemDefinition
    participating_agents: List[str]
    coordinator_agent_id: str
    strategy: SolutionStrategy
    status: str = "active"
    subtasks: List[SubTask] = field(default_factory=list)
    solutions: List[Solution] = field(default_factory=list)
    final_solution: Optional[Dict[str, Any]] = None
    created_at: float = field(default_factory=time.time)
    started_at: Optional[float] = None
    completed_at: Optional[float] = None
    collaboration_metrics: Dict[str, Any] = field(default_factory=dict)


class CollaborativeProblemSolver:
    """Manages distributed problem-solving across multiple agents."""
    
    def __init__(self, max_concurrent_problems: int = 100):
        self.max_concurrent_problems = max_concurrent_problems
        
        # Active problem sessions
        self.active_sessions: Dict[str, CollaborativeProblemSession] = {}
        self.completed_sessions: List[CollaborativeProblemSession] = []
        
        # Task execution
        self.task_queue: asyncio.Queue = asyncio.Queue()
        self.task_executors: Dict[str, asyncio.Task] = {}  # agent_id -> executor task
        
        # Solution synthesis strategies
        self.synthesis_strategies: Dict[SolutionStrategy, Callable] = {
            SolutionStrategy.VOTING: self._synthesize_by_voting,
            SolutionStrategy.WEIGHTED_AVERAGE: self._synthesize_by_weighted_average,
            SolutionStrategy.CONSENSUS: self._synthesize_by_consensus,
            SolutionStrategy.HIERARCHICAL: self._synthesize_hierarchical,
            SolutionStrategy.COMPETITIVE: self._synthesize_competitive,
            SolutionStrategy.HYBRID: self._synthesize_hybrid
        }
        
        # Performance metrics
        self.solver_metrics = {
            'problems_solved': 0,
            'problems_failed': 0,
            'avg_solution_time': 0.0,
            'avg_agents_per_problem': 0.0,
            'task_success_rate': 0.0,
            'collaboration_efficiency': 0.0,
            'solution_quality_avg': 0.0
        }
        
        # Configuration
        self.config = {
            'max_subtasks_per_problem': 50,
            'task_timeout': 300.0,  # 5 minutes
            'problem_timeout': 1800.0,  # 30 minutes
            'min_solution_confidence': 0.3,
            'quality_threshold': 0.7,
            'max_task_retries': 2
        }
        
        logger.info(f"Collaborative Problem Solver initialized with capacity: {max_concurrent_problems}")
    
    async def initiate_collaborative_problem(self,
                                           problem: ProblemDefinition,
                                           participating_agents: List[str],
                                           coordinator_agent_id: str,
                                           strategy: SolutionStrategy = SolutionStrategy.CONSENSUS) -> str:
        """Initiate a collaborative problem-solving session."""
        
        if len(self.active_sessions) >= self.max_concurrent_problems:
            raise RuntimeError(f"Maximum concurrent problems ({self.max_concurrent_problems}) reached")
        
        session_id = f"prob_{uuid.uuid4().hex[:8]}"
        
        # Create problem session
        session = CollaborativeProblemSession(
            session_id=session_id,
            problem=problem,
            participating_agents=participating_agents,
            coordinator_agent_id=coordinator_agent_id,
            strategy=strategy
        )
        
        # Initialize collaboration metrics
        session.collaboration_metrics = {
            'start_time': time.time(),
            'agent_contributions': {agent_id: 0 for agent_id in participating_agents},
            'task_assignments': {},
            'communication_events': 0,
            'solution_iterations': 0,
            'quality_improvements': 0
        }
        
        self.active_sessions[session_id] = session
        
        # Decompose problem into subtasks
        await self._decompose_problem(session)
        
        # Start problem-solving process
        session.started_at = time.time()
        
        logger.info(f"Initiated collaborative problem '{problem.title}' with {len(participating_agents)} agents")
        return session_id
    
    async def _decompose_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose problem into manageable subtasks."""
        problem = session.problem
        
        # Determine decomposition strategy based on problem type
        if problem.problem_type == ProblemType.OPTIMIZATION:
            await self._decompose_optimization_problem(session)
        elif problem.problem_type == ProblemType.CLASSIFICATION:
            await self._decompose_classification_problem(session)
        elif problem.problem_type == ProblemType.PLANNING:
            await self._decompose_planning_problem(session)
        elif problem.problem_type == ProblemType.SEARCH:
            await self._decompose_search_problem(session)
        elif problem.problem_type == ProblemType.REASONING:
            await self._decompose_reasoning_problem(session)
        elif problem.problem_type == ProblemType.CREATIVE_SYNTHESIS:
            await self._decompose_creative_problem(session)
        elif problem.problem_type == ProblemType.DATA_ANALYSIS:
            await self._decompose_data_analysis_problem(session)
        elif problem.problem_type == ProblemType.SIMULATION:
            await self._decompose_simulation_problem(session)
        else:
            # Generic decomposition
            await self._decompose_generic_problem(session)
        
        logger.info(f"Decomposed problem {problem.problem_id} into {len(session.subtasks)} subtasks")
    
    async def _decompose_optimization_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose optimization problem into subtasks."""
        problem = session.problem
        
        # Create subtasks for optimization
        subtasks = [
            SubTask(
                task_id=f"opt_explore_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Explore Solution Space",
                description="Explore different regions of the solution space",
                required_capabilities=["optimization", "exploration"],
                priority=1
            ),
            SubTask(
                task_id=f"opt_local_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Local Optimization",
                description="Perform local optimization from different starting points",
                required_capabilities=["optimization", "local_search"],
                priority=2
            ),
            SubTask(
                task_id=f"opt_constraints_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Constraint Analysis",
                description="Analyze and validate solution constraints",
                required_capabilities=["constraint_handling", "validation"],
                priority=1
            ),
            SubTask(
                task_id=f"opt_evaluate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Solution Evaluation",
                description="Evaluate and compare candidate solutions",
                required_capabilities=["evaluation", "comparison"],
                priority=3,
                dependencies=[f"opt_explore_{uuid.uuid4().hex[:6]}", f"opt_local_{uuid.uuid4().hex[:6]}"]
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_classification_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose classification problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"cls_preprocess_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Data Preprocessing",
                description="Clean and preprocess input data",
                required_capabilities=["data_processing", "feature_engineering"],
                priority=1
            ),
            SubTask(
                task_id=f"cls_features_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Feature Extraction",
                description="Extract relevant features for classification",
                required_capabilities=["feature_extraction", "pattern_recognition"],
                priority=2
            ),
            SubTask(
                task_id=f"cls_classify_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Classification",
                description="Perform classification using different approaches",
                required_capabilities=["classification", "machine_learning"],
                priority=3
            ),
            SubTask(
                task_id=f"cls_validate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Result Validation",
                description="Validate classification results",
                required_capabilities=["validation", "statistical_analysis"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_planning_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose planning problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"plan_analyze_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Situation Analysis",
                description="Analyze current situation and goals",
                required_capabilities=["analysis", "goal_setting"],
                priority=1
            ),
            SubTask(
                task_id=f"plan_generate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Plan Generation",
                description="Generate possible action plans",
                required_capabilities=["planning", "creative_thinking"],
                priority=2
            ),
            SubTask(
                task_id=f"plan_evaluate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Plan Evaluation",
                description="Evaluate feasibility and effectiveness of plans",
                required_capabilities=["evaluation", "risk_assessment"],
                priority=3
            ),
            SubTask(
                task_id=f"plan_optimize_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Plan Optimization",
                description="Optimize selected plan for better outcomes",
                required_capabilities=["optimization", "refinement"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_search_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose search problem into subtasks."""
        problem = session.problem
        search_space_size = problem.input_data.get('search_space_size', 1000)
        num_agents = len(session.participating_agents)
        
        # Divide search space among agents
        partition_size = math.ceil(search_space_size / num_agents)
        
        for i in range(num_agents):
            start_pos = i * partition_size
            end_pos = min(start_pos + partition_size, search_space_size)
            
            subtask = SubTask(
                task_id=f"search_partition_{i}_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title=f"Search Partition {i+1}",
                description=f"Search partition {start_pos} to {end_pos}",
                required_capabilities=["search", "exploration"],
                input_data={
                    'search_start': start_pos,
                    'search_end': end_pos,
                    'search_criteria': problem.input_data.get('search_criteria', {})
                },
                priority=1
            )
            session.subtasks.append(subtask)
        
        # Add result aggregation task
        aggregation_task = SubTask(
            task_id=f"search_aggregate_{uuid.uuid4().hex[:6]}",
            parent_problem_id=problem.problem_id,
            title="Aggregate Search Results",
            description="Combine and rank search results from all partitions",
            required_capabilities=["aggregation", "ranking"],
            priority=2,
            dependencies=[st.task_id for st in session.subtasks]
        )
        session.subtasks.append(aggregation_task)
    
    async def _decompose_reasoning_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose reasoning problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"reason_facts_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Fact Gathering",
                description="Gather and verify relevant facts",
                required_capabilities=["fact_checking", "knowledge_retrieval"],
                priority=1
            ),
            SubTask(
                task_id=f"reason_infer_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Inference Generation",
                description="Generate logical inferences from facts",
                required_capabilities=["logical_reasoning", "inference"],
                priority=2
            ),
            SubTask(
                task_id=f"reason_validate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Reasoning Validation",
                description="Validate reasoning chains and conclusions",
                required_capabilities=["validation", "logical_analysis"],
                priority=3
            ),
            SubTask(
                task_id=f"reason_synthesize_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Conclusion Synthesis",
                description="Synthesize final conclusions from validated reasoning",
                required_capabilities=["synthesis", "conclusion_drawing"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_creative_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose creative synthesis problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"create_inspire_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Inspiration Gathering",
                description="Gather diverse sources of inspiration",
                required_capabilities=["creativity", "inspiration_gathering"],
                priority=1
            ),
            SubTask(
                task_id=f"create_ideate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Ideation",
                description="Generate creative ideas and concepts",
                required_capabilities=["ideation", "creative_thinking"],
                priority=2
            ),
            SubTask(
                task_id=f"create_combine_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Concept Combination",
                description="Combine ideas in novel ways",
                required_capabilities=["synthesis", "combination"],
                priority=3
            ),
            SubTask(
                task_id=f"create_refine_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Creative Refinement",
                description="Refine and polish creative solutions",
                required_capabilities=["refinement", "creative_evaluation"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_data_analysis_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose data analysis problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"data_clean_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Data Cleaning",
                description="Clean and validate input data",
                required_capabilities=["data_cleaning", "validation"],
                priority=1
            ),
            SubTask(
                task_id=f"data_explore_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Exploratory Analysis",
                description="Perform exploratory data analysis",
                required_capabilities=["data_exploration", "statistical_analysis"],
                priority=2
            ),
            SubTask(
                task_id=f"data_model_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Modeling",
                description="Build analytical models",
                required_capabilities=["modeling", "statistical_methods"],
                priority=3
            ),
            SubTask(
                task_id=f"data_interpret_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Result Interpretation",
                description="Interpret analysis results",
                required_capabilities=["interpretation", "insight_generation"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_simulation_problem(self, session: CollaborativeProblemSession) -> None:
        """Decompose simulation problem into subtasks."""
        problem = session.problem
        
        subtasks = [
            SubTask(
                task_id=f"sim_setup_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Simulation Setup",
                description="Set up simulation parameters and environment",
                required_capabilities=["simulation", "parameter_setting"],
                priority=1
            ),
            SubTask(
                task_id=f"sim_run_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Simulation Execution",
                description="Execute simulation runs",
                required_capabilities=["simulation_execution", "monitoring"],
                priority=2
            ),
            SubTask(
                task_id=f"sim_analyze_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Result Analysis",
                description="Analyze simulation results",
                required_capabilities=["result_analysis", "statistical_analysis"],
                priority=3
            ),
            SubTask(
                task_id=f"sim_validate_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title="Simulation Validation",
                description="Validate simulation accuracy and reliability",
                required_capabilities=["validation", "verification"],
                priority=4
            )
        ]
        
        session.subtasks.extend(subtasks)
    
    async def _decompose_generic_problem(self, session: CollaborativeProblemSession) -> None:
        """Generic problem decomposition fallback."""
        problem = session.problem
        num_agents = len(session.participating_agents)
        
        # Create parallel subtasks for generic problem
        for i in range(min(num_agents, 4)):  # Max 4 parallel subtasks
            subtask = SubTask(
                task_id=f"generic_{i}_{uuid.uuid4().hex[:6]}",
                parent_problem_id=problem.problem_id,
                title=f"Problem Component {i+1}",
                description=f"Work on component {i+1} of the problem",
                required_capabilities=problem.required_capabilities,
                priority=1
            )
            session.subtasks.append(subtask)
        
        # Add synthesis task
        synthesis_task = SubTask(
            task_id=f"generic_synthesis_{uuid.uuid4().hex[:6]}",
            parent_problem_id=problem.problem_id,
            title="Solution Synthesis",
            description="Synthesize solutions from all components",
            required_capabilities=["synthesis", "integration"],
            priority=2,
            dependencies=[st.task_id for st in session.subtasks]
        )
        session.subtasks.append(synthesis_task)
    
    async def assign_tasks_to_agents(self, session_id: str, agent_capabilities: Dict[str, List[str]]) -> Dict[str, List[str]]:
        """Assign subtasks to agents based on capabilities and availability."""
        
        if session_id not in self.active_sessions:
            raise ValueError(f"Session {session_id} not found")
        
        session = self.active_sessions[session_id]
        assignments = {agent_id: [] for agent_id in session.participating_agents}
        
        # Sort tasks by priority and dependencies
        available_tasks = [task for task in session.subtasks if task.status == TaskStatus.PENDING]
        available_tasks.sort(key=lambda t: (t.priority, len(t.dependencies)))
        
        for task in available_tasks:
            # Check if dependencies are satisfied
            if not await self._are_dependencies_satisfied(session, task):
                continue
            
            # Find best agent for this task
            best_agent = await self._find_best_agent_for_task(task, agent_capabilities, assignments)
            
            if best_agent:
                task.assigned_agent_id = best_agent
                task.status = TaskStatus.ASSIGNED
                assignments[best_agent].append(task.task_id)
                
                # Update session metrics
                session.collaboration_metrics['task_assignments'][task.task_id] = {
                    'agent_id': best_agent,
                    'assigned_at': time.time()
                }
        
        logger.info(f"Assigned {sum(len(tasks) for tasks in assignments.values())} tasks in session {session_id}")
        return assignments
    
    async def _are_dependencies_satisfied(self, session: CollaborativeProblemSession, task: SubTask) -> bool:
        """Check if task dependencies are satisfied."""
        if not task.dependencies:
            return True
        
        for dep_task_id in task.dependencies:
            dep_task = next((t for t in session.subtasks if t.task_id == dep_task_id), None)
            if not dep_task or dep_task.status != TaskStatus.COMPLETED:
                return False
        
        return True
    
    async def _find_best_agent_for_task(self, 
                                      task: SubTask, 
                                      agent_capabilities: Dict[str, List[str]], 
                                      current_assignments: Dict[str, List[str]]) -> Optional[str]:
        """Find the best agent for a specific task."""
        
        best_agent = None
        best_score = -1.0
        
        for agent_id, capabilities in agent_capabilities.items():
            # Calculate capability match score
            capability_score = 0.0
            if task.required_capabilities:
                matches = sum(1 for cap in task.required_capabilities if cap in capabilities)
                capability_score = matches / len(task.required_capabilities)
            else:
                capability_score = 0.5  # Neutral score if no specific requirements
            
            # Calculate workload balance score (prefer less loaded agents)
            current_workload = len(current_assignments.get(agent_id, []))
            max_workload = max(len(assignments) for assignments in current_assignments.values()) or 1
            workload_score = 1.0 - (current_workload / max_workload)
            
            # Combined score
            total_score = (capability_score * 0.7) + (workload_score * 0.3)
            
            if total_score > best_score and capability_score > 0.3:  # Minimum capability threshold
                best_score = total_score
                best_agent = agent_id
        
        return best_agent
    
    async def submit_task_solution(self, 
                                 session_id: str, 
                                 task_id: str, 
                                 agent_id: str, 
                                 solution_data: Dict[str, Any],
                                 confidence: float = 0.8) -> bool:
        """Submit solution for a subtask."""
        
        if session_id not in self.active_sessions:
            logger.warning(f"Session {session_id} not found")
            return False
        
        session = self.active_sessions[session_id]
        
        # Find the task
        task = next((t for t in session.subtasks if t.task_id == task_id), None)
        if not task:
            logger.warning(f"Task {task_id} not found in session {session_id}")
            return False
        
        # Verify agent assignment
        if task.assigned_agent_id != agent_id:
            logger.warning(f"Agent {agent_id} not assigned to task {task_id}")
            return False
        
        # Update task with solution
        task.output_data = solution_data.copy()
        task.status = TaskStatus.COMPLETED
        task.completed_at = time.time()
        task.progress = 1.0
        
        # Create solution record
        solution = Solution(
            solution_id=f"sol_{uuid.uuid4().hex[:8]}",
            problem_id=session.problem.problem_id,
            agent_id=agent_id,
            solution_data=solution_data,
            confidence=confidence,
            reasoning=solution_data.get('reasoning', ''),
            metadata={
                'task_id': task_id,
                'session_id': session_id,
                'completion_time': task.completed_at - (task.started_at or task.created_at)
            }
        )
        
        session.solutions.append(solution)
        
        # Update collaboration metrics
        session.collaboration_metrics['agent_contributions'][agent_id] += 1
        session.collaboration_metrics['solution_iterations'] += 1
        
        logger.info(f"Agent {agent_id} submitted solution for task {task_id} in session {session_id}")
        
        # Check if all tasks are completed
        await self._check_problem_completion(session)
        
        return True
    
    async def update_task_progress(self, 
                                 session_id: str, 
                                 task_id: str, 
                                 agent_id: str, 
                                 progress: float, 
                                 status_update: str = "") -> bool:
        """Update progress on a task."""
        
        if session_id not in self.active_sessions:
            return False
        
        session = self.active_sessions[session_id]
        
        # Find the task
        task = next((t for t in session.subtasks if t.task_id == task_id), None)
        if not task or task.assigned_agent_id != agent_id:
            return False
        
        # Update task progress
        task.progress = max(0.0, min(1.0, progress))
        
        if task.status == TaskStatus.ASSIGNED and progress > 0:
            task.status = TaskStatus.IN_PROGRESS
            task.started_at = time.time()
        
        logger.debug(f"Updated task {task_id} progress: {progress:.1%}")
        return True
    
    async def _check_problem_completion(self, session: CollaborativeProblemSession) -> None:
        """Check if problem is ready for solution synthesis."""
        
        # Check if all tasks are completed
        completed_tasks = [t for t in session.subtasks if t.status == TaskStatus.COMPLETED]
        total_tasks = len(session.subtasks)
        
        if len(completed_tasks) == total_tasks:
            # All tasks completed, synthesize final solution
            await self._synthesize_final_solution(session)
    
    async def _synthesize_final_solution(self, session: CollaborativeProblemSession) -> None:
        """Synthesize final solution from all subtask solutions."""
        
        logger.info(f"Synthesizing final solution for problem {session.problem.problem_id}")
        
        # Get synthesis strategy
        synthesis_function = self.synthesis_strategies.get(session.strategy)
        if not synthesis_function:
            synthesis_function = self.synthesis_strategies[SolutionStrategy.CONSENSUS]
        
        # Synthesize solution
        final_solution = await synthesis_function(session)
        
        # Update session with final solution
        session.final_solution = final_solution
        session.status = 'completed'
        session.completed_at = time.time()
        
        # Calculate final metrics
        duration = session.completed_at - (session.started_at or session.created_at)
        session.collaboration_metrics.update({
            'total_duration': duration,
            'tasks_completed': len([t for t in session.subtasks if t.status == TaskStatus.COMPLETED]),
            'average_task_duration': duration / max(len(session.subtasks), 1),
            'final_solution_confidence': final_solution.get('confidence', 0.0),
            'final_solution_quality': final_solution.get('quality_score', 0.0)
        })
        
        # Move to completed sessions
        self.completed_sessions.append(session)
        del self.active_sessions[session.session_id]
        
        # Update solver metrics
        self.solver_metrics['problems_solved'] += 1
        self._update_solver_metrics(session)
        
        logger.info(f"Completed collaborative problem {session.problem.problem_id} in {duration:.2f}s")
    
    async def _synthesize_by_voting(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using voting mechanism."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Simple majority voting on solution elements
        solution_votes = {}
        
        for solution in session.solutions:
            solution_key = str(solution.solution_data.get('main_result', solution.solution_id))
            if solution_key not in solution_votes:
                solution_votes[solution_key] = {'count': 0, 'total_confidence': 0.0, 'solutions': []}
            
            solution_votes[solution_key]['count'] += 1
            solution_votes[solution_key]['total_confidence'] += solution.confidence
            solution_votes[solution_key]['solutions'].append(solution)
        
        # Find winning solution
        winner = max(solution_votes.items(), key=lambda x: x[1]['count'])
        winning_solutions = winner[1]['solutions']
        
        return {
            'synthesis_method': 'voting',
            'final_result': winning_solutions[0].solution_data,
            'confidence': winner[1]['total_confidence'] / winner[1]['count'],
            'quality_score': sum(s.quality_score for s in winning_solutions) / len(winning_solutions),
            'vote_count': winner[1]['count'],
            'contributing_agents': [s.agent_id for s in winning_solutions],
            'reasoning': f"Selected by majority vote ({winner[1]['count']} votes)"
        }
    
    async def _synthesize_by_weighted_average(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using weighted average of solutions."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Weight by confidence and quality
        total_weight = 0.0
        weighted_result = {}
        
        for solution in session.solutions:
            weight = (solution.confidence + solution.quality_score) / 2.0
            total_weight += weight
            
            # Combine numerical results (if available)
            for key, value in solution.solution_data.items():
                if isinstance(value, (int, float)):
                    if key not in weighted_result:
                        weighted_result[key] = 0.0
                    weighted_result[key] += value * weight
        
        # Normalize by total weight
        if total_weight > 0:
            for key in weighted_result:
                weighted_result[key] /= total_weight
        
        avg_confidence = sum(s.confidence for s in session.solutions) / len(session.solutions)
        avg_quality = sum(s.quality_score for s in session.solutions) / len(session.solutions)
        
        return {
            'synthesis_method': 'weighted_average',
            'final_result': weighted_result,
            'confidence': avg_confidence,
            'quality_score': avg_quality,
            'contributing_agents': [s.agent_id for s in session.solutions],
            'reasoning': f"Weighted average of {len(session.solutions)} solutions"
        }
    
    async def _synthesize_by_consensus(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using consensus mechanism."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Find areas of agreement among solutions
        consensus_elements = {}
        solution_count = len(session.solutions)
        threshold = solution_count * 0.6  # 60% agreement threshold
        
        # Analyze each solution element
        all_keys = set()
        for solution in session.solutions:
            all_keys.update(solution.solution_data.keys())
        
        for key in all_keys:
            values = []
            for solution in session.solutions:
                if key in solution.solution_data:
                    values.append(solution.solution_data[key])
            
            # Check for consensus on this element
            if len(values) >= threshold:
                if all(isinstance(v, (int, float)) for v in values):
                    # Numerical consensus - use average
                    consensus_elements[key] = sum(values) / len(values)
                elif len(set(str(v) for v in values)) == 1:
                    # Exact match consensus
                    consensus_elements[key] = values[0]
                else:
                    # Partial consensus - use most common
                    value_counts = {}
                    for v in values:
                        v_str = str(v)
                        value_counts[v_str] = value_counts.get(v_str, 0) + 1
                    
                    most_common = max(value_counts.items(), key=lambda x: x[1])
                    if most_common[1] >= threshold:
                        consensus_elements[key] = values[0]  # Use first occurrence of most common
        
        avg_confidence = sum(s.confidence for s in session.solutions) / len(session.solutions)
        avg_quality = sum(s.quality_score for s in session.solutions) / len(session.solutions)
        
        return {
            'synthesis_method': 'consensus',
            'final_result': consensus_elements,
            'confidence': avg_confidence,
            'quality_score': avg_quality,
            'consensus_level': len(consensus_elements) / max(len(all_keys), 1),
            'contributing_agents': [s.agent_id for s in session.solutions],
            'reasoning': f"Consensus from {len(session.solutions)} solutions with {len(consensus_elements)} agreed elements"
        }
    
    async def _synthesize_hierarchical(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using hierarchical approach (coordinator decides)."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Find solution from coordinator agent, or best solution if no coordinator solution
        coordinator_solution = None
        for solution in session.solutions:
            if solution.agent_id == session.coordinator_agent_id:
                coordinator_solution = solution
                break
        
        if not coordinator_solution:
            # Use highest quality solution
            coordinator_solution = max(session.solutions, key=lambda s: s.quality_score)
        
        return {
            'synthesis_method': 'hierarchical',
            'final_result': coordinator_solution.solution_data,
            'confidence': coordinator_solution.confidence,
            'quality_score': coordinator_solution.quality_score,
            'selected_agent': coordinator_solution.agent_id,
            'contributing_agents': [s.agent_id for s in session.solutions],
            'reasoning': f"Selected solution from coordinator/best agent {coordinator_solution.agent_id}"
        }
    
    async def _synthesize_competitive(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using competitive selection (best solution wins)."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Select solution with highest combined score
        best_solution = max(session.solutions, 
                           key=lambda s: (s.confidence + s.quality_score) / 2.0)
        
        return {
            'synthesis_method': 'competitive',
            'final_result': best_solution.solution_data,
            'confidence': best_solution.confidence,
            'quality_score': best_solution.quality_score,
            'winning_agent': best_solution.agent_id,
            'contributing_agents': [s.agent_id for s in session.solutions],
            'reasoning': f"Best solution selected from {len(session.solutions)} candidates"
        }
    
    async def _synthesize_hybrid(self, session: CollaborativeProblemSession) -> Dict[str, Any]:
        """Synthesize solution using hybrid approach combining multiple strategies."""
        if not session.solutions:
            return {'error': 'No solutions to synthesize'}
        
        # Use consensus for areas of agreement, competitive for disagreements
        consensus_result = await self._synthesize_by_consensus(session)
        competitive_result = await self._synthesize_competitive(session)
        
        # Combine results
        final_result = consensus_result['final_result'].copy()
        
        # Add competitive elements where no consensus
        for key, value in competitive_result['final_result'].items():
            if key not in final_result:
                final_result[key] = value
        
        avg_confidence = (consensus_result['confidence'] + competitive_result['confidence']) / 2.0
        avg_quality = (consensus_result['quality_score'] + competitive_result['quality_score']) / 2.0
        
        return {
            'synthesis_method': 'hybrid',
            'final_result': final_result,
            'confidence': avg_confidence,
            'quality_score': avg_quality,
            'consensus_elements': len(consensus_result['final_result']),
            'competitive_elements': len(competitive_result['final_result']) - len(consensus_result['final_result']),
            'contributing_agents': [s.agent_id for s in session.solutions],
            'reasoning': "Hybrid approach: consensus + competitive selection"
        }
    
    def _update_solver_metrics(self, session: CollaborativeProblemSession) -> None:
        """Update solver performance metrics."""
        duration = session.collaboration_metrics.get('total_duration', 0.0)
        agents_count = len(session.participating_agents)
        final_quality = session.collaboration_metrics.get('final_solution_quality', 0.0)
        
        # Update averages
        total_problems = self.solver_metrics['problems_solved'] + self.solver_metrics['problems_failed']
        
        if total_problems > 0:
            # Update average solution time
            current_avg_time = self.solver_metrics['avg_solution_time']
            new_avg_time = ((current_avg_time * (total_problems - 1)) + duration) / total_problems
            self.solver_metrics['avg_solution_time'] = new_avg_time
            
            # Update average agents per problem
            current_avg_agents = self.solver_metrics['avg_agents_per_problem']
            new_avg_agents = ((current_avg_agents * (total_problems - 1)) + agents_count) / total_problems
            self.solver_metrics['avg_agents_per_problem'] = new_avg_agents
            
            # Update average solution quality
            current_avg_quality = self.solver_metrics['solution_quality_avg']
            new_avg_quality = ((current_avg_quality * (total_problems - 1)) + final_quality) / total_problems
            self.solver_metrics['solution_quality_avg'] = new_avg_quality
            
            # Calculate task success rate
            total_tasks = len(session.subtasks)
            completed_tasks = len([t for t in session.subtasks if t.status == TaskStatus.COMPLETED])
            session_success_rate = completed_tasks / max(total_tasks, 1)
            
            current_task_success = self.solver_metrics['task_success_rate']
            new_task_success = ((current_task_success * (total_problems - 1)) + session_success_rate) / total_problems
            self.solver_metrics['task_success_rate'] = new_task_success
    
    def get_problem_status(self, session_id: str) -> Optional[Dict[str, Any]]:
        """Get status of a specific problem-solving session."""
        session = self.active_sessions.get(session_id)
        if not session:
            return None
        
        # Calculate progress
        total_tasks = len(session.subtasks)
        completed_tasks = len([t for t in session.subtasks if t.status == TaskStatus.COMPLETED])
        in_progress_tasks = len([t for t in session.subtasks if t.status == TaskStatus.IN_PROGRESS])
        
        overall_progress = 0.0
        if total_tasks > 0:
            progress_sum = sum(t.progress for t in session.subtasks)
            overall_progress = progress_sum / total_tasks
        
        return {
            'session_id': session_id,
            'problem_title': session.problem.title,
            'problem_type': session.problem.problem_type.value,
            'status': session.status,
            'participating_agents': session.participating_agents,
            'coordinator': session.coordinator_agent_id,
            'strategy': session.strategy.value,
            'progress': {
                'overall_progress': overall_progress,
                'total_tasks': total_tasks,
                'completed_tasks': completed_tasks,
                'in_progress_tasks': in_progress_tasks,
                'pending_tasks': total_tasks - completed_tasks - in_progress_tasks
            },
            'solutions_received': len(session.solutions),
            'duration': time.time() - (session.started_at or session.created_at),
            'collaboration_metrics': session.collaboration_metrics
        }
    
    def get_solver_stats(self) -> Dict[str, Any]:
        """Get comprehensive solver statistics."""
        active_problems_by_type = {}
        for session in self.active_sessions.values():
            problem_type = session.problem.problem_type.value
            active_problems_by_type[problem_type] = active_problems_by_type.get(problem_type, 0) + 1
        
        return {
            'solver_metrics': self.solver_metrics,
            'active_sessions': {
                'total': len(self.active_sessions),
                'by_type': active_problems_by_type
            },
            'completed_sessions': len(self.completed_sessions),
            'supported_problem_types': [pt.value for pt in ProblemType],
            'supported_solution_strategies': [ss.value for ss in SolutionStrategy],
            'configuration': self.config
        }
    
    async def shutdown(self) -> None:
        """Gracefully shutdown collaborative problem solver."""
        logger.info("Shutting down Collaborative Problem Solver...")
        
        # Complete any active sessions
        active_session_ids = list(self.active_sessions.keys())
        for session_id in active_session_ids:
            session = self.active_sessions[session_id]
            session.status = 'terminated'
            logger.warning(f"Terminated active problem session {session_id}")
        
        # Clear data structures
        self.active_sessions.clear()
        self.completed_sessions.clear()
        
        # Stop task executors
        for executor in self.task_executors.values():
            executor.cancel()
        self.task_executors.clear()
        
        logger.info("Collaborative Problem Solver shutdown complete")