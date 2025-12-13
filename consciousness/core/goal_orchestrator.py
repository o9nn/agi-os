"""
Goal Orchestrator - Iteration N+5

Implements active goal pursuit:
1. Breaks goals into actionable steps
2. Schedules goal-related activities in cognitive loop
3. Tracks progress and adjusts strategies
4. Completes or abandons goals based on outcomes
5. Learns from goal pursuit experiences

This transforms goals from passive declarations to active drivers of behavior.
"""

import asyncio
import time
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime, timedelta
import random


class GoalStatus(Enum):
    """Goal status states."""
    ACTIVE = "Active"
    PAUSED = "Paused"
    COMPLETED = "Completed"
    ABANDONED = "Abandoned"


class StepStatus(Enum):
    """Goal step status states."""
    PENDING = "Pending"
    IN_PROGRESS = "InProgress"
    COMPLETED = "Completed"
    FAILED = "Failed"


@dataclass
class GoalStep:
    """Represents an actionable step toward a goal."""
    id: str
    goal_id: str
    description: str
    status: StepStatus
    priority: float
    estimated_effort: float  # 0.0 to 1.0
    actual_effort: float = 0.0
    required_skills: List[str] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)  # Other step IDs
    created: datetime = field(default_factory=datetime.now)
    started: Optional[datetime] = None
    completed: Optional[datetime] = None
    attempts: int = 0
    max_attempts: int = 3


@dataclass
class GoalPursuitSession:
    """Represents a session of working on a goal."""
    id: str
    goal_id: str
    step_id: str
    start_time: float
    end_time: float
    effort_applied: float
    progress_made: float
    skills_practiced: List[str]
    insights_gained: List[str]
    success: bool


class GoalOrchestrator:
    """
    Goal orchestrator for active goal pursuit.
    
    Responsibilities:
    - Decompose goals into actionable steps
    - Schedule goal work in cognitive cycles
    - Track progress and update goal status
    - Adjust strategies based on outcomes
    - Learn from goal pursuit experiences
    - Coordinate with skill system for development
    """
    
    def __init__(self, goal_system=None, skill_system=None, memory_system=None):
        """
        Initialize goal orchestrator.
        
        Args:
            goal_system: Goal management system
            skill_system: Skill development system
            memory_system: Memory system for learning
        """
        self.goal_system = goal_system
        self.skill_system = skill_system
        self.memory_system = memory_system
        
        # Goal pursuit state
        self.active = False
        self.current_goal: Optional[Any] = None
        self.current_step: Optional[GoalStep] = None
        self.goal_steps: Dict[str, List[GoalStep]] = {}  # goal_id -> steps
        self.pursuit_sessions: List[GoalPursuitSession] = []
        
        # Scheduling
        self.time_budget_per_cycle = 10.0  # seconds per cognitive cycle
        self.min_session_duration = 5.0  # minimum seconds per session
        
        # Statistics
        self.total_sessions = 0
        self.total_steps_completed = 0
        self.total_goals_completed = 0
        self.success_rate = 0.0
        
    async def start(self):
        """Start goal orchestration."""
        if self.active:
            print("⚠️  Goal orchestrator already active")
            return
            
        self.active = True
        print("🎯 ═══════════════════════════════════════════════════════")
        print("🎯 Goal Orchestrator: Starting")
        print("🎯 ═══════════════════════════════════════════════════════")
        print("🎯 Active goal pursuit enabled")
        print("🎯 Breaking goals into actionable steps")
        print("🎯 ═══════════════════════════════════════════════════════\n")
        
        # Decompose existing goals
        await self._decompose_all_goals()
        
    async def stop(self):
        """Stop goal orchestration."""
        if not self.active:
            return
            
        self.active = False
        print("\n🎯 Stopping Goal Orchestrator...")
        self._print_summary()
        
    async def pursue_goals(self, duration: float) -> Dict[str, Any]:
        """
        Pursue active goals for a specified duration.
        
        Args:
            duration: Time to spend on goal pursuit
            
        Returns:
            Results of goal pursuit session
        """
        if not self.active:
            return {}
            
        start_time = time.time()
        results = {
            'sessions': 0,
            'progress': 0.0,
            'steps_completed': 0,
            'skills_practiced': []
        }
        
        while time.time() - start_time < duration:
            # Select next goal and step
            goal, step = await self._select_next_work()
            
            if not goal or not step:
                # No work available
                break
                
            # Work on the step
            session = await self._work_on_step(goal, step, self.min_session_duration)
            
            if session:
                results['sessions'] += 1
                results['progress'] += session.progress_made
                results['skills_practiced'].extend(session.skills_practiced)
                
                if session.success and step.status == StepStatus.COMPLETED:
                    results['steps_completed'] += 1
                    
                    # Check if goal is complete
                    if await self._check_goal_completion(goal):
                        self.total_goals_completed += 1
                        print(f"🎯 ✅ Goal completed: {goal.description}")
        
        return results
        
    async def _decompose_all_goals(self):
        """Decompose all active goals into steps."""
        if not self.goal_system:
            return
            
        goals = self._get_active_goals()
        
        for goal in goals:
            if goal.id not in self.goal_steps:
                steps = await self._decompose_goal(goal)
                self.goal_steps[goal.id] = steps
                print(f"🎯 Decomposed goal '{goal.description}' into {len(steps)} steps")
                
    async def _decompose_goal(self, goal: Any) -> List[GoalStep]:
        """
        Decompose a goal into actionable steps.
        
        This is a critical function that breaks down high-level goals
        into concrete, executable steps.
        """
        steps = []
        
        # Analyze goal to determine steps
        # This is a simplified version - could use LLM for richer decomposition
        
        # Step 1: Identify knowledge gaps
        if hasattr(goal, 'knowledge_gaps') and goal.knowledge_gaps:
            for i, gap in enumerate(goal.knowledge_gaps):
                step = GoalStep(
                    id=f"{goal.id}_step_kg_{i}",
                    goal_id=goal.id,
                    description=f"Learn about: {gap}",
                    status=StepStatus.PENDING,
                    priority=0.8,
                    estimated_effort=0.6,
                    required_skills=["research", "learning"]
                )
                steps.append(step)
        
        # Step 2: Develop required skills
        if hasattr(goal, 'required_skills') and goal.required_skills:
            for i, skill_name in enumerate(goal.required_skills):
                # Check current proficiency
                current_prof = self._get_skill_proficiency(skill_name)
                
                if current_prof < 0.7:  # Needs improvement
                    step = GoalStep(
                        id=f"{goal.id}_step_skill_{i}",
                        goal_id=goal.id,
                        description=f"Practice skill: {skill_name}",
                        status=StepStatus.PENDING,
                        priority=0.7,
                        estimated_effort=0.8,
                        required_skills=[skill_name]
                    )
                    steps.append(step)
        
        # Step 3: Execute main goal activities
        # Generate 2-3 execution steps based on goal description
        execution_steps = self._generate_execution_steps(goal)
        steps.extend(execution_steps)
        
        # Step 4: Validation and reflection
        validation_step = GoalStep(
            id=f"{goal.id}_step_validate",
            goal_id=goal.id,
            description=f"Validate achievement of: {goal.description}",
            status=StepStatus.PENDING,
            priority=0.9,
            estimated_effort=0.3,
            required_skills=["reflection", "evaluation"],
            dependencies=[s.id for s in steps]  # Depends on all previous steps
        )
        steps.append(validation_step)
        
        return steps
        
    def _generate_execution_steps(self, goal: Any) -> List[GoalStep]:
        """Generate execution steps for a goal."""
        steps = []
        
        # Simple heuristic: create 2-3 execution steps
        descriptions = [
            f"Begin work on: {goal.description}",
            f"Make progress on: {goal.description}",
            f"Complete core work on: {goal.description}"
        ]
        
        for i, desc in enumerate(descriptions):
            step = GoalStep(
                id=f"{goal.id}_step_exec_{i}",
                goal_id=goal.id,
                description=desc,
                status=StepStatus.PENDING,
                priority=0.6 + (i * 0.1),
                estimated_effort=0.7,
                required_skills=getattr(goal, 'required_skills', [])
            )
            steps.append(step)
        
        return steps
        
    async def _select_next_work(self) -> Tuple[Optional[Any], Optional[GoalStep]]:
        """
        Select the next goal and step to work on.
        
        Uses priority-based selection with consideration of:
        - Goal priority
        - Step priority
        - Skill readiness
        - Dependencies
        """
        goals = self._get_active_goals()
        
        if not goals:
            return None, None
        
        # Sort goals by priority
        goals = sorted(goals, key=lambda g: g.priority, reverse=True)
        
        # Find the highest priority available step
        best_goal = None
        best_step = None
        best_score = -1.0
        
        for goal in goals:
            if goal.id not in self.goal_steps:
                continue
                
            steps = self.goal_steps[goal.id]
            available_steps = [
                s for s in steps 
                if s.status in [StepStatus.PENDING, StepStatus.IN_PROGRESS]
                and self._dependencies_met(s, steps)
                and s.attempts < s.max_attempts
            ]
            
            for step in available_steps:
                # Calculate selection score
                score = self._calculate_step_score(goal, step)
                
                if score > best_score:
                    best_score = score
                    best_goal = goal
                    best_step = step
        
        return best_goal, best_step
        
    def _dependencies_met(self, step: GoalStep, all_steps: List[GoalStep]) -> bool:
        """Check if step dependencies are met."""
        if not step.dependencies:
            return True
            
        for dep_id in step.dependencies:
            dep_step = next((s for s in all_steps if s.id == dep_id), None)
            if not dep_step or dep_step.status != StepStatus.COMPLETED:
                return False
        
        return True
        
    def _calculate_step_score(self, goal: Any, step: GoalStep) -> float:
        """Calculate priority score for a step."""
        score = 0.0
        
        # Base: goal priority * step priority
        score += goal.priority * step.priority
        
        # Bonus for steps in progress (continuity)
        if step.status == StepStatus.IN_PROGRESS:
            score += 0.2
        
        # Penalty for high effort steps (prefer quick wins)
        score -= step.estimated_effort * 0.1
        
        # Bonus for skill readiness
        skill_readiness = self._calculate_skill_readiness(step)
        score += skill_readiness * 0.3
        
        # Penalty for previous failures
        score -= step.attempts * 0.1
        
        return score
        
    def _calculate_skill_readiness(self, step: GoalStep) -> float:
        """Calculate how ready we are skill-wise for a step."""
        if not step.required_skills:
            return 1.0
            
        total_proficiency = 0.0
        for skill_name in step.required_skills:
            prof = self._get_skill_proficiency(skill_name)
            total_proficiency += prof
        
        return total_proficiency / len(step.required_skills)
        
    async def _work_on_step(self, goal: Any, step: GoalStep, duration: float) -> Optional[GoalPursuitSession]:
        """
        Work on a specific goal step.
        
        This simulates cognitive effort applied to the step.
        """
        session_id = f"session_{self.total_sessions}"
        start_time = time.time()
        
        # Mark step as in progress
        if step.status == StepStatus.PENDING:
            step.status = StepStatus.IN_PROGRESS
            step.started = datetime.now()
        
        step.attempts += 1
        
        print(f"🎯 Working on: {step.description}")
        
        # Simulate work
        await asyncio.sleep(min(duration, 2.0))  # Simulate effort
        
        # Calculate progress based on skill proficiency and effort
        skill_readiness = self._calculate_skill_readiness(step)
        effort_applied = min(duration / 10.0, 1.0)  # Normalize effort
        
        # Progress is a function of skill and effort
        progress_made = skill_readiness * effort_applied * random.uniform(0.7, 1.0)
        
        # Update step
        step.actual_effort += effort_applied
        
        # Check if step is complete
        success = False
        if step.actual_effort >= step.estimated_effort * 0.8:  # 80% threshold
            step.status = StepStatus.COMPLETED
            step.completed = datetime.now()
            success = True
            self.total_steps_completed += 1
            print(f"🎯 ✅ Step completed: {step.description}")
        elif step.attempts >= step.max_attempts:
            step.status = StepStatus.FAILED
            print(f"🎯 ❌ Step failed after {step.attempts} attempts: {step.description}")
        
        # Practice skills
        skills_practiced = []
        if self.skill_system and step.required_skills:
            for skill_name in step.required_skills:
                self._practice_skill(skill_name, progress_made)
                skills_practiced.append(skill_name)
        
        # Create session record
        session = GoalPursuitSession(
            id=session_id,
            goal_id=goal.id,
            step_id=step.id,
            start_time=start_time,
            end_time=time.time(),
            effort_applied=effort_applied,
            progress_made=progress_made,
            skills_practiced=skills_practiced,
            insights_gained=[],
            success=success
        )
        
        self.pursuit_sessions.append(session)
        self.total_sessions += 1
        
        # Update success rate
        successful_sessions = sum(1 for s in self.pursuit_sessions if s.success)
        self.success_rate = successful_sessions / len(self.pursuit_sessions)
        
        # Update goal progress
        if hasattr(goal, 'progress'):
            goal.progress = self._calculate_goal_progress(goal)
        
        return session
        
    async def _check_goal_completion(self, goal: Any) -> bool:
        """Check if a goal is complete."""
        if goal.id not in self.goal_steps:
            return False
            
        steps = self.goal_steps[goal.id]
        
        # Goal is complete if all steps are completed
        all_complete = all(s.status == StepStatus.COMPLETED for s in steps)
        
        if all_complete:
            if hasattr(goal, 'status'):
                goal.status = GoalStatus.COMPLETED
            if hasattr(goal, 'progress'):
                goal.progress = 1.0
            return True
        
        return False
        
    def _calculate_goal_progress(self, goal: Any) -> float:
        """Calculate overall progress on a goal."""
        if goal.id not in self.goal_steps:
            return 0.0
            
        steps = self.goal_steps[goal.id]
        if not steps:
            return 0.0
        
        completed = sum(1 for s in steps if s.status == StepStatus.COMPLETED)
        return completed / len(steps)
        
    def _get_active_goals(self) -> List[Any]:
        """Get all active goals."""
        if not self.goal_system:
            return []
            
        goals = getattr(self.goal_system, 'goals', [])
        return [g for g in goals if getattr(g, 'status', None) == GoalStatus.ACTIVE]
        
    def _get_skill_proficiency(self, skill_name: str) -> float:
        """Get current proficiency for a skill."""
        if not self.skill_system:
            return 0.5  # Default moderate proficiency
            
        skills = getattr(self.skill_system, 'skills', [])
        for skill in skills:
            if skill.name == skill_name:
                return skill.proficiency
        
        return 0.3  # Default low proficiency for unknown skills
        
    def _practice_skill(self, skill_name: str, amount: float):
        """Practice a skill (increase proficiency)."""
        if not self.skill_system:
            return
            
        skills = getattr(self.skill_system, 'skills', [])
        for skill in skills:
            if skill.name == skill_name:
                # Increase proficiency with diminishing returns
                improvement = amount * 0.05 * (1.0 - skill.proficiency)
                skill.proficiency = min(1.0, skill.proficiency + improvement)
                skill.practice_count += 1
                skill.last_practiced = datetime.now()
                return
        
        # Skill not found - could create it here
        
    def _print_summary(self):
        """Print goal orchestrator summary."""
        print("\n" + "="*60)
        print("🎯 Goal Orchestrator Summary")
        print("="*60)
        print(f"Total pursuit sessions: {self.total_sessions}")
        print(f"Steps completed: {self.total_steps_completed}")
        print(f"Goals completed: {self.total_goals_completed}")
        print(f"Success rate: {self.success_rate*100:.1f}%")
        
        # Show active goals
        active_goals = self._get_active_goals()
        print(f"\nActive goals: {len(active_goals)}")
        for goal in active_goals[:5]:  # Show top 5
            progress = self._calculate_goal_progress(goal)
            print(f"  - {goal.description} ({progress*100:.0f}% complete)")
        
        print("="*60)
