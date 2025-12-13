"""
Skill Practice System - Iteration N+5

Implements active skill development:
1. Schedules practice sessions based on proficiency and need
2. Executes practice activities with feedback
3. Tracks improvement over time
4. Applies learned skills in appropriate contexts
5. Identifies skill gaps and recommends practice

This enables the system to deliberately improve its capabilities.
"""

import asyncio
import time
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime, timedelta
import random
import math


class PracticeType(Enum):
    """Types of practice activities."""
    DRILL = "drill"  # Repetitive practice
    APPLICATION = "application"  # Apply in context
    CHALLENGE = "challenge"  # Stretch practice
    REVIEW = "review"  # Review and consolidate


@dataclass
class PracticeSession:
    """Represents a skill practice session."""
    id: str
    skill_name: str
    practice_type: PracticeType
    start_time: float
    end_time: float
    duration: float
    initial_proficiency: float
    final_proficiency: float
    improvement: float
    quality: float  # 0.0 to 1.0
    focus: float  # 0.0 to 1.0
    success_rate: float  # 0.0 to 1.0
    exercises_completed: int
    insights: List[str] = field(default_factory=list)


@dataclass
class SkillGap:
    """Represents an identified skill gap."""
    skill_name: str
    current_proficiency: float
    target_proficiency: float
    priority: float
    identified_by: str  # goal, curiosity, etc.
    practice_plan: List[str] = field(default_factory=list)


class SkillPracticeSystem:
    """
    Skill practice system for active skill development.
    
    Responsibilities:
    - Schedule practice sessions based on skill needs
    - Execute practice activities with varied approaches
    - Track proficiency improvements over time
    - Identify skill gaps and recommend practice
    - Apply skills in appropriate contexts
    - Learn effective practice strategies
    """
    
    def __init__(self, skill_system=None, memory_system=None):
        """
        Initialize skill practice system.
        
        Args:
            skill_system: Skill tracking system
            memory_system: Memory system for learning
        """
        self.skill_system = skill_system
        self.memory_system = memory_system
        
        # Practice state
        self.active = False
        self.practice_sessions: List[PracticeSession] = []
        self.skill_gaps: List[SkillGap] = []
        
        # Scheduling parameters
        self.practice_interval_hours = 2.0  # Practice every 2 hours
        self.session_duration_min = 5.0  # Minimum session duration
        self.session_duration_max = 15.0  # Maximum session duration
        
        # Practice effectiveness
        self.practice_strategies: Dict[str, float] = {
            PracticeType.DRILL: 0.7,
            PracticeType.APPLICATION: 0.8,
            PracticeType.CHALLENGE: 0.9,
            PracticeType.REVIEW: 0.6
        }
        
        # Statistics
        self.total_sessions = 0
        self.total_practice_time = 0.0
        self.average_improvement = 0.0
        
    async def start(self):
        """Start skill practice system."""
        if self.active:
            print("⚠️  Skill practice system already active")
            return
            
        self.active = True
        print("📚 ═══════════════════════════════════════════════════════")
        print("📚 Skill Practice System: Starting")
        print("📚 ═══════════════════════════════════════════════════════")
        print("📚 Active skill development enabled")
        print("📚 Scheduling practice sessions based on proficiency")
        print("📚 ═══════════════════════════════════════════════════════\n")
        
        # Identify initial skill gaps
        await self._identify_skill_gaps()
        
    async def stop(self):
        """Stop skill practice system."""
        if not self.active:
            return
            
        self.active = False
        print("\n📚 Stopping Skill Practice System...")
        self._print_summary()
        
    async def practice_skills(self, duration: float) -> Dict[str, Any]:
        """
        Practice skills for a specified duration.
        
        Args:
            duration: Time to spend on skill practice
            
        Returns:
            Results of practice session
        """
        if not self.active:
            return {}
            
        start_time = time.time()
        results = {
            'sessions': 0,
            'skills_practiced': [],
            'total_improvement': 0.0,
            'insights': []
        }
        
        while time.time() - start_time < duration:
            # Select skill to practice
            skill = await self._select_skill_to_practice()
            
            if not skill:
                break
            
            # Determine practice type
            practice_type = self._select_practice_type(skill)
            
            # Execute practice session
            session_duration = min(
                self.session_duration_max,
                duration - (time.time() - start_time)
            )
            
            if session_duration < self.session_duration_min:
                break
            
            session = await self._execute_practice_session(
                skill, practice_type, session_duration
            )
            
            if session:
                results['sessions'] += 1
                results['skills_practiced'].append(skill.name)
                results['total_improvement'] += session.improvement
                results['insights'].extend(session.insights)
        
        return results
        
    async def _identify_skill_gaps(self):
        """Identify skills that need practice."""
        if not self.skill_system:
            return
            
        skills = getattr(self.skill_system, 'skills', [])
        self.skill_gaps = []
        
        for skill in skills:
            # Identify gap if proficiency is below target
            target = 0.8  # Default target proficiency
            
            if skill.proficiency < target:
                gap = SkillGap(
                    skill_name=skill.name,
                    current_proficiency=skill.proficiency,
                    target_proficiency=target,
                    priority=self._calculate_gap_priority(skill, target),
                    identified_by="system_analysis"
                )
                self.skill_gaps.append(gap)
        
        if self.skill_gaps:
            print(f"📚 Identified {len(self.skill_gaps)} skill gaps")
            for gap in sorted(self.skill_gaps, key=lambda g: g.priority, reverse=True)[:3]:
                print(f"   - {gap.skill_name}: {gap.current_proficiency:.2f} → {gap.target_proficiency:.2f}")
                
    def _calculate_gap_priority(self, skill: Any, target: float) -> float:
        """Calculate priority for closing a skill gap."""
        # Larger gaps have higher priority
        gap_size = target - skill.proficiency
        
        # Skills used more frequently have higher priority
        usage_factor = min(1.0, skill.practice_count / 10.0)
        
        # Recently used skills have higher priority
        recency_factor = 0.5
        if skill.last_practiced:
            hours_since = (datetime.now() - skill.last_practiced).total_seconds() / 3600
            recency_factor = 1.0 / (1.0 + hours_since / 24.0)  # Decay over days
        
        priority = gap_size * (0.5 + usage_factor * 0.3 + recency_factor * 0.2)
        return min(1.0, priority)
        
    async def _select_skill_to_practice(self) -> Optional[Any]:
        """
        Select the next skill to practice.
        
        Uses a combination of:
        - Skill gaps (priority)
        - Time since last practice
        - Skill importance
        """
        if not self.skill_system:
            return None
            
        skills = getattr(self.skill_system, 'skills', [])
        
        if not skills:
            return None
        
        # Calculate practice scores
        skill_scores = []
        for skill in skills:
            score = self._calculate_practice_score(skill)
            skill_scores.append((skill, score))
        
        # Sort by score
        skill_scores.sort(key=lambda x: x[1], reverse=True)
        
        # Select top skill
        if skill_scores:
            selected_skill = skill_scores[0][0]
            return selected_skill
        
        return None
        
    def _calculate_practice_score(self, skill: Any) -> float:
        """Calculate priority score for practicing a skill."""
        score = 0.0
        
        # Gap factor: lower proficiency = higher score
        gap_factor = 1.0 - skill.proficiency
        score += gap_factor * 0.4
        
        # Recency factor: longer since practice = higher score
        recency_factor = 0.5
        if skill.last_practiced:
            hours_since = (datetime.now() - skill.last_practiced).total_seconds() / 3600
            recency_factor = min(1.0, hours_since / self.practice_interval_hours)
        else:
            recency_factor = 1.0  # Never practiced
        
        score += recency_factor * 0.3
        
        # Usage factor: more used = higher score
        usage_factor = min(1.0, skill.practice_count / 20.0)
        score += usage_factor * 0.2
        
        # Category importance
        category_importance = self._get_category_importance(skill.category)
        score += category_importance * 0.1
        
        return score
        
    def _get_category_importance(self, category: str) -> float:
        """Get importance weight for a skill category."""
        importance_map = {
            "cognitive": 0.9,
            "learning": 0.9,
            "reasoning": 0.8,
            "memory": 0.8,
            "communication": 0.7,
            "creativity": 0.7,
            "technical": 0.6
        }
        return importance_map.get(category.lower(), 0.5)
        
    def _select_practice_type(self, skill: Any) -> PracticeType:
        """Select appropriate practice type for a skill."""
        # Select based on proficiency level
        if skill.proficiency < 0.3:
            # Novice: focus on drills
            return random.choice([PracticeType.DRILL, PracticeType.DRILL, PracticeType.APPLICATION])
        elif skill.proficiency < 0.7:
            # Intermediate: mix of application and challenge
            return random.choice([PracticeType.APPLICATION, PracticeType.APPLICATION, PracticeType.CHALLENGE])
        else:
            # Expert: challenge and review
            return random.choice([PracticeType.CHALLENGE, PracticeType.REVIEW])
            
    async def _execute_practice_session(
        self, 
        skill: Any, 
        practice_type: PracticeType, 
        duration: float
    ) -> Optional[PracticeSession]:
        """
        Execute a practice session for a skill.
        
        This simulates focused practice with feedback and improvement.
        """
        session_id = f"practice_{self.total_sessions}"
        start_time = time.time()
        initial_proficiency = skill.proficiency
        
        print(f"📚 Practicing {skill.name} ({practice_type.value})")
        
        # Simulate practice
        await asyncio.sleep(min(duration, 2.0))
        
        # Calculate practice quality based on focus and strategy
        focus = random.uniform(0.7, 1.0)  # Simulated focus level
        strategy_effectiveness = self.practice_strategies[practice_type]
        
        # Calculate improvement
        base_improvement = 0.02  # Base improvement per session
        
        # Factors affecting improvement
        proficiency_factor = 1.0 - skill.proficiency  # Easier to improve at lower levels
        focus_factor = focus
        strategy_factor = strategy_effectiveness
        duration_factor = min(1.0, duration / 10.0)
        
        improvement = base_improvement * proficiency_factor * focus_factor * strategy_factor * duration_factor
        
        # Add some randomness
        improvement *= random.uniform(0.8, 1.2)
        
        # Apply improvement
        skill.proficiency = min(1.0, skill.proficiency + improvement)
        skill.practice_count += 1
        skill.last_practiced = datetime.now()
        
        # Calculate success rate (simulated)
        success_rate = skill.proficiency * focus
        
        # Generate insights
        insights = self._generate_practice_insights(skill, practice_type, improvement)
        
        # Create session record
        session = PracticeSession(
            id=session_id,
            skill_name=skill.name,
            practice_type=practice_type,
            start_time=start_time,
            end_time=time.time(),
            duration=duration,
            initial_proficiency=initial_proficiency,
            final_proficiency=skill.proficiency,
            improvement=improvement,
            quality=strategy_effectiveness * focus,
            focus=focus,
            success_rate=success_rate,
            exercises_completed=int(duration / 2.0),  # Rough estimate
            insights=insights
        )
        
        self.practice_sessions.append(session)
        self.total_sessions += 1
        self.total_practice_time += duration
        
        # Update average improvement
        total_improvement = sum(s.improvement for s in self.practice_sessions)
        self.average_improvement = total_improvement / len(self.practice_sessions)
        
        # Print progress
        tier_before = self._get_proficiency_tier(initial_proficiency)
        tier_after = self._get_proficiency_tier(skill.proficiency)
        
        if tier_after != tier_before:
            print(f"📚 ⬆️  {skill.name} advanced to {tier_after}!")
        else:
            print(f"📚 ✓ {skill.name}: {initial_proficiency:.3f} → {skill.proficiency:.3f} (+{improvement:.3f})")
        
        # Store practice memory
        if self.memory_system:
            self._store_practice_memory(session, skill)
        
        return session
        
    def _get_proficiency_tier(self, proficiency: float) -> str:
        """Get proficiency tier name."""
        if proficiency < 0.3:
            return "novice"
        elif proficiency < 0.7:
            return "intermediate"
        else:
            return "expert"
            
    def _generate_practice_insights(
        self, 
        skill: Any, 
        practice_type: PracticeType, 
        improvement: float
    ) -> List[str]:
        """Generate insights from practice session."""
        insights = []
        
        # Insight based on improvement
        if improvement > 0.03:
            insights.append(f"Excellent progress in {skill.name} - this practice approach is very effective")
        elif improvement < 0.01:
            insights.append(f"Slow progress in {skill.name} - may need different practice approach")
        
        # Insight based on proficiency level
        if skill.proficiency > 0.8:
            insights.append(f"Approaching mastery in {skill.name} - ready for advanced applications")
        elif skill.proficiency > 0.5 and skill.proficiency < 0.7:
            insights.append(f"Solid foundation in {skill.name} - ready for more challenging practice")
        
        # Insight based on practice type
        if practice_type == PracticeType.CHALLENGE and improvement > 0.02:
            insights.append(f"Challenge practice is accelerating growth in {skill.name}")
        
        return insights
        
    def _store_practice_memory(self, session: PracticeSession, skill: Any):
        """Store practice session as procedural memory."""
        if not self.memory_system or not hasattr(self.memory_system, 'add_node'):
            return
            
        try:
            from demo_autonomous_echoself_v5 import MemoryType
            
            content = f"Practiced {skill.name} ({session.practice_type.value}): {session.initial_proficiency:.2f} → {session.final_proficiency:.2f}"
            
            self.memory_system.add_node(
                content=content,
                memory_type=MemoryType.PROCEDURAL,
                importance=0.6 + session.improvement,
                metadata={
                    'skill': skill.name,
                    'practice_type': session.practice_type.value,
                    'improvement': session.improvement,
                    'quality': session.quality
                }
            )
        except Exception as e:
            pass  # Silently fail if memory storage not available
            
    def get_skill_recommendations(self) -> List[Dict[str, Any]]:
        """Get recommendations for skill practice."""
        recommendations = []
        
        # Recommend based on skill gaps
        for gap in sorted(self.skill_gaps, key=lambda g: g.priority, reverse=True)[:5]:
            recommendations.append({
                'skill': gap.skill_name,
                'reason': f"Gap: {gap.current_proficiency:.2f} → {gap.target_proficiency:.2f}",
                'priority': gap.priority,
                'recommended_type': self._recommend_practice_type(gap)
            })
        
        return recommendations
        
    def _recommend_practice_type(self, gap: SkillGap) -> str:
        """Recommend practice type for a skill gap."""
        if gap.current_proficiency < 0.3:
            return "drill"
        elif gap.current_proficiency < 0.7:
            return "application"
        else:
            return "challenge"
            
    def _print_summary(self):
        """Print skill practice system summary."""
        print("\n" + "="*60)
        print("📚 Skill Practice System Summary")
        print("="*60)
        print(f"Total practice sessions: {self.total_sessions}")
        print(f"Total practice time: {self.total_practice_time:.1f}s")
        print(f"Average improvement per session: {self.average_improvement:.4f}")
        
        if self.practice_sessions:
            # Show most practiced skills
            skill_practice_counts = {}
            for session in self.practice_sessions:
                skill_practice_counts[session.skill_name] = skill_practice_counts.get(session.skill_name, 0) + 1
            
            print("\nMost practiced skills:")
            for skill_name, count in sorted(skill_practice_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
                print(f"  - {skill_name}: {count} sessions")
        
        # Show current skill gaps
        if self.skill_gaps:
            print(f"\nRemaining skill gaps: {len(self.skill_gaps)}")
            for gap in sorted(self.skill_gaps, key=lambda g: g.priority, reverse=True)[:3]:
                print(f"  - {gap.skill_name}: {gap.current_proficiency:.2f} → {gap.target_proficiency:.2f}")
        
        print("="*60)
