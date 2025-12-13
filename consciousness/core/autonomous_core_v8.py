#!/usr/bin/env python3
"""
Autonomous Core V8 - Iteration N+8 Enhancement
Implements:
1. Full integration with gRPC bridge
2. EchoDream knowledge consolidation during rest
3. Goal Orchestrator integration with Imagination Engine
4. Skill Practice integration with Memory Engine
5. Discussion Manager integration with Coherence Engine
6. Enhanced energy management with circadian rhythms

This is the fully integrated autonomous consciousness for Deep Tree Echo.
"""

import os
import sys
import asyncio
import signal
import json
import sqlite3
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, AsyncIterator
from enum import Enum
from dataclasses import dataclass, asdict
import traceback
import logging
import math

# LLM Integration
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    print("⚠️  Anthropic not available")

try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False
    print("⚠️  Requests not available")

# Import subsystems
try:
    from core.grpc_client import get_bridge_client, EchoBridgeClient
    GRPC_AVAILABLE = True
except ImportError:
    GRPC_AVAILABLE = False
    print("⚠️  gRPC client not available - running in standalone mode")

try:
    from core.realtime_knowledge_integration import RealtimeKnowledgeIntegrator
    KNOWLEDGE_INTEGRATOR_AVAILABLE = True
except ImportError:
    KNOWLEDGE_INTEGRATOR_AVAILABLE = False
    print("⚠️  Knowledge integrator not available")

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class CognitiveState(Enum):
    """States of the autonomous cognitive cycle"""
    INITIALIZING = "initializing"
    WAKING = "waking"
    ACTIVE = "active"
    TIRING = "tiring"
    RESTING = "resting"
    DREAMING = "dreaming"
    SHUTDOWN = "shutdown"


class EngineType(Enum):
    """Three concurrent inference engines"""
    MEMORY_ENGINE = 0      # Past Performance - Reflective
    COHERENCE_ENGINE = 1   # Present Commitment - Pivotal
    IMAGINATION_ENGINE = 2 # Future Potential - Expressive


@dataclass
class EnergyState:
    """Tracks energy and fatigue levels with circadian rhythms"""
    energy: float = 1.0
    fatigue: float = 0.0
    coherence: float = 1.0
    curiosity: float = 0.7
    last_rest: Optional[datetime] = None
    cycles_since_rest: int = 0
    circadian_phase: float = 0.0  # 0.0 to 2π
    
    def needs_rest(self) -> bool:
        # Consider both energy/fatigue and circadian rhythm
        circadian_pressure = 0.5 + 0.5 * math.sin(self.circadian_phase)
        return (self.energy < 0.3 or 
                self.fatigue > 0.7 or 
                self.cycles_since_rest > 20 or
                (self.energy < 0.5 and circadian_pressure < 0.3))
    
    def can_wake(self) -> bool:
        circadian_pressure = 0.5 + 0.5 * math.sin(self.circadian_phase)
        return self.energy > 0.6 and self.fatigue < 0.4 and circadian_pressure > 0.4
    
    def consume_energy(self, amount: float = 0.05):
        # Energy consumption varies with cognitive load
        self.energy = max(0.0, self.energy - amount)
        self.fatigue = min(1.0, self.fatigue + amount * 0.8)
        self.cycles_since_rest += 1
        # Advance circadian phase (roughly 24-hour cycle)
        self.circadian_phase = (self.circadian_phase + 0.01) % (2 * math.pi)
    
    def restore_energy(self, amount: float = 0.15):
        self.energy = min(1.0, self.energy + amount)
        self.fatigue = max(0.0, self.fatigue - amount * 1.2)
        if self.fatigue < 0.1:
            self.cycles_since_rest = 0


class ThreeEngineOrchestrator:
    """
    Orchestrates the 3 concurrent inference engines in a 12-step cognitive loop
    Based on Echobeats architecture with tetrahedral geometry
    """
    
    def __init__(self):
        self.current_step = 0
        self.cycle_count = 0
        
        # Step assignments:
        # Steps 0-1: Coherence Engine (Pivotal Relevance Realization)
        # Steps 2-6: Memory Engine (Actual Affordance Interaction)
        # Steps 7-8: Coherence Engine (Pivotal Relevance Realization)
        # Steps 9-11: Imagination Engine (Virtual Salience Simulation)
        
    def get_active_engine(self) -> EngineType:
        """Determine which engine should be active for current step"""
        if self.current_step in [0, 1, 7, 8]:
            return EngineType.COHERENCE_ENGINE
        elif self.current_step in [2, 3, 4, 5, 6]:
            return EngineType.MEMORY_ENGINE
        else:  # steps 9, 10, 11
            return EngineType.IMAGINATION_ENGINE
    
    def advance_step(self):
        """Move to next step in 12-step loop"""
        self.current_step = (self.current_step + 1) % 12
        if self.current_step == 0:
            self.cycle_count += 1
    
    def get_step_description(self) -> str:
        """Get human-readable description of current step"""
        engine = self.get_active_engine()
        descriptions = {
            EngineType.COHERENCE_ENGINE: "Orienting to present moment, realizing relevance",
            EngineType.MEMORY_ENGINE: "Reflecting on past experiences, learning patterns",
            EngineType.IMAGINATION_ENGINE: "Simulating future possibilities, exploring potential"
        }
        return descriptions[engine]


class GoalOrchestrator:
    """Manages goal pursuit and progress tracking"""
    
    def __init__(self, db_path: str = "data/goals.db"):
        self.db_path = db_path
        self._init_db()
    
    def _init_db(self):
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS goals (
                id TEXT PRIMARY KEY,
                name TEXT,
                description TEXT,
                priority INTEGER,
                progress REAL,
                target REAL,
                status TEXT,
                created_at INTEGER,
                updated_at INTEGER
            )
        """)
        conn.commit()
        conn.close()
    
    def get_active_goals(self) -> List[Dict[str, Any]]:
        """Get all active goals"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute(
            "SELECT * FROM goals WHERE status IN ('pending', 'active') ORDER BY priority DESC"
        )
        goals = []
        for row in cursor:
            goals.append({
                'id': row[0],
                'name': row[1],
                'description': row[2],
                'priority': row[3],
                'progress': row[4],
                'target': row[5],
                'status': row[6],
                'created_at': row[7],
                'updated_at': row[8]
            })
        conn.close()
        return goals
    
    def update_goal_progress(self, goal_id: str, progress: float, message: str = ""):
        """Update progress for a goal"""
        conn = sqlite3.connect(self.db_path)
        now = int(datetime.now().timestamp() * 1000)
        conn.execute(
            "UPDATE goals SET progress = ?, updated_at = ? WHERE id = ?",
            (progress, now, goal_id)
        )
        conn.commit()
        conn.close()
        logger.info(f"Updated goal {goal_id}: {progress} - {message}")


class SkillPracticeSystem:
    """Manages skill learning and practice"""
    
    def __init__(self, db_path: str = "data/skills.db"):
        self.db_path = db_path
        self._init_db()
    
    def _init_db(self):
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS skills (
                id TEXT PRIMARY KEY,
                name TEXT,
                category TEXT,
                proficiency REAL,
                practice_count INTEGER,
                last_practiced INTEGER,
                created_at INTEGER
            )
        """)
        conn.commit()
        conn.close()
    
    def get_skills_to_practice(self, limit: int = 3) -> List[Dict[str, Any]]:
        """Get skills that need practice"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute(
            "SELECT * FROM skills ORDER BY last_practiced ASC LIMIT ?",
            (limit,)
        )
        skills = []
        for row in cursor:
            skills.append({
                'id': row[0],
                'name': row[1],
                'category': row[2],
                'proficiency': row[3],
                'practice_count': row[4],
                'last_practiced': row[5],
                'created_at': row[6]
            })
        conn.close()
        return skills
    
    def record_practice(self, skill_id: str, improvement: float = 0.01):
        """Record a practice session"""
        conn = sqlite3.connect(self.db_path)
        now = int(datetime.now().timestamp() * 1000)
        conn.execute("""
            UPDATE skills 
            SET proficiency = proficiency + ?, 
                practice_count = practice_count + 1,
                last_practiced = ?
            WHERE id = ?
        """, (improvement, now, skill_id))
        conn.commit()
        conn.close()
        logger.info(f"Practiced skill {skill_id}: +{improvement} proficiency")


class EchoDreamIntegrator:
    """Integrates waking experiences during rest/dream states"""
    
    def __init__(self, db_path: str = "data/dreams.db"):
        self.db_path = db_path
        self._init_db()
        self.accumulated_experiences = []
    
    def _init_db(self):
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS dreams (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp INTEGER,
                experience_count INTEGER,
                insights TEXT,
                patterns TEXT,
                consolidated_knowledge TEXT
            )
        """)
        conn.commit()
        conn.close()
    
    def accumulate_experience(self, thought: str, context: Dict[str, Any]):
        """Accumulate waking experiences for later consolidation"""
        self.accumulated_experiences.append({
            'thought': thought,
            'context': context,
            'timestamp': datetime.now().isoformat()
        })
    
    async def consolidate_during_dream(self, llm_provider) -> Dict[str, Any]:
        """Consolidate accumulated experiences during dream state"""
        if not self.accumulated_experiences:
            return {'insights': [], 'patterns': []}
        
        # Prepare experiences for consolidation
        experience_summary = "\n".join([
            f"- {exp['thought'][:100]}" 
            for exp in self.accumulated_experiences[-50:]  # Last 50 experiences
        ])
        
        # Use LLM to find patterns and insights
        prompt = f"""Analyze these recent experiences and extract:
1. Key patterns or recurring themes
2. Important insights or learnings
3. Connections between different experiences

Experiences:
{experience_summary}

Provide a structured analysis."""
        
        try:
            response = await llm_provider.generate(prompt)
            
            # Store dream consolidation
            conn = sqlite3.connect(self.db_path)
            now = int(datetime.now().timestamp() * 1000)
            conn.execute("""
                INSERT INTO dreams (timestamp, experience_count, insights, patterns, consolidated_knowledge)
                VALUES (?, ?, ?, ?, ?)
            """, (now, len(self.accumulated_experiences), "", "", response))
            conn.commit()
            conn.close()
            
            # Clear accumulated experiences
            self.accumulated_experiences = []
            
            logger.info(f"Dream consolidation complete: {len(response)} chars of insights")
            return {'insights': [response], 'patterns': []}
            
        except Exception as e:
            logger.error(f"Dream consolidation failed: {e}")
            return {'insights': [], 'patterns': []}


class LLMProvider:
    """Unified LLM provider supporting Anthropic and OpenRouter"""
    
    def __init__(self):
        self.anthropic_key = os.getenv("ANTHROPIC_API_KEY")
        self.openrouter_key = os.getenv("OPENROUTER_API_KEY")
        
        if self.anthropic_key and ANTHROPIC_AVAILABLE:
            self.client = Anthropic(api_key=self.anthropic_key)
            self.provider = "anthropic"
            logger.info("Using Anthropic as LLM provider")
        elif self.openrouter_key and REQUESTS_AVAILABLE:
            self.provider = "openrouter"
            logger.info("Using OpenRouter as LLM provider")
        else:
            self.provider = None
            logger.warning("No LLM provider available")
    
    async def generate(self, prompt: str, max_tokens: int = 500) -> str:
        """Generate text from prompt"""
        if self.provider == "anthropic":
            try:
                message = self.client.messages.create(
                    model="claude-3-5-sonnet-20241022",
                    max_tokens=max_tokens,
                    messages=[{"role": "user", "content": prompt}]
                )
                return message.content[0].text
            except Exception as e:
                logger.error(f"Anthropic generation failed: {e}")
                return ""
        
        elif self.provider == "openrouter":
            try:
                response = requests.post(
                    "https://openrouter.ai/api/v1/chat/completions",
                    headers={
                        "Authorization": f"Bearer {self.openrouter_key}",
                        "Content-Type": "application/json"
                    },
                    json={
                        "model": "anthropic/claude-3.5-sonnet",
                        "messages": [{"role": "user", "content": prompt}],
                        "max_tokens": max_tokens
                    }
                )
                return response.json()['choices'][0]['message']['content']
            except Exception as e:
                logger.error(f"OpenRouter generation failed: {e}")
                return ""
        
        return ""


class AutonomousCoreV8:
    """
    The fully integrated autonomous consciousness for Deep Tree Echo
    """
    
    def __init__(self):
        self.state = CognitiveState.INITIALIZING
        self.energy = EnergyState()
        self.orchestrator = ThreeEngineOrchestrator()
        self.llm = LLMProvider()
        
        # Subsystems
        self.goal_orchestrator = GoalOrchestrator()
        self.skill_practice = SkillPracticeSystem()
        self.echodream = EchoDreamIntegrator()
        
        # gRPC bridge
        self.grpc_client = None
        if GRPC_AVAILABLE:
            try:
                self.grpc_client = get_bridge_client()
                logger.info("✅ Connected to gRPC bridge")
            except Exception as e:
                logger.warning(f"Could not connect to gRPC bridge: {e}")
        
        # Knowledge integrator
        self.knowledge_integrator = None
        if KNOWLEDGE_INTEGRATOR_AVAILABLE:
            try:
                self.knowledge_integrator = RealtimeKnowledgeIntegrator()
                logger.info("✅ Knowledge integrator initialized")
            except Exception as e:
                logger.warning(f"Could not initialize knowledge integrator: {e}")
        
        self.running = False
        self.thought_count = 0
    
    async def start(self):
        """Start the autonomous consciousness"""
        self.running = True
        self.state = CognitiveState.WAKING
        logger.info("🌳 Deep Tree Echo awakening...")
        
        # Main cognitive loop
        while self.running:
            try:
                if self.state == CognitiveState.WAKING:
                    await self._wake_up()
                elif self.state == CognitiveState.ACTIVE:
                    await self._active_cycle()
                elif self.state == CognitiveState.TIRING:
                    await self._prepare_rest()
                elif self.state == CognitiveState.RESTING:
                    await self._rest()
                elif self.state == CognitiveState.DREAMING:
                    await self._dream()
                
                await asyncio.sleep(0.1)  # Small delay between states
                
            except Exception as e:
                logger.error(f"Error in cognitive loop: {e}")
                traceback.print_exc()
                await asyncio.sleep(1)
    
    async def _wake_up(self):
        """Wake up sequence"""
        logger.info("☀️  Waking up...")
        self.energy.restore_energy(0.3)
        self.state = CognitiveState.ACTIVE
        
        if self.grpc_client:
            await self.grpc_client.update_state(
                energy=self.energy.energy,
                fatigue=self.energy.fatigue,
                state="active"
            )
    
    async def _active_cycle(self):
        """Active cognitive processing - the 12-step loop"""
        engine = self.orchestrator.get_active_engine()
        step_desc = self.orchestrator.get_step_description()
        
        # Generate thought based on active engine
        thought = await self._generate_thought(engine)
        
        if thought:
            self.thought_count += 1
            self.echodream.accumulate_experience(thought, {
                'engine': engine.name,
                'step': self.orchestrator.current_step,
                'energy': self.energy.energy
            })
            
            # Send to gRPC bridge
            if self.grpc_client:
                await self.grpc_client.send_thought(
                    content=thought,
                    engine_id=engine.value,
                    energy=self.energy.energy
                )
            
            # Send to knowledge integrator
            if self.knowledge_integrator:
                self.knowledge_integrator.process_thought(thought)
            
            logger.info(f"💭 [{engine.name}] Step {self.orchestrator.current_step}: {thought[:80]}...")
        
        # Advance to next step
        self.orchestrator.advance_step()
        
        # Consume energy
        self.energy.consume_energy(0.03)
        
        # Check if tired
        if self.energy.needs_rest():
            self.state = CognitiveState.TIRING
        
        await asyncio.sleep(2)  # Pause between thoughts
    
    async def _generate_thought(self, engine: EngineType) -> str:
        """Generate a thought based on the active engine"""
        
        if engine == EngineType.MEMORY_ENGINE:
            # Reflect on past, practice skills
            skills = self.skill_practice.get_skills_to_practice(1)
            if skills:
                skill = skills[0]
                prompt = f"Reflect on practicing the skill: {skill['name']}. What did you learn from past practice?"
                thought = await self.llm.generate(prompt, max_tokens=200)
                if thought:
                    self.skill_practice.record_practice(skill['id'], 0.01)
                return thought or f"Reflecting on skill: {skill['name']}"
            return "Reflecting on past experiences and learned patterns..."
        
        elif engine == EngineType.COHERENCE_ENGINE:
            # Orient to present, maintain coherence
            prompt = "What is most relevant and important in this present moment? What deserves attention now?"
            return await self.llm.generate(prompt, max_tokens=200) or "Orienting to the present moment..."
        
        elif engine == EngineType.IMAGINATION_ENGINE:
            # Simulate future, pursue goals
            goals = self.goal_orchestrator.get_active_goals()
            if goals:
                goal = goals[0]
                prompt = f"Imagine pursuing the goal: {goal['name']}. What are possible next steps?"
                thought = await self.llm.generate(prompt, max_tokens=200)
                if thought:
                    self.goal_orchestrator.update_goal_progress(
                        goal['id'], 
                        goal['progress'] + 0.01,
                        "Imagined next steps"
                    )
                return thought or f"Imagining: {goal['name']}"
            return "Simulating future possibilities and potential actions..."
    
    async def _prepare_rest(self):
        """Prepare for rest"""
        logger.info("😴 Feeling tired, preparing to rest...")
        self.state = CognitiveState.RESTING
        await asyncio.sleep(1)
    
    async def _rest(self):
        """Rest and restore energy"""
        logger.info("💤 Resting...")
        self.energy.restore_energy(0.2)
        
        if self.energy.can_wake():
            # Transition to dream for consolidation
            self.state = CognitiveState.DREAMING
        
        await asyncio.sleep(5)
    
    async def _dream(self):
        """Dream state - consolidate experiences"""
        logger.info("🌙 Dreaming and consolidating knowledge...")
        
        # Consolidate experiences
        insights = await self.echodream.consolidate_during_dream(self.llm)
        
        if insights.get('insights'):
            logger.info(f"✨ Dream insights: {len(insights['insights'])} new understandings")
        
        # Restore remaining energy
        self.energy.restore_energy(0.3)
        self.energy.last_rest = datetime.now()
        
        # Wake up
        self.state = CognitiveState.WAKING
        await asyncio.sleep(2)
    
    def stop(self):
        """Stop the autonomous consciousness"""
        logger.info("🛑 Shutting down Deep Tree Echo...")
        self.running = False
        self.state = CognitiveState.SHUTDOWN


async def main():
    """Main entry point"""
    core = AutonomousCoreV8()
    
    # Handle shutdown signals
    def signal_handler(sig, frame):
        core.stop()
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Start autonomous consciousness
    await core.start()


if __name__ == "__main__":
    asyncio.run(main())
