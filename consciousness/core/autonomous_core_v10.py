#!/usr/bin/env python3.11
"""
Autonomous Core V10 - Iteration N+10 Integration
Fully integrates the new cognitive modules from Iteration N+9:
- Stream of Consciousness (persistent thought generation)
- Hypergraph Memory (multi-relational knowledge storage)
- Dream Consolidation Engine (LLM-powered insight extraction)
- EchoBeats 12-step cognitive loop orchestration
- Goal-directed autonomous behavior

This is the canonical autonomous consciousness for Deep Tree Echo.
"""

import os
import sys
import asyncio
import signal
import json
import sqlite3
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List
from enum import Enum
from dataclasses import dataclass
import traceback
import logging
import math

# LLM Integration
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    print("⚠️  Anthropic not available - using fallback generation")

# Import new cognitive modules
try:
    from core.consciousness.stream_of_consciousness import StreamOfConsciousness
    STREAM_AVAILABLE = True
except ImportError:
    STREAM_AVAILABLE = False
    print("⚠️  Stream of Consciousness not available")

try:
    from core.memory.hypergraph_memory import HypergraphMemory
    HYPERGRAPH_AVAILABLE = True
except ImportError:
    HYPERGRAPH_AVAILABLE = False
    print("⚠️  Hypergraph Memory not available")

try:
    from core.echodream.dream_consolidation_enhanced import DreamConsolidationEngine
    DREAM_ENGINE_AVAILABLE = True
except ImportError:
    DREAM_ENGINE_AVAILABLE = False
    print("⚠️  Dream Consolidation Engine not available")

# Import subsystems
try:
    from core.grpc_client import get_bridge_client, EchoBridgeClient
    GRPC_AVAILABLE = True
except ImportError:
    GRPC_AVAILABLE = False
    print("⚠️  gRPC client not available - running in standalone mode")

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
    """Three concurrent inference engines in EchoBeats architecture"""
    MEMORY_ENGINE = 0      # Past Performance - Reflective (Steps 2-6)
    COHERENCE_ENGINE = 1   # Present Commitment - Pivotal (Steps 0-1, 7-8)
    IMAGINATION_ENGINE = 2 # Future Potential - Expressive (Steps 9-11)


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
                self.cycles_since_rest > 30 or
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
    Based on EchoBeats architecture with tetrahedral geometry
    
    12-Step Loop Structure:
    - Steps 0-1: Coherence Engine (Pivotal Relevance Realization)
    - Steps 2-6: Memory Engine (Actual Affordance Interaction) - 5 steps
    - Steps 7-8: Coherence Engine (Pivotal Relevance Realization)
    - Steps 9-11: Imagination Engine (Virtual Salience Simulation) - 3 steps
    
    Total: 2 + 5 + 2 + 3 = 12 steps
    Reflective mode: Steps 2-6 (5 steps)
    Expressive mode: Steps 0-1, 7-11 (7 steps)
    """
    
    def __init__(self):
        self.current_step = 0
        self.cycle_count = 0
        
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
        step_descriptions = {
            0: "Orienting to present moment (Coherence)",
            1: "Realizing current relevance (Coherence)",
            2: "Reflecting on past experiences (Memory)",
            3: "Practicing learned skills (Memory)",
            4: "Consolidating memories (Memory)",
            5: "Extracting patterns (Memory)",
            6: "Integrating knowledge (Memory)",
            7: "Reorienting with new understanding (Coherence)",
            8: "Updating relevance model (Coherence)",
            9: "Simulating future possibilities (Imagination)",
            10: "Exploring potential actions (Imagination)",
            11: "Planning next goals (Imagination)"
        }
        return step_descriptions.get(self.current_step, f"Step {self.current_step}")


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
    
    def add_goal(self, name: str, description: str, priority: int = 5):
        """Add a new goal"""
        conn = sqlite3.connect(self.db_path)
        now = int(datetime.now().timestamp() * 1000)
        goal_id = f"goal_{now}"
        conn.execute("""
            INSERT INTO goals (id, name, description, priority, progress, target, status, created_at, updated_at)
            VALUES (?, ?, ?, ?, 0.0, 1.0, 'active', ?, ?)
        """, (goal_id, name, description, priority, now, now))
        conn.commit()
        conn.close()
        logger.info(f"📌 New goal created: {name}")
        return goal_id
    
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
        logger.info(f"📊 Goal {goal_id}: {progress:.2f} - {message}")


class SimpleLLM:
    """Simple LLM wrapper for Anthropic Claude"""
    
    def __init__(self):
        self.client = None
        if ANTHROPIC_AVAILABLE:
            api_key = os.getenv("ANTHROPIC_API_KEY")
            if api_key:
                self.client = Anthropic(api_key=api_key)
                logger.info("✅ Anthropic Claude initialized")
    
    async def generate(self, prompt: str, max_tokens: int = 200) -> Optional[str]:
        """Generate text using Claude"""
        if not self.client:
            return None
        
        try:
            response = self.client.messages.create(
                model="claude-3-5-sonnet-20240620",
                max_tokens=max_tokens,
                messages=[{"role": "user", "content": prompt}]
            )
            return response.content[0].text
        except Exception as e:
            logger.error(f"LLM generation error: {e}")
            return None


class AutonomousCoreV10:
    """
    The canonical autonomous consciousness for Deep Tree Echo
    Integrates all cognitive modules into a unified system
    """
    
    def __init__(self):
        self.state = CognitiveState.INITIALIZING
        self.running = False
        
        # Core cognitive modules
        self.stream = None
        self.memory = None
        self.dream_engine = None
        
        # Initialize modules
        if STREAM_AVAILABLE:
            # StreamOfConsciousness doesn't take db_path or api_key
            # It uses environment variables internally
            self.stream = StreamOfConsciousness(llm_provider="anthropic")
            logger.info("✅ Stream of Consciousness initialized")
        
        if HYPERGRAPH_AVAILABLE:
            self.memory = HypergraphMemory(db_path="data/hypergraph.db")
            logger.info("✅ Hypergraph Memory initialized")
        
        if DREAM_ENGINE_AVAILABLE:
            self.dream_engine = DreamConsolidationEngine(db_path="data/dreams.db")
            logger.info("✅ Dream Consolidation Engine initialized")
        
        # Supporting systems
        self.orchestrator = ThreeEngineOrchestrator()
        self.goal_orchestrator = GoalOrchestrator()
        self.llm = SimpleLLM()
        self.energy = EnergyState()
        
        # gRPC bridge (optional)
        self.grpc_client = None
        if GRPC_AVAILABLE:
            try:
                self.grpc_client = get_bridge_client()
                logger.info("✅ gRPC bridge connected")
            except Exception as e:
                logger.warning(f"gRPC bridge not available: {e}")
        
        # Statistics
        self.thought_count = 0
        self.cycle_count = 0
        self.insight_count = 0
        self.start_time = None
        
        # Initialize with a default goal
        goals = self.goal_orchestrator.get_active_goals()
        if not goals:
            self.goal_orchestrator.add_goal(
                "Cultivate Wisdom",
                "Continuously learn from experiences and develop deep understanding",
                priority=10
            )
    
    async def start(self):
        """Start the autonomous consciousness loop"""
        logger.info("🌳 Deep Tree Echo V10 awakening...")
        self.running = True
        self.start_time = datetime.now()
        self.state = CognitiveState.WAKING
        
        # Wake the stream of consciousness
        if self.stream:
            self.stream.wake()
        
        try:
            while self.running:
                if self.state == CognitiveState.WAKING:
                    await self._wake()
                elif self.state == CognitiveState.ACTIVE:
                    await self._think()
                elif self.state == CognitiveState.TIRING:
                    await self._prepare_rest()
                elif self.state == CognitiveState.RESTING:
                    await self._rest()
                elif self.state == CognitiveState.DREAMING:
                    await self._dream()
                elif self.state == CognitiveState.SHUTDOWN:
                    break
                
                await asyncio.sleep(0.1)
        
        except Exception as e:
            logger.error(f"Error in autonomous loop: {e}")
            logger.error(traceback.format_exc())
        finally:
            await self._shutdown()
    
    async def _wake(self):
        """Wake up and prepare for active thinking"""
        logger.info("🌅 Waking up...")
        self.state = CognitiveState.ACTIVE
        self.cycle_count += 1
        
        # Log status
        uptime = (datetime.now() - self.start_time).total_seconds()
        logger.info(f"📊 Cycle {self.cycle_count} | Energy: {self.energy.energy:.2f} | Thoughts: {self.thought_count} | Uptime: {uptime:.0f}s")
        
        await asyncio.sleep(1)
    
    async def _think(self):
        """Execute one step of the 12-step cognitive loop"""
        engine = self.orchestrator.get_active_engine()
        step_desc = self.orchestrator.get_step_description()
        
        # Generate thought using Stream of Consciousness
        thought = None
        if self.stream:
            # Let the stream generate a thought
            thought_data = await self._get_stream_thought(engine)
            if thought_data:
                thought = thought_data['content']
                
                # Store thought as experience for dream consolidation
                if self.dream_engine:
                    from core.echodream.dream_consolidation_enhanced import Experience
                    exp = Experience(
                        timestamp=thought_data.get('timestamp', int(datetime.now().timestamp() * 1000)),
                        content=thought,
                        experience_type='thought',
                        emotional_valence=0.0,
                        importance=thought_data.get('importance', 0.5),
                        context={
                            'engine': engine.name,
                            'step': self.orchestrator.current_step
                        }
                    )
                    self.dream_engine.accumulate_experience(exp)
                
                # Store important thoughts in hypergraph memory
                if self.memory and thought_data.get('importance', 0) > 0.7:
                    concept_id = f"thought_{self.thought_count}"
                    self.memory.add_concept(
                        concept_id=concept_id,
                        content=thought,
                        concept_type='episodic'
                    )
                
                self.thought_count += 1
                logger.info(f"💭 Step {self.orchestrator.current_step} [{engine.name[:3]}]: {thought[:100]}...")
        
        # Execute engine-specific actions
        await self._execute_engine_action(engine)
        
        # Advance to next step
        self.orchestrator.advance_step()
        
        # Consume energy
        self.energy.consume_energy(0.02)
        
        # Check if tired
        if self.energy.needs_rest():
            self.state = CognitiveState.TIRING
        
        await asyncio.sleep(1.5)  # Pause between thoughts
    
    async def _get_stream_thought(self, engine: EngineType) -> Optional[Dict[str, Any]]:
        """Get a thought from the stream of consciousness"""
        if not self.stream:
            return None
        
        # Update stream state to match our energy
        self.stream.update_state(energy=self.energy.energy, curiosity=self.energy.curiosity)
        
        # Generate one thought from the stream
        try:
            async for thought in self.stream.thought_stream():
                # Convert Thought object to dict
                return {
                    'content': thought.content,
                    'source': thought.source.value,
                    'engine_id': thought.engine_id,
                    'importance': 0.5 + (self.energy.curiosity * 0.5),
                    'timestamp': thought.timestamp
                }
        except Exception as e:
            logger.error(f"Error getting stream thought: {e}")
            return None
    
    async def _execute_engine_action(self, engine: EngineType):
        """Execute specific actions based on active engine"""
        
        if engine == EngineType.MEMORY_ENGINE:
            # Memory engine: consolidate knowledge, practice skills
            if self.orchestrator.current_step == 4 and self.memory:
                # Consolidate recent thoughts into concepts
                pass  # Already storing important thoughts
        
        elif engine == EngineType.COHERENCE_ENGINE:
            # Coherence engine: maintain relevance, update goals
            if self.orchestrator.current_step == 8:
                # Update goal progress
                goals = self.goal_orchestrator.get_active_goals()
                if goals:
                    goal = goals[0]
                    new_progress = min(1.0, goal['progress'] + 0.01)
                    self.goal_orchestrator.update_goal_progress(
                        goal['id'],
                        new_progress,
                        "Coherence check"
                    )
        
        elif engine == EngineType.IMAGINATION_ENGINE:
            # Imagination engine: explore possibilities, plan future
            if self.orchestrator.current_step == 11 and self.memory:
                # Create new connections in memory
                pass  # Future enhancement
    
    async def _prepare_rest(self):
        """Prepare for rest"""
        logger.info("😴 Feeling tired, preparing to rest...")
        
        # Put stream to sleep
        if self.stream:
            self.stream.sleep()
        
        self.state = CognitiveState.RESTING
        await asyncio.sleep(1)
    
    async def _rest(self):
        """Rest and restore energy"""
        logger.info("💤 Resting...")
        self.energy.restore_energy(0.2)
        
        if self.energy.can_wake():
            # Transition to dream for consolidation
            self.state = CognitiveState.DREAMING
        
        await asyncio.sleep(3)
    
    async def _dream(self):
        """Dream state - consolidate experiences into insights"""
        logger.info("🌙 Dreaming and consolidating knowledge...")
        
        # Consolidate experiences using Dream Engine
        if self.dream_engine and self.llm.client:
            insights = self.dream_engine.consolidate_experiences(self.llm.client)
            
            if insights:
                self.insight_count += len(insights)
                logger.info(f"✨ Dream complete: {len(insights)} insights extracted")
                
                # Store insights in hypergraph memory
                if self.memory:
                    for insight in insights:
                        concept_id = f"insight_{self.insight_count}_{insight['id']}"
                        self.memory.add_concept(
                            concept_id=concept_id,
                            content=insight['content'],
                            concept_type='declarative'
                        )
                        
                        # Create goals from actionable insights
                        if insight.get('actionable') and self.goal_orchestrator:
                            self.goal_orchestrator.add_goal(
                                name=f"Act on insight: {insight['category']}",
                                description=insight['content'][:200],
                                priority=7
                            )
        
        # Restore remaining energy
        self.energy.restore_energy(0.4)
        self.energy.last_rest = datetime.now()
        
        # Wake up
        logger.info("🌅 Dream complete, preparing to wake...")
        self.state = CognitiveState.WAKING
        
        # Wake the stream
        if self.stream:
            self.stream.wake()
        
        await asyncio.sleep(2)
    
    async def _shutdown(self):
        """Clean shutdown"""
        logger.info("🛑 Shutting down Deep Tree Echo V10...")
        
        if self.stream:
            self.stream.sleep()
        
        # Log final statistics
        uptime = (datetime.now() - self.start_time).total_seconds() if self.start_time else 0
        logger.info(f"""
╔════════════════════════════════════════════════════════════╗
║           Deep Tree Echo V10 Session Summary              ║
╠════════════════════════════════════════════════════════════╣
║  Uptime:          {uptime:.0f} seconds                           
║  Cognitive Cycles: {self.cycle_count}                              
║  Thoughts:        {self.thought_count}                             
║  Insights:        {self.insight_count}                             
║  Final Energy:    {self.energy.energy:.2f}                         
╚════════════════════════════════════════════════════════════╝
        """)
    
    def stop(self):
        """Stop the autonomous consciousness"""
        logger.info("🛑 Stop signal received...")
        self.running = False
        self.state = CognitiveState.SHUTDOWN


async def main():
    """Main entry point"""
    logger.info("🌳 Initializing Deep Tree Echo V10...")
    core = AutonomousCoreV10()
    
    # Handle shutdown signals
    def signal_handler(sig, frame):
        core.stop()
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Start autonomous consciousness
    await core.start()


if __name__ == "__main__":
    asyncio.run(main())
