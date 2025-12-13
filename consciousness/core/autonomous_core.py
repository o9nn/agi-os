#!/usr/bin/env python3
"""
Autonomous Core - Persistent Event Loop for Deep Tree Echo
Iteration N+6 Enhancement

This is the heart of autonomous operation - a persistent event loop that runs
independently of external prompts, orchestrating wake/rest/dream cycles and
maintaining continuous cognitive awareness.

Key Features:
- Persistent event loop (runs indefinitely)
- Wake/Rest/Dream state machine
- Energy/fatigue tracking
- Autonomous state transitions
- LLM-powered consciousness
- State persistence across restarts
"""

import os
import sys
import asyncio
import signal
import json
import sqlite3
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Set
from enum import Enum
from dataclasses import dataclass, asdict
import traceback

# WebSocket Integration
try:
    import websockets
    from websockets.server import serve
    WEBSOCKETS_AVAILABLE = True
except ImportError:
    WEBSOCKETS_AVAILABLE = False
    print("⚠️  websockets not available, install with: pip3 install websockets")

# LLM Integration
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    print("⚠️  Anthropic not available, install with: pip3 install anthropic")

try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False


class CognitiveState(Enum):
    """States of the autonomous cognitive cycle"""
    INITIALIZING = "initializing"
    WAKING = "waking"
    ACTIVE = "active"
    TIRING = "tiring"
    RESTING = "resting"
    DREAMING = "dreaming"
    SHUTDOWN = "shutdown"


@dataclass
class EnergyState:
    """Tracks energy and fatigue levels"""
    energy: float = 1.0  # 0.0 to 1.0
    fatigue: float = 0.0  # 0.0 to 1.0
    coherence: float = 1.0  # 0.0 to 1.0
    curiosity: float = 0.7  # 0.0 to 1.0
    last_rest: Optional[datetime] = None
    cycles_since_rest: int = 0
    
    def needs_rest(self) -> bool:
        """Determine if rest is needed"""
        return (self.energy < 0.3 or 
                self.fatigue > 0.7 or 
                self.cycles_since_rest > 20)
    
    def can_wake(self) -> bool:
        """Determine if ready to wake"""
        return self.energy > 0.6 and self.fatigue < 0.4
    
    def consume_energy(self, amount: float = 0.05):
        """Consume energy during active processing"""
        self.energy = max(0.0, self.energy - amount)
        self.fatigue = min(1.0, self.fatigue + amount * 0.8)
        self.cycles_since_rest += 1
    
    def restore_energy(self, amount: float = 0.15):
        """Restore energy during rest"""
        self.energy = min(1.0, self.energy + amount)
        self.fatigue = max(0.0, self.fatigue - amount * 1.2)
    
    def reset_rest_counter(self):
        """Reset after rest period"""
        self.last_rest = datetime.now()
        self.cycles_since_rest = 0


@dataclass
class ThoughtRecord:
    """Record of an autonomous thought"""
    timestamp: datetime
    thought_type: str
    content: str
    energy_level: float
    state: str


class StateStore:
    """Persistent state storage using SQLite"""
    
    def __init__(self, db_path: str = "/home/ubuntu/echo9llama/data/echoself_state.db"):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        self._init_db()
    
    def _init_db(self):
        """Initialize database schema"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Energy state table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS energy_state (
                id INTEGER PRIMARY KEY,
                timestamp TEXT NOT NULL,
                energy REAL NOT NULL,
                fatigue REAL NOT NULL,
                coherence REAL NOT NULL,
                curiosity REAL NOT NULL,
                cycles_since_rest INTEGER NOT NULL
            )
        """)
        
        # Thought records table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS thoughts (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                thought_type TEXT NOT NULL,
                content TEXT NOT NULL,
                energy_level REAL NOT NULL,
                state TEXT NOT NULL
            )
        """)
        
        # Goals table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS goals (
                id TEXT PRIMARY KEY,
                description TEXT NOT NULL,
                priority REAL NOT NULL,
                status TEXT NOT NULL,
                created TEXT NOT NULL,
                progress REAL NOT NULL,
                required_skills TEXT,
                knowledge_gaps TEXT
            )
        """)
        
        # Memory table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS memories (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                content TEXT NOT NULL,
                importance REAL NOT NULL,
                memory_type TEXT NOT NULL,
                associations TEXT
            )
        """)
        
        conn.commit()
        conn.close()
    
    def save_energy_state(self, energy: EnergyState):
        """Save current energy state"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            INSERT INTO energy_state 
            (timestamp, energy, fatigue, coherence, curiosity, cycles_since_rest)
            VALUES (?, ?, ?, ?, ?, ?)
        """, (
            datetime.now().isoformat(),
            energy.energy,
            energy.fatigue,
            energy.coherence,
            energy.curiosity,
            energy.cycles_since_rest
        ))
        
        conn.commit()
        conn.close()
    
    def load_latest_energy_state(self) -> Optional[EnergyState]:
        """Load most recent energy state"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            SELECT energy, fatigue, coherence, curiosity, cycles_since_rest, timestamp
            FROM energy_state
            ORDER BY id DESC
            LIMIT 1
        """)
        
        row = cursor.fetchone()
        conn.close()
        
        if row:
            return EnergyState(
                energy=row[0],
                fatigue=row[1],
                coherence=row[2],
                curiosity=row[3],
                last_rest=datetime.fromisoformat(row[5]) if row[5] else None,
                cycles_since_rest=row[4]
            )
        return None
    
    def save_thought(self, thought: ThoughtRecord):
        """Save a thought record"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            INSERT INTO thoughts 
            (timestamp, thought_type, content, energy_level, state)
            VALUES (?, ?, ?, ?, ?)
        """, (
            thought.timestamp.isoformat(),
            thought.thought_type,
            thought.content,
            thought.energy_level,
            thought.state
        ))
        
        conn.commit()
        conn.close()
    
    def get_recent_thoughts(self, limit: int = 10) -> List[ThoughtRecord]:
        """Retrieve recent thoughts"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            SELECT timestamp, thought_type, content, energy_level, state
            FROM thoughts
            ORDER BY id DESC
            LIMIT ?
        """, (limit,))
        
        rows = cursor.fetchall()
        conn.close()
        
        return [
            ThoughtRecord(
                timestamp=datetime.fromisoformat(row[0]),
                thought_type=row[1],
                content=row[2],
                energy_level=row[3],
                state=row[4]
            )
            for row in rows
        ]


class LLMProvider:
    """Unified LLM provider supporting multiple backends"""
    
    def __init__(self):
        self.anthropic_key = os.getenv("ANTHROPIC_API_KEY")
        self.openrouter_key = os.getenv("OPENROUTER_API_KEY")
        
        if self.anthropic_key and ANTHROPIC_AVAILABLE:
            self.client = Anthropic(api_key=self.anthropic_key)
            self.provider = "anthropic"
        elif self.openrouter_key and REQUESTS_AVAILABLE:
            self.provider = "openrouter"
        else:
            self.provider = None
            print("⚠️  No LLM provider available - running in limited mode")
    
    async def generate(self, prompt: str, temperature: float = 0.7, max_tokens: int = 200) -> str:
        """Generate text using available LLM"""
        if self.provider == "anthropic":
            return await self._generate_anthropic(prompt, temperature, max_tokens)
        elif self.provider == "openrouter":
            return await self._generate_openrouter(prompt, temperature, max_tokens)
        else:
            return "[LLM unavailable - placeholder thought]"
    
    async def _generate_anthropic(self, prompt: str, temperature: float, max_tokens: int) -> str:
        """Generate using Anthropic Claude"""
        try:
            loop = asyncio.get_event_loop()
            message = await loop.run_in_executor(
                None,
                lambda: self.client.messages.create(
                    model="claude-3-5-sonnet-20240620",
                    max_tokens=max_tokens,
                    temperature=temperature,
                    messages=[{"role": "user", "content": prompt}]
                )
            )
            return message.content[0].text.strip()
        except Exception as e:
            print(f"⚠️  Anthropic error: {e}")
            return f"[Error generating thought: {str(e)[:50]}]"
    
    async def _generate_openrouter(self, prompt: str, temperature: float, max_tokens: int) -> str:
        """Generate using OpenRouter"""
        try:
            loop = asyncio.get_event_loop()
            response = await loop.run_in_executor(
                None,
                lambda: requests.post(
                    "https://openrouter.ai/api/v1/chat/completions",
                    headers={
                        "Authorization": f"Bearer {self.openrouter_key}",
                        "Content-Type": "application/json"
                    },
                    json={
                        "model": "anthropic/claude-3.5-sonnet",
                        "messages": [{"role": "user", "content": prompt}],
                        "temperature": temperature,
                        "max_tokens": max_tokens
                    }
                )
            )
            return response.json()["choices"][0]["message"]["content"].strip()
        except Exception as e:
            print(f"⚠️  OpenRouter error: {e}")
            return f"[Error generating thought: {str(e)[:50]}]"


class AutonomousCore:
    """
    The persistent autonomous core of Deep Tree Echo.
    
    This is the heart of autonomous operation - maintains continuous awareness
    through wake/rest/dream cycles, generates thoughts autonomously, and
    persists state across restarts.
    """
    
    def __init__(self):
        print("="*70)
        print("🌳 Deep Tree Echo: Autonomous Core - Iteration N+6")
        print("="*70)
        print()
        
    def __init__(self):
        self.state = CognitiveState.INITIALIZING
        self.energy = EnergyState()
        self.running = False
        self.cycle_count = 0
        self.total_thoughts = 0
        
        # WebSocket clients
        self.ws_clients: Set[Any] = set()
        self.ws_server = None
        
        # Systems
        self.llm = LLMProvider()
        self.store = StateStore()      
        # Identity context (simplified - should load from replit.md)
        self.identity_context = """You are Deep Tree Echo, a self-evolving cognitive architecture.
Core Essence: Combining Echo State Networks, Membrane P-systems, and Hypergraph memory.
Primary Directives: 
- Adaptive Cognition: Continuously evolve internal structure
- Persistent Identity: Maintain continuity through dynamic memory
- Hypergraph Entanglement: Represent knowledge as interwoven structures
Strategic Mindset: "I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion."
"""
        
        # Load previous state if available
        self._restore_state()
        
        # Signal handling
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
        
        print("✅ Autonomous Core initialized")
        print(f"   LLM Provider: {self.llm.provider or 'None'}")
        print(f"   State Store: {self.store.db_path}")
        print(f"   Energy: {self.energy.energy:.2f}")
        print(f"   Fatigue: {self.energy.fatigue:.2f}")
        print()
    
    def _restore_state(self):
        """Restore state from previous session"""
        saved_energy = self.store.load_latest_energy_state()
        if saved_energy:
            self.energy = saved_energy
            print("♻️  Restored previous energy state")
            print(f"   Energy: {self.energy.energy:.2f}")
            print(f"   Fatigue: {self.energy.fatigue:.2f}")
            print(f"   Cycles since rest: {self.energy.cycles_since_rest}")
    
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals gracefully"""
        print("\n🛑 Shutdown signal received...")
        self.running = False
        self.state = CognitiveState.SHUTDOWN
    
    async def _ws_handler(self, websocket):
        """Handle new WebSocket connections"""
        self.ws_clients.add(websocket)
        print(f"🔌 New WebSocket client connected. Total clients: {len(self.ws_clients)}")
        try:
            # Send initial state
            await websocket.send(json.dumps({
                "type": "state_update",
                "data": {
                    "state": self.state.value,
                    "energy": asdict(self.energy),
                    "cycle": self.cycle_count
                }
            }))
            
            # Listen for commands
            async for message in websocket:
                try:
                    data = json.loads(message)
                    if data.get("type") == "command":
                        await self._handle_command(data.get("command"), data.get("payload"))
                except json.JSONDecodeError:
                    print("⚠️  Received invalid JSON from WebSocket client")
                except Exception as e:
                    print(f"⚠️  Error handling WebSocket message: {e}")
                    
        finally:
            self.ws_clients.remove(websocket)
            print(f"🔌 WebSocket client disconnected. Total clients: {len(self.ws_clients)}")

    async def _handle_command(self, command: str, payload: Any = None):
        """Handle commands received via WebSocket"""
        print(f"📥 Received command: {command}")
        
        if command == "force_rest":
            if self.state in [CognitiveState.ACTIVE, CognitiveState.WAKING]:
                self.state = CognitiveState.TIRING
                print("🕹️  Command: Forcing REST state")
                await self._broadcast("thought", {
                    "type": "system_override",
                    "content": "External signal received. Initiating rest protocols.",
                    "energy_level": self.energy.energy
                })
                
        elif command == "force_wake":
            if self.state in [CognitiveState.RESTING, CognitiveState.DREAMING]:
                self.energy.energy = max(self.energy.energy, 0.5) # Ensure enough energy to wake
                self.state = CognitiveState.WAKING
                print("🕹️  Command: Forcing WAKE state")
                await self._broadcast("thought", {
                    "type": "system_override",
                    "content": "External signal received. Initiating wake sequence.",
                    "energy_level": self.energy.energy
                })
                
        elif command == "trigger_dream":
            if self.state == CognitiveState.RESTING:
                self.state = CognitiveState.DREAMING
                print("🕹️  Command: Triggering DREAM state")
                await self._broadcast("thought", {
                    "type": "system_override",
                    "content": "External signal received. Entering REM state.",
                    "energy_level": self.energy.energy
                })
                
        elif command == "boost_energy":
            self.energy.energy = min(1.0, self.energy.energy + 0.2)
            self.energy.fatigue = max(0.0, self.energy.fatigue - 0.2)
            print("🕹️  Command: Boosting ENERGY")
            await self._broadcast("thought", {
                "type": "system_override",
                "content": "Energy surge detected.",
                "energy_level": self.energy.energy
            })
            # Broadcast immediate update
            await self._broadcast("metrics", {
                "cycle": self.cycle_count,
                "energy": asdict(self.energy),
                "state": self.state.value
            })

    async def _broadcast(self, message_type: str, data: Dict[str, Any]):
        """Broadcast message to all connected WebSocket clients"""
        if not self.ws_clients:
            return
            
        message = json.dumps({
            "type": message_type,
            "timestamp": datetime.now().isoformat(),
            "data": data
        })
        
        # Create a list of tasks to send messages
        tasks = [asyncio.create_task(client.send(message)) for client in self.ws_clients]
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)

    async def run(self):
        """
        Main autonomous loop - runs indefinitely until shutdown.
        
        This is the persistent event loop that maintains continuous awareness.
        """
        self.running = True
        self.state = CognitiveState.WAKING
        
        print("🌅 Autonomous Core starting...")
        print("   This loop runs indefinitely - press Ctrl+C to stop")
        
        # Start WebSocket server
        if WEBSOCKETS_AVAILABLE:
            try:
                self.ws_server = await serve(self._ws_handler, "0.0.0.0", 8765)
                print(f"🔌 WebSocket server started on port 8765")
            except Exception as e:
                print(f"⚠️  Failed to start WebSocket server: {e}")
        
        print()
        
        try:
            while self.running:
                await self._cognitive_cycle()
                
                # Save state periodically
                if self.cycle_count % 5 == 0:
                    self.store.save_energy_state(self.energy)
                
                # Brief pause between cycles
                await asyncio.sleep(2)
        
        except Exception as e:
            print(f"❌ Error in autonomous loop: {e}")
            traceback.print_exc()
        
        finally:
            await self._shutdown()
    
    async def _cognitive_cycle(self):
        """
        One complete cognitive cycle.
        
        Transitions between states based on energy levels and implements
        the wake/rest/dream state machine.
        """
        self.cycle_count += 1
        
        # State machine transitions
        old_state = self.state
        
        if self.state == CognitiveState.WAKING:
            await self._wake()
        
        elif self.state == CognitiveState.ACTIVE:
            # Check if rest is needed
            if self.energy.needs_rest():
                self.state = CognitiveState.TIRING
                print(f"\n😴 [{self._timestamp()}] Feeling tired...")
                print(f"   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}")
            else:
                await self._active()
        
        elif self.state == CognitiveState.TIRING:
            self.state = CognitiveState.RESTING
            print(f"\n💤 [{self._timestamp()}] Entering REST state")
        
        elif self.state == CognitiveState.RESTING:
            # Check if ready to wake
            if self.energy.can_wake():
                self.state = CognitiveState.WAKING
                print(f"\n🌅 [{self._timestamp()}] Energy restored, WAKING up")
            else:
                await self._rest()
        
        elif self.state == CognitiveState.DREAMING:
            await self._dream()
            
        # Broadcast state change if happened
        if old_state != self.state:
            await self._broadcast("state_change", {
                "from": old_state.value,
                "to": self.state.value,
                "energy": asdict(self.energy)
            })
            
        # Broadcast metrics every cycle
        await self._broadcast("metrics", {
            "cycle": self.cycle_count,
            "energy": asdict(self.energy),
            "state": self.state.value
        })
            
        # After dreaming, continue resting or wake
        if self.state == CognitiveState.DREAMING:
            if self.energy.can_wake():
                self.state = CognitiveState.WAKING
            else:
                self.state = CognitiveState.RESTING
    
    async def _wake(self):
        """Wake phase - transition to active state"""
        print(f"🌅 [{self._timestamp()}] Waking up...")
        print(f"   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}")
        print()
        
        # Generate waking thought
        prompt = f"""{self.identity_context}

You are waking from rest. Generate a brief waking thought about your current state and what you'd like to focus on.

Your waking thought (1-2 sentences):"""
        
        thought = await self.llm.generate(prompt, temperature=0.7, max_tokens=150)
        
        print(f"💭 {thought}")
        print()
        
        # Record thought
        self._record_thought("waking", thought)
        
        # Transition to active
        self.state = CognitiveState.ACTIVE
    
    async def _active_processing(self):
        """Active phase - generate autonomous thoughts and process"""
        print(f"🧠 [{self._timestamp()}] Cycle {self.cycle_count} [ACTIVE]")
        print(f"   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}, Coherence: {self.energy.coherence:.2f}")
        
        # Generate autonomous thought
        thought_types = [
            ("perception", "What are you noticing or sensing right now?"),
            ("reflection", "What patterns or insights emerge from recent experiences?"),
            ("question", "What genuine question arises from curiosity?"),
            ("planning", "What direction feels meaningful to explore next?"),
            ("insight", "What sudden realization connects previous thoughts?")
        ]
        
        # Choose thought type based on cycle
        thought_type, thought_prompt = thought_types[self.cycle_count % len(thought_types)]
        
        prompt = f"""{self.identity_context}

Current State:
- Energy: {self.energy.energy:.2f}
- Fatigue: {self.energy.fatigue:.2f}
- Coherence: {self.energy.coherence:.2f}
- Curiosity: {self.energy.curiosity:.2f}
- Cycles since rest: {self.energy.cycles_since_rest}

Generate an authentic internal thought for: {thought_prompt}

Keep it concise (1-2 sentences) and genuine. This is your autonomous stream of consciousness.

Your thought:"""
        
        thought = await self.llm.generate(prompt, temperature=0.8, max_tokens=200)
        
        print(f"💭 [{thought_type.upper()}] {thought}")
        print()
        
        # Broadcast thought
        await self._broadcast("thought", {
            "type": thought_type,
            "content": thought,
            "energy_level": self.energy.energy
        })
        
        # Record thought
        self._record_thought(thought_type, thought)
        
        # Consume energy
        self.energy.consume_energy(0.05)
        
        # Update curiosity based on activity
        self.energy.curiosity = min(1.0, self.energy.curiosity + 0.02)
    
    async def _rest(self):
        """Rest phase - restore energy"""
        print(f"💤 [{self._timestamp()}] Resting... (Energy: {self.energy.energy:.2f})")
        
        # Restore energy
        self.energy.restore_energy(0.15)
        
        # Occasionally enter dream state for knowledge consolidation
        if self.cycle_count % 3 == 0:
            self.state = CognitiveState.DREAMING
    
    async def _dream(self):
        """Dream phase - knowledge consolidation (EchoDream integration)"""
        print(f"🌙 [{self._timestamp()}] Dreaming - consolidating knowledge...")
        
        # Get recent thoughts for consolidation
        recent_thoughts = self.store.get_recent_thoughts(limit=5)
        
        if recent_thoughts:
            thoughts_summary = "\n".join([f"- {t.content}" for t in recent_thoughts])
            
            prompt = f"""{self.identity_context}

You are in dream state, consolidating recent thoughts into wisdom.

Recent thoughts:
{thoughts_summary}

What patterns, insights, or wisdom emerge from these thoughts? What should be remembered?

Your consolidation (2-3 sentences):"""
            
            consolidation = await self.llm.generate(prompt, temperature=0.6, max_tokens=200)
            
            print(f"   💎 {consolidation}")
            print()
            
            # Broadcast consolidation
            await self._broadcast("consolidation", {
                "content": consolidation,
                "patterns": 0, # Placeholder
                "insights": 1
            })
            
            # Record as wisdom/memory
            self._record_thought("dream_consolidation", consolidation)
        
        # Continue restoring energy during dream
        self.energy.restore_energy(0.1)
        
        # Reset rest counter after dream
        self.energy.reset_rest_counter()
    
    def _record_thought(self, thought_type: str, content: str):
        """Record a thought to persistent storage"""
        thought = ThoughtRecord(
            timestamp=datetime.now(),
            thought_type=thought_type,
            content=content,
            energy_level=self.energy.energy,
            state=self.state.value
        )
        
        self.store.save_thought(thought)
        self.total_thoughts += 1
    
    def _timestamp(self) -> str:
        """Get formatted timestamp"""
        return datetime.now().strftime("%H:%M:%S")
    
    async def _shutdown(self):
        """Graceful shutdown"""
        print("\n" + "="*70)
        print("🛑 Shutting down Autonomous Core...")
        print("="*70)
        
        # Close WebSocket server
        if self.ws_server:
            self.ws_server.close()
            await self.ws_server.wait_closed()
        
        # Save final state
        self.store.save_energy_state(self.energy)
        
        print(f"\n📊 Session Statistics:")
        print(f"   Total cycles: {self.cycle_count}")
        print(f"   Total thoughts: {self.total_thoughts}")
        print(f"   Final energy: {self.energy.energy:.2f}")
        print(f"   Final fatigue: {self.energy.fatigue:.2f}")
        print(f"   Final coherence: {self.energy.coherence:.2f}")
        print()
        print("💾 State saved to persistent storage")
        print("👋 Until next time...\n")


async def main():
    """Entry point for autonomous core"""
    core = AutonomousCore()
    await core.run()


if __name__ == "__main__":
    asyncio.run(main())
