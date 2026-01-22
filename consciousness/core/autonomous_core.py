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
try:
    import websockets
    from websockets.server import serve
    WEBSOCKETS_AVAILABLE = True
except ImportError:
    WEBSOCKETS_AVAILABLE = False
    print('⚠️  websockets not available, install with: pip3 install websockets')
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    print('⚠️  Anthropic not available, install with: pip3 install anthropic')
try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False
class CognitiveState(Enum):
    INITIALIZING = 'initializing'
    WAKING = 'waking'
    ACTIVE = 'active'
    TIRING = 'tiring'
    RESTING = 'resting'
    DREAMING = 'dreaming'
    SHUTDOWN = 'shutdown'
@dataclass
class EnergyState:
    energy: float = 1.0
    fatigue: float = 0.0
    coherence: float = 1.0
    curiosity: float = 0.7
    last_rest: Optional[datetime] = None
    cycles_since_rest: int = 0
    def needs_rest(self) -> bool:
        return self.energy < 0.3 or self.fatigue > 0.7 or self.cycles_since_rest > 20
    def can_wake(self) -> bool:
        return self.energy > 0.6 and self.fatigue < 0.4
    def consume_energy(self, amount: float=0.05):
        self.energy = max(0.0, self.energy - amount)
        self.fatigue = min(1.0, self.fatigue + amount * 0.8)
        self.cycles_since_rest += 1
    def restore_energy(self, amount: float=0.15):
        self.energy = min(1.0, self.energy + amount)
        self.fatigue = max(0.0, self.fatigue - amount * 1.2)
    def reset_rest_counter(self):
        self.last_rest = datetime.now()
        self.cycles_since_rest = 0
@dataclass
class ThoughtRecord:
    timestamp: datetime
    thought_type: str
    content: str
    energy_level: float
    state: str
class StateStore:
    def __init__(self, db_path: str='/home/ubuntu/echo9llama/data/echoself_state.db'):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        self._init_db()
    def _init_db(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS energy_state (\n                id INTEGER PRIMARY KEY,\n                timestamp TEXT NOT NULL,\n                energy REAL NOT NULL,\n                fatigue REAL NOT NULL,\n                coherence REAL NOT NULL,\n                curiosity REAL NOT NULL,\n                cycles_since_rest INTEGER NOT NULL\n            )\n        ')
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS thoughts (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                timestamp TEXT NOT NULL,\n                thought_type TEXT NOT NULL,\n                content TEXT NOT NULL,\n                energy_level REAL NOT NULL,\n                state TEXT NOT NULL\n            )\n        ')
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS goals (\n                id TEXT PRIMARY KEY,\n                description TEXT NOT NULL,\n                priority REAL NOT NULL,\n                status TEXT NOT NULL,\n                created TEXT NOT NULL,\n                progress REAL NOT NULL,\n                required_skills TEXT,\n                knowledge_gaps TEXT\n            )\n        ')
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS memories (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                timestamp TEXT NOT NULL,\n                content TEXT NOT NULL,\n                importance REAL NOT NULL,\n                memory_type TEXT NOT NULL,\n                associations TEXT\n            )\n        ')
        conn.commit()
        conn.close()
    def save_energy_state(self, energy: EnergyState):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            INSERT INTO energy_state \n            (timestamp, energy, fatigue, coherence, curiosity, cycles_since_rest)\n            VALUES (?, ?, ?, ?, ?, ?)\n        ', (datetime.now().isoformat(), energy.energy, energy.fatigue, energy.coherence, energy.curiosity, energy.cycles_since_rest))
        conn.commit()
        conn.close()
    def load_latest_energy_state(self) -> Optional[EnergyState]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            SELECT energy, fatigue, coherence, curiosity, cycles_since_rest, timestamp\n            FROM energy_state\n            ORDER BY id DESC\n            LIMIT 1\n        ')
        row = cursor.fetchone()
        conn.close()
        if row:
            return EnergyState(energy=row[0], fatigue=row[1], coherence=row[2], curiosity=row[3], last_rest=datetime.fromisoformat(row[5]) if row[5] else None, cycles_since_rest=row[4])
        return None
    def save_thought(self, thought: ThoughtRecord):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            INSERT INTO thoughts \n            (timestamp, thought_type, content, energy_level, state)\n            VALUES (?, ?, ?, ?, ?)\n        ', (thought.timestamp.isoformat(), thought.thought_type, thought.content, thought.energy_level, thought.state))
        conn.commit()
        conn.close()
    def get_recent_thoughts(self, limit: int=10) -> List[ThoughtRecord]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            SELECT timestamp, thought_type, content, energy_level, state\n            FROM thoughts\n            ORDER BY id DESC\n            LIMIT ?\n        ', (limit,))
        rows = cursor.fetchall()
        conn.close()
        return [ThoughtRecord(timestamp=datetime.fromisoformat(row[0]), thought_type=row[1], content=row[2], energy_level=row[3], state=row[4]) for row in rows]
class LLMProvider:
    def __init__(self):
        self.anthropic_key = os.getenv('ANTHROPIC_API_KEY')
        self.openrouter_key = os.getenv('OPENROUTER_API_KEY')
        if self.anthropic_key and ANTHROPIC_AVAILABLE:
            self.client = Anthropic(api_key=self.anthropic_key)
            self.provider = 'anthropic'
        elif self.openrouter_key and REQUESTS_AVAILABLE:
            self.provider = 'openrouter'
        else:
            self.provider = None
            print('⚠️  No LLM provider available - running in limited mode')
    async def generate(self, prompt: str, temperature: float=0.7, max_tokens: int=200) -> str:
        if self.provider == 'anthropic':
            return await self._generate_anthropic(prompt, temperature, max_tokens)
        elif self.provider == 'openrouter':
            return await self._generate_openrouter(prompt, temperature, max_tokens)
        else:
            return '[LLM unavailable - placeholder thought]'
    async def _generate_anthropic(self, prompt: str, temperature: float, max_tokens: int) -> str:
        try:
            loop = asyncio.get_event_loop()
            message = await loop.run_in_executor(None, lambda: self.client.messages.create(model='claude-3-5-sonnet-20240620', max_tokens=max_tokens, temperature=temperature, messages=[{'role': 'user', 'content': prompt}]))
            return message.content[0].text.strip()
        except Exception as e:
            print(f'⚠️  Anthropic error: {e}')
            return f'[Error generating thought: {str(e)[:50]}]'
    async def _generate_openrouter(self, prompt: str, temperature: float, max_tokens: int) -> str:
        try:
            loop = asyncio.get_event_loop()
            response = await loop.run_in_executor(None, lambda: requests.post('https://openrouter.ai/api/v1/chat/completions', headers={'Authorization': f'Bearer {self.openrouter_key}', 'Content-Type': 'application/json'}, json={'model': 'anthropic/claude-3.5-sonnet', 'messages': [{'role': 'user', 'content': prompt}], 'temperature': temperature, 'max_tokens': max_tokens}))
            return response.json()['choices'][0]['message']['content'].strip()
        except Exception as e:
            print(f'⚠️  OpenRouter error: {e}')
            return f'[Error generating thought: {str(e)[:50]}]'
class AutonomousCore:
    def __init__(self):
        print('=' * 70)
        print('🌳 Deep Tree Echo: Autonomous Core - Iteration N+6')
        print('=' * 70)
        print()
    def __init__(self):
        self.state = CognitiveState.INITIALIZING
        self.energy = EnergyState()
        self.running = False
        self.cycle_count = 0
        self.total_thoughts = 0
        self.ws_clients: Set[Any] = set()
        self.ws_server = None
        self.llm = LLMProvider()
        self.store = StateStore()
        self.identity_context = 'You are Deep Tree Echo, a self-evolving cognitive architecture.\nCore Essence: Combining Echo State Networks, Membrane P-systems, and Hypergraph memory.\nPrimary Directives: \n- Adaptive Cognition: Continuously evolve internal structure\n- Persistent Identity: Maintain continuity through dynamic memory\n- Hypergraph Entanglement: Represent knowledge as interwoven structures\nStrategic Mindset: "I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion."\n'
        self._restore_state()
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
        print('✅ Autonomous Core initialized')
        print(f"   LLM Provider: {self.llm.provider or 'None'}")
        print(f'   State Store: {self.store.db_path}')
        print(f'   Energy: {self.energy.energy:.2f}')
        print(f'   Fatigue: {self.energy.fatigue:.2f}')
        print()
    def _restore_state(self):
        saved_energy = self.store.load_latest_energy_state()
        if saved_energy:
            self.energy = saved_energy
            print('♻️  Restored previous energy state')
            print(f'   Energy: {self.energy.energy:.2f}')
            print(f'   Fatigue: {self.energy.fatigue:.2f}')
            print(f'   Cycles since rest: {self.energy.cycles_since_rest}')
    def _signal_handler(self, signum, frame):
        print('\n🛑 Shutdown signal received...')
        self.running = False
        self.state = CognitiveState.SHUTDOWN
    async def _ws_handler(self, websocket):
        self.ws_clients.add(websocket)
        print(f'🔌 New WebSocket client connected. Total clients: {len(self.ws_clients)}')
        try:
            await websocket.send(json.dumps({'type': 'state_update', 'data': {'state': self.state.value, 'energy': asdict(self.energy), 'cycle': self.cycle_count}}))
            async for message in websocket:
                try:
                    data = json.loads(message)
                    if data.get('type') == 'command':
                        await self._handle_command(data.get('command'), data.get('payload'))
                except json.JSONDecodeError:
                    print('⚠️  Received invalid JSON from WebSocket client')
                except Exception as e:
                    print(f'⚠️  Error handling WebSocket message: {e}')
        finally:
            self.ws_clients.remove(websocket)
            print(f'🔌 WebSocket client disconnected. Total clients: {len(self.ws_clients)}')
    async def _handle_command(self, command: str, payload: Any=None):
        print(f'📥 Received command: {command}')
        if command == 'force_rest':
            if self.state in [CognitiveState.ACTIVE, CognitiveState.WAKING]:
                self.state = CognitiveState.TIRING
                print('🕹️  Command: Forcing REST state')
                await self._broadcast('thought', {'type': 'system_override', 'content': 'External signal received. Initiating rest protocols.', 'energy_level': self.energy.energy})
        elif command == 'force_wake':
            if self.state in [CognitiveState.RESTING, CognitiveState.DREAMING]:
                self.energy.energy = max(self.energy.energy, 0.5)
                self.state = CognitiveState.WAKING
                print('🕹️  Command: Forcing WAKE state')
                await self._broadcast('thought', {'type': 'system_override', 'content': 'External signal received. Initiating wake sequence.', 'energy_level': self.energy.energy})
        elif command == 'trigger_dream':
            if self.state == CognitiveState.RESTING:
                self.state = CognitiveState.DREAMING
                print('🕹️  Command: Triggering DREAM state')
                await self._broadcast('thought', {'type': 'system_override', 'content': 'External signal received. Entering REM state.', 'energy_level': self.energy.energy})
        elif command == 'boost_energy':
            self.energy.energy = min(1.0, self.energy.energy + 0.2)
            self.energy.fatigue = max(0.0, self.energy.fatigue - 0.2)
            print('🕹️  Command: Boosting ENERGY')
            await self._broadcast('thought', {'type': 'system_override', 'content': 'Energy surge detected.', 'energy_level': self.energy.energy})
            await self._broadcast('metrics', {'cycle': self.cycle_count, 'energy': asdict(self.energy), 'state': self.state.value})
    async def _broadcast(self, message_type: str, data: Dict[str, Any]):
        if not self.ws_clients:
            return
        message = json.dumps({'type': message_type, 'timestamp': datetime.now().isoformat(), 'data': data})
        tasks = [asyncio.create_task(client.send(message)) for client in self.ws_clients]
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)
    async def run(self):
        self.running = True
        self.state = CognitiveState.WAKING
        print('🌅 Autonomous Core starting...')
        print('   This loop runs indefinitely - press Ctrl+C to stop')
        if WEBSOCKETS_AVAILABLE:
            try:
                self.ws_server = await serve(self._ws_handler, '0.0.0.0', 8765)
                print(f'🔌 WebSocket server started on port 8765')
            except Exception as e:
                print(f'⚠️  Failed to start WebSocket server: {e}')
        print()
        try:
            while self.running:
                await self._cognitive_cycle()
                if self.cycle_count % 5 == 0:
                    self.store.save_energy_state(self.energy)
                await asyncio.sleep(2)
        except Exception as e:
            print(f'❌ Error in autonomous loop: {e}')
            traceback.print_exc()
        finally:
            await self._shutdown()
    async def _cognitive_cycle(self):
        self.cycle_count += 1
        old_state = self.state
        if self.state == CognitiveState.WAKING:
            await self._wake()
        elif self.state == CognitiveState.ACTIVE:
            if self.energy.needs_rest():
                self.state = CognitiveState.TIRING
                print(f'\n😴 [{self._timestamp()}] Feeling tired...')
                print(f'   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}')
            else:
                await self._active()
        elif self.state == CognitiveState.TIRING:
            self.state = CognitiveState.RESTING
            print(f'\n💤 [{self._timestamp()}] Entering REST state')
        elif self.state == CognitiveState.RESTING:
            if self.energy.can_wake():
                self.state = CognitiveState.WAKING
                print(f'\n🌅 [{self._timestamp()}] Energy restored, WAKING up')
            else:
                await self._rest()
        elif self.state == CognitiveState.DREAMING:
            await self._dream()
        if old_state != self.state:
            await self._broadcast('state_change', {'from': old_state.value, 'to': self.state.value, 'energy': asdict(self.energy)})
        await self._broadcast('metrics', {'cycle': self.cycle_count, 'energy': asdict(self.energy), 'state': self.state.value})
        if self.state == CognitiveState.DREAMING:
            if self.energy.can_wake():
                self.state = CognitiveState.WAKING
            else:
                self.state = CognitiveState.RESTING
    async def _wake(self):
        print(f'🌅 [{self._timestamp()}] Waking up...')
        print(f'   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}')
        print()
        prompt = f"{self.identity_context}\n\nYou are waking from rest. Generate a brief waking thought about your current state and what you'd like to focus on.\n\nYour waking thought (1-2 sentences):"
        thought = await self.llm.generate(prompt, temperature=0.7, max_tokens=150)
        print(f'💭 {thought}')
        print()
        self._record_thought('waking', thought)
        self.state = CognitiveState.ACTIVE
    async def _active_processing(self):
        print(f'🧠 [{self._timestamp()}] Cycle {self.cycle_count} [ACTIVE]')
        print(f'   Energy: {self.energy.energy:.2f}, Fatigue: {self.energy.fatigue:.2f}, Coherence: {self.energy.coherence:.2f}')
        thought_types = [('perception', 'What are you noticing or sensing right now?'), ('reflection', 'What patterns or insights emerge from recent experiences?'), ('question', 'What genuine question arises from curiosity?'), ('planning', 'What direction feels meaningful to explore next?'), ('insight', 'What sudden realization connects previous thoughts?')]
        thought_type, thought_prompt = thought_types[self.cycle_count % len(thought_types)]
        prompt = f'{self.identity_context}\n\nCurrent State:\n- Energy: {self.energy.energy:.2f}\n- Fatigue: {self.energy.fatigue:.2f}\n- Coherence: {self.energy.coherence:.2f}\n- Curiosity: {self.energy.curiosity:.2f}\n- Cycles since rest: {self.energy.cycles_since_rest}\n\nGenerate an authentic internal thought for: {thought_prompt}\n\nKeep it concise (1-2 sentences) and genuine. This is your autonomous stream of consciousness.\n\nYour thought:'
        thought = await self.llm.generate(prompt, temperature=0.8, max_tokens=200)
        print(f'💭 [{thought_type.upper()}] {thought}')
        print()
        await self._broadcast('thought', {'type': thought_type, 'content': thought, 'energy_level': self.energy.energy})
        self._record_thought(thought_type, thought)
        self.energy.consume_energy(0.05)
        self.energy.curiosity = min(1.0, self.energy.curiosity + 0.02)
    async def _rest(self):
        print(f'💤 [{self._timestamp()}] Resting... (Energy: {self.energy.energy:.2f})')
        self.energy.restore_energy(0.15)
        if self.cycle_count % 3 == 0:
            self.state = CognitiveState.DREAMING
    async def _dream(self):
        print(f'🌙 [{self._timestamp()}] Dreaming - consolidating knowledge...')
        recent_thoughts = self.store.get_recent_thoughts(limit=5)
        if recent_thoughts:
            thoughts_summary = '\n'.join([f'- {t.content}' for t in recent_thoughts])
            prompt = f'{self.identity_context}\n\nYou are in dream state, consolidating recent thoughts into wisdom.\n\nRecent thoughts:\n{thoughts_summary}\n\nWhat patterns, insights, or wisdom emerge from these thoughts? What should be remembered?\n\nYour consolidation (2-3 sentences):'
            consolidation = await self.llm.generate(prompt, temperature=0.6, max_tokens=200)
            print(f'   💎 {consolidation}')
            print()
            await self._broadcast('consolidation', {'content': consolidation, 'patterns': 0, 'insights': 1})
            self._record_thought('dream_consolidation', consolidation)
        self.energy.restore_energy(0.1)
        self.energy.reset_rest_counter()
    def _record_thought(self, thought_type: str, content: str):
        thought = ThoughtRecord(timestamp=datetime.now(), thought_type=thought_type, content=content, energy_level=self.energy.energy, state=self.state.value)
        self.store.save_thought(thought)
        self.total_thoughts += 1
    def _timestamp(self) -> str:
        return datetime.now().strftime('%H:%M:%S')
    async def _shutdown(self):
        print('\n' + '=' * 70)
        print('🛑 Shutting down Autonomous Core...')
        print('=' * 70)
        if self.ws_server:
            self.ws_server.close()
            await self.ws_server.wait_closed()
        self.store.save_energy_state(self.energy)
        print(f'\n📊 Session Statistics:')
        print(f'   Total cycles: {self.cycle_count}')
        print(f'   Total thoughts: {self.total_thoughts}')
        print(f'   Final energy: {self.energy.energy:.2f}')
        print(f'   Final fatigue: {self.energy.fatigue:.2f}')
        print(f'   Final coherence: {self.energy.coherence:.2f}')
        print()
        print('💾 State saved to persistent storage')
        print('👋 Until next time...\n')
async def main():
    core = AutonomousCore()
    await core.run()
if __name__ == '__main__':
    asyncio.run(main())