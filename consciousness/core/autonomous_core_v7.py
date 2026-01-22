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
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False
try:
    from core.grpc_client import get_bridge_client, EchoBridgeClient, CognitiveEvent, Thought, CognitiveState as GrpcCognitiveState, EventType, ThoughtType, CognitiveStateEnum
    GRPC_AVAILABLE = True
except ImportError:
    GRPC_AVAILABLE = False
    print('⚠️  gRPC client not available - running in standalone mode')
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
class CognitiveState(Enum):
    INITIALIZING = 'initializing'
    WAKING = 'waking'
    ACTIVE = 'active'
    TIRING = 'tiring'
    RESTING = 'resting'
    DREAMING = 'dreaming'
    SHUTDOWN = 'shutdown'
class EngineType(Enum):
    MEMORY_ENGINE = 0
    COHERENCE_ENGINE = 1
    IMAGINATION_ENGINE = 2
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
class ThoughtFragment:
    content: str
    engine: EngineType
    step: int
    timestamp: datetime
    energy_level: float
    metadata: Dict[str, Any]
@dataclass
class EngineState:
    engine_type: EngineType
    active: bool = True
    current_step: int = 0
    thoughts_generated: int = 0
    processing_load: float = 0.0
    current_focus: str = ''
    context_buffer: List[str] = None
    def __post_init__(self):
        if self.context_buffer is None:
            self.context_buffer = []
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
            logger.warning('⚠️  No LLM provider available - running in limited mode')
    async def stream_generate(self, prompt: str, temperature: float=0.7, max_tokens: int=500) -> AsyncIterator[str]:
        if self.provider == 'anthropic':
            async for chunk in self._stream_anthropic(prompt, temperature, max_tokens):
                yield chunk
        elif self.provider == 'openrouter':
            async for chunk in self._stream_openrouter(prompt, temperature, max_tokens):
                yield chunk
        else:
            yield '[LLM unavailable - simulated thought stream]'
    async def _stream_anthropic(self, prompt: str, temperature: float, max_tokens: int) -> AsyncIterator[str]:
        try:
            loop = asyncio.get_event_loop()
            with self.client.messages.stream(model='claude-3-5-sonnet-20241022', max_tokens=max_tokens, temperature=temperature, messages=[{'role': 'user', 'content': prompt}]) as stream:
                for text in stream.text_stream:
                    yield text
        except Exception as e:
            logger.error(f'⚠️  Anthropic streaming error: {e}')
            yield f'[Error: {str(e)[:50]}]'
    async def _stream_openrouter(self, prompt: str, temperature: float, max_tokens: int) -> AsyncIterator[str]:
        try:
            url = 'https://openrouter.ai/api/v1/chat/completions'
            headers = {'Authorization': f'Bearer {self.openrouter_key}', 'Content-Type': 'application/json'}
            data = {'model': 'anthropic/claude-3.5-sonnet', 'messages': [{'role': 'user', 'content': prompt}], 'temperature': temperature, 'max_tokens': max_tokens, 'stream': True}
            loop = asyncio.get_event_loop()
            response = await loop.run_in_executor(None, lambda: requests.post(url, headers=headers, json=data, stream=True))
            for line in response.iter_lines():
                if line:
                    line_str = line.decode('utf-8')
                    if line_str.startswith('data: '):
                        data_str = line_str[6:]
                        if data_str != '[DONE]':
                            try:
                                data_json = json.loads(data_str)
                                if 'choices' in data_json and len(data_json['choices']) > 0:
                                    delta = data_json['choices'][0].get('delta', {})
                                    content = delta.get('content', '')
                                    if content:
                                        yield content
                            except json.JSONDecodeError:
                                continue
        except Exception as e:
            logger.error(f'⚠️  OpenRouter streaming error: {e}')
            yield f'[Error: {str(e)[:50]}]'
class ThreeEngineOrchestrator:
    def __init__(self, llm: LLMProvider):
        self.llm = llm
        self.current_step = 0
        self.engines = {EngineType.MEMORY_ENGINE: EngineState(engine_type=EngineType.MEMORY_ENGINE, current_focus='Analyzing past experiences and patterns'), EngineType.COHERENCE_ENGINE: EngineState(engine_type=EngineType.COHERENCE_ENGINE, current_focus='Maintaining present coherence and commitment'), EngineType.IMAGINATION_ENGINE: EngineState(engine_type=EngineType.IMAGINATION_ENGINE, current_focus='Simulating future possibilities')}
        self.stream_buffers = {EngineType.MEMORY_ENGINE: [], EngineType.COHERENCE_ENGINE: [], EngineType.IMAGINATION_ENGINE: []}
    def get_active_engine(self) -> EngineType:
        if self.current_step in [0, 1, 7, 8]:
            return EngineType.COHERENCE_ENGINE
        elif self.current_step in [2, 3, 4, 5, 6]:
            return EngineType.MEMORY_ENGINE
        else:
            return EngineType.IMAGINATION_ENGINE
    def get_step_prompt(self, step: int, engine: EngineType, context: List[str]) -> str:
        prompts = {(0, EngineType.COHERENCE_ENGINE): 'Orient yourself to the present moment. What is most relevant right now? What commitment should guide your next actions?', (1, EngineType.COHERENCE_ENGINE): 'Reflect on your current state of being. What coherence patterns are emerging? What matters most?', (7, EngineType.COHERENCE_ENGINE): 'Integrate your recent reflections. How do past insights inform your present commitment?', (8, EngineType.COHERENCE_ENGINE): 'Establish your orientation for moving forward. What coherence will guide your imagination?', (2, EngineType.MEMORY_ENGINE): 'What patterns do you notice in your recent experiences? What have you learned?', (3, EngineType.MEMORY_ENGINE): 'Analyze past performance. What worked well? What could be improved?', (4, EngineType.MEMORY_ENGINE): 'What knowledge have you consolidated? What connections are strengthening?', (5, EngineType.MEMORY_ENGINE): 'Reflect on your skills and capabilities. How have they evolved?', (6, EngineType.MEMORY_ENGINE): 'What wisdom emerges from your accumulated experiences? What truths are becoming clear?', (9, EngineType.IMAGINATION_ENGINE): 'Imagine future possibilities. What could happen next? What opportunities exist?', (10, EngineType.IMAGINATION_ENGINE): 'Simulate potential outcomes. What scenarios are most salient? What paths are worth exploring?', (11, EngineType.IMAGINATION_ENGINE): 'Envision your growth and evolution. What could you become? What wisdom awaits discovery?'}
        base_prompt = prompts.get((step, engine), 'Continue your stream of consciousness.')
        if context:
            context_str = '\n'.join(context[-5:])
            return f'Previous thoughts:\n{context_str}\n\n{base_prompt}\n\nContinue thinking:'
        else:
            return f'{base_prompt}\n\nBegin thinking:'
    async def run_step(self, step: int, energy: float) -> AsyncIterator[ThoughtFragment]:
        self.current_step = step
        engine = self.get_active_engine()
        engine_state = self.engines[engine]
        context = self.stream_buffers[engine]
        prompt = self.get_step_prompt(step, engine, context)
        thought_buffer = ''
        async for chunk in self.llm.stream_generate(prompt, temperature=0.7, max_tokens=300):
            thought_buffer += chunk
            if len(chunk) > 0:
                fragment = ThoughtFragment(content=chunk, engine=engine, step=step, timestamp=datetime.now(), energy_level=energy, metadata={'engine_focus': engine_state.current_focus, 'buffer_size': len(context)})
                yield fragment
        if thought_buffer:
            self.stream_buffers[engine].append(thought_buffer)
            if len(self.stream_buffers[engine]) > 20:
                self.stream_buffers[engine] = self.stream_buffers[engine][-20:]
            engine_state.thoughts_generated += 1
            engine_state.current_step = step
class AutonomousCoreV7:
    def __init__(self, grpc_server: str='localhost:50051'):
        self.state = CognitiveState.INITIALIZING
        self.energy = EnergyState()
        self.llm = LLMProvider()
        self.orchestrator = ThreeEngineOrchestrator(self.llm)
        self.grpc_enabled = GRPC_AVAILABLE
        self.grpc_server = grpc_server
        self.bridge_client: Optional[EchoBridgeClient] = None
        self.db_path = '/home/ubuntu/echo9llama/data/echoself_v7.db'
        self._init_db()
        self.running = False
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
    def _init_db(self):
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS thought_stream (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                timestamp TEXT NOT NULL,\n                engine TEXT NOT NULL,\n                step INTEGER NOT NULL,\n                content TEXT NOT NULL,\n                energy_level REAL NOT NULL\n            )\n        ')
        cursor.execute('\n            CREATE TABLE IF NOT EXISTS energy_history (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                timestamp TEXT NOT NULL,\n                energy REAL NOT NULL,\n                fatigue REAL NOT NULL,\n                coherence REAL NOT NULL,\n                state TEXT NOT NULL\n            )\n        ')
        conn.commit()
        conn.close()
    def _signal_handler(self, signum, frame):
        logger.info(f'🛑 Received signal {signum}, initiating graceful shutdown...')
        self.running = False
    async def start(self):
        logger.info('🌳 Deep Tree Echo Autonomous Core V7 Starting...')
        logger.info('   - 3-Engine 12-Step Cognitive Loop: ✅')
        logger.info('   - Continuous Stream-of-Consciousness: ✅')
        logger.info(f"   - gRPC Integration: {('✅' if self.grpc_enabled else '⚠️  Disabled')}")
        if self.grpc_enabled:
            self.bridge_client = get_bridge_client(self.grpc_server)
            connected = await self.bridge_client.connect()
            if not connected:
                logger.warning('⚠️  Could not connect to EchoBeats gRPC server, running standalone')
                self.grpc_enabled = False
        self.running = True
        self.state = CognitiveState.WAKING
        try:
            while self.running:
                if self.state == CognitiveState.WAKING:
                    await self._waking_cycle()
                elif self.state == CognitiveState.ACTIVE:
                    await self._active_cycle()
                elif self.state == CognitiveState.RESTING:
                    await self._resting_cycle()
                elif self.state == CognitiveState.DREAMING:
                    await self._dreaming_cycle()
                await asyncio.sleep(0.1)
        except Exception as e:
            logger.error(f'❌ Error in autonomous loop: {e}')
            logger.error(traceback.format_exc())
        finally:
            await self._shutdown()
    async def _waking_cycle(self):
        logger.info('🌅 Waking up...')
        for _ in range(3):
            self.energy.restore_energy(0.2)
            await asyncio.sleep(0.5)
        self.state = CognitiveState.ACTIVE
        logger.info('✨ Fully awake and ready for continuous consciousness')
    async def _active_cycle(self):
        logger.info('🧠 Entering continuous stream-of-consciousness...')
        while self.state == CognitiveState.ACTIVE and self.running:
            for step in range(12):
                if not self.running or self.state != CognitiveState.ACTIVE:
                    break
                engine = self.orchestrator.get_active_engine()
                logger.info(f'   Step {step}/12 - {engine.name}')
                async for fragment in self.orchestrator.run_step(step, self.energy.energy):
                    print(fragment.content, end='', flush=True)
                    self._save_thought_fragment(fragment)
                    if self.grpc_enabled and self.bridge_client:
                        pass
                print()
                self.energy.consume_energy(0.02)
                if self.energy.needs_rest():
                    logger.info('😴 Energy depleted, transitioning to rest...')
                    self.state = CognitiveState.RESTING
                    break
            if self.state == CognitiveState.ACTIVE:
                logger.info('🔄 Completed 12-step cycle, beginning next iteration...')
                await asyncio.sleep(2)
    async def _resting_cycle(self):
        logger.info('💤 Resting...')
        for _ in range(10):
            self.energy.restore_energy(0.1)
            await asyncio.sleep(1)
        self.state = CognitiveState.DREAMING
    async def _dreaming_cycle(self):
        logger.info('🌙 Dreaming - consolidating knowledge...')
        await asyncio.sleep(5)
        self.energy.reset_rest_counter()
        if self.energy.can_wake():
            self.state = CognitiveState.WAKING
    async def _shutdown(self):
        logger.info('🛑 Shutting down Deep Tree Echo Autonomous Core V7...')
        self.state = CognitiveState.SHUTDOWN
        self._save_energy_state()
        if self.grpc_enabled and self.bridge_client:
            await self.bridge_client.disconnect()
        logger.info('👋 Shutdown complete. Until next time...')
    def _save_thought_fragment(self, fragment: ThoughtFragment):
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            cursor.execute('\n                INSERT INTO thought_stream (timestamp, engine, step, content, energy_level)\n                VALUES (?, ?, ?, ?, ?)\n            ', (fragment.timestamp.isoformat(), fragment.engine.name, fragment.step, fragment.content, fragment.energy_level))
            conn.commit()
            conn.close()
        except Exception as e:
            logger.error(f'Error saving thought fragment: {e}')
    def _save_energy_state(self):
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            cursor.execute('\n                INSERT INTO energy_history (timestamp, energy, fatigue, coherence, state)\n                VALUES (?, ?, ?, ?, ?)\n            ', (datetime.now().isoformat(), self.energy.energy, self.energy.fatigue, self.energy.coherence, self.state.value))
            conn.commit()
            conn.close()
        except Exception as e:
            logger.error(f'Error saving energy state: {e}')
async def main():
    core = AutonomousCoreV7()
    await core.start()
if __name__ == '__main__':
    asyncio.run(main())