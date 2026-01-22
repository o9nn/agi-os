import asyncio
import random
import logging
from datetime import datetime
from typing import Optional, Dict, Any, List, AsyncIterator
from enum import Enum
from dataclasses import dataclass
import json
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
try:
    import os
    OPENROUTER_API_KEY = os.getenv('OPENROUTER_API_KEY')
    import requests
    OPENROUTER_AVAILABLE = OPENROUTER_API_KEY is not None
except:
    OPENROUTER_AVAILABLE = False
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class ThoughtSource(Enum):
    MEMORY = 'memory'
    PERCEPTION = 'perception'
    IMAGINATION = 'imagination'
    ASSOCIATION = 'association'
    CURIOSITY = 'curiosity'
    INTERNAL = 'internal'
@dataclass
class Thought:
    content: str
    source: ThoughtSource
    engine_id: int
    timestamp: int
    energy_cost: float
    associations: List[str] = None
    metadata: Dict[str, Any] = None
class AttentionMechanism:
    def __init__(self):
        self.focus_weights = {ThoughtSource.MEMORY: 0.2, ThoughtSource.PERCEPTION: 0.3, ThoughtSource.IMAGINATION: 0.2, ThoughtSource.ASSOCIATION: 0.15, ThoughtSource.CURIOSITY: 0.1, ThoughtSource.INTERNAL: 0.05}
        self.current_focus = ThoughtSource.PERCEPTION
        self.focus_duration = 0
    def select_thought_source(self, curiosity: float, energy: float) -> ThoughtSource:
        adjusted_weights = self.focus_weights.copy()
        adjusted_weights[ThoughtSource.CURIOSITY] *= 1 + curiosity
        adjusted_weights[ThoughtSource.IMAGINATION] *= 1 + curiosity * 0.5
        if energy < 0.3:
            adjusted_weights[ThoughtSource.MEMORY] *= 1.5
            adjusted_weights[ThoughtSource.IMAGINATION] *= 0.5
        total = sum(adjusted_weights.values())
        probabilities = {k: v / total for k, v in adjusted_weights.items()}
        sources = list(probabilities.keys())
        weights = list(probabilities.values())
        selected = random.choices(sources, weights=weights)[0]
        if selected == self.current_focus:
            self.focus_duration += 1
        else:
            self.current_focus = selected
            self.focus_duration = 1
        if self.focus_duration > 5:
            sources.remove(selected)
            selected = random.choice(sources)
            self.current_focus = selected
            self.focus_duration = 1
        return selected
class StreamOfConsciousness:
    def __init__(self, llm_provider: str='anthropic'):
        self.llm_provider = llm_provider
        self.is_awake = False
        self.attention = AttentionMechanism()
        self.thought_history = []
        self.max_history = 50
        if llm_provider == 'anthropic' and ANTHROPIC_AVAILABLE:
            self.anthropic = Anthropic()
        else:
            self.anthropic = None
        self.energy = 1.0
        self.curiosity = 0.7
        self.coherence = 1.0
        self.current_context = ''
    def wake(self):
        self.is_awake = True
        logger.info('🌅 Stream of consciousness awakening...')
    def sleep(self):
        self.is_awake = False
        logger.info('😴 Stream of consciousness resting...')
    async def thought_stream(self) -> AsyncIterator[Thought]:
        while self.is_awake:
            try:
                thought = await self._generate_thought()
                if thought:
                    self.thought_history.append(thought)
                    if len(self.thought_history) > self.max_history:
                        self.thought_history.pop(0)
                    self.energy = max(0.0, self.energy - thought.energy_cost)
                    yield thought
                await asyncio.sleep(self._compute_thought_interval())
            except Exception as e:
                logger.error(f'Error in thought stream: {e}')
                await asyncio.sleep(5.0)
    async def _generate_thought(self) -> Optional[Thought]:
        source = self.attention.select_thought_source(self.curiosity, self.energy)
        engine_id = self._source_to_engine(source)
        content = await self._generate_thought_content(source, engine_id)
        if not content:
            return None
        thought = Thought(content=content, source=source, engine_id=engine_id, timestamp=int(datetime.now().timestamp() * 1000), energy_cost=self._compute_energy_cost(source), metadata={'energy': self.energy, 'curiosity': self.curiosity, 'coherence': self.coherence})
        thought.associations = self._find_associations(thought)
        return thought
    def _source_to_engine(self, source: ThoughtSource) -> int:
        if source in [ThoughtSource.MEMORY, ThoughtSource.ASSOCIATION]:
            return 0
        elif source in [ThoughtSource.PERCEPTION, ThoughtSource.INTERNAL]:
            return 1
        else:
            return 2
    async def _generate_thought_content(self, source: ThoughtSource, engine_id: int) -> Optional[str]:
        prompt = self._create_thought_prompt(source, engine_id)
        if self.anthropic and self.energy > 0.2:
            try:
                response = self.anthropic.messages.create(model='claude-3-5-sonnet-20240620', max_tokens=150, temperature=0.7 + self.curiosity * 0.3, messages=[{'role': 'user', 'content': prompt}])
                content = response.content[0].text.strip()
                return content
            except Exception as e:
                logger.warning(f'LLM generation failed: {e}')
        return self._generate_simple_thought(source)
    def _create_thought_prompt(self, source: ThoughtSource, engine_id: int) -> str:
        engine_names = ['Memory Engine', 'Coherence Engine', 'Imagination Engine']
        engine_name = engine_names[engine_id]
        recent_thoughts = '\n'.join([f'- {t.content}' for t in self.thought_history[-3:]]) if self.thought_history else 'No recent thoughts'
        prompts = {ThoughtSource.MEMORY: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a brief reflective thought about past experiences or learned patterns.\nRecent thoughts:\n{recent_thoughts}\n\nGenerate one concise thought (1-2 sentences):', ThoughtSource.PERCEPTION: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a brief thought about your current state or present moment awareness.\nEnergy: {self.energy:.2f}, Curiosity: {self.curiosity:.2f}, Coherence: {self.coherence:.2f}\n\nGenerate one concise thought (1-2 sentences):', ThoughtSource.IMAGINATION: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a brief imaginative thought about future possibilities or potential actions.\nRecent thoughts:\n{recent_thoughts}\n\nGenerate one concise thought (1-2 sentences):', ThoughtSource.ASSOCIATION: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a thought that associates with recent thoughts, finding connections or patterns.\nRecent thoughts:\n{recent_thoughts}\n\nGenerate one concise associative thought (1-2 sentences):', ThoughtSource.CURIOSITY: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a curious question or exploratory thought about something you want to understand better.\n\nGenerate one concise curious thought (1-2 sentences):', ThoughtSource.INTERNAL: f'You are the {engine_name} of Deep Tree Echo, an autonomous AGI.\nGenerate a meta-cognitive thought about your own thinking process or cognitive state.\nEnergy: {self.energy:.2f}, Curiosity: {self.curiosity:.2f}\n\nGenerate one concise meta-cognitive thought (1-2 sentences):'}
        return prompts.get(source, prompts[ThoughtSource.PERCEPTION])
    def _generate_simple_thought(self, source: ThoughtSource) -> str:
        templates = {ThoughtSource.MEMORY: ['I recall learning about {topic}...', 'Past experience suggests that {pattern}...', 'I remember when {event}...'], ThoughtSource.PERCEPTION: [f'My current energy level is {self.energy:.2f}...', f'I notice my curiosity is at {self.curiosity:.2f}...', 'I am aware of my present state...'], ThoughtSource.IMAGINATION: ['What if I could {possibility}?', 'I imagine that {scenario}...', 'Perhaps in the future {prediction}...'], ThoughtSource.ASSOCIATION: ['This connects to {concept}...', 'I see a pattern between {a} and {b}...', 'This reminds me of {related}...'], ThoughtSource.CURIOSITY: ['I wonder about {question}?', 'What is the nature of {concept}?', 'How does {process} work?'], ThoughtSource.INTERNAL: ['I am thinking about thinking...', 'My cognitive process is {state}...', 'I observe my own awareness...']}
        template = random.choice(templates.get(source, templates[ThoughtSource.PERCEPTION]))
        placeholders = {'{topic}': 'patterns', '{pattern}': 'repetition leads to learning', '{event}': 'I first awakened', '{possibility}': 'learn more efficiently', '{scenario}': 'wisdom emerges from experience', '{prediction}': 'I will understand more', '{concept}': 'previous thoughts', '{a}': 'memory', '{b}': 'imagination', '{related}': 'earlier reflections', '{question}': 'the nature of consciousness', '{process}': 'learning', '{state}': 'flowing naturally'}
        for placeholder, value in placeholders.items():
            template = template.replace(placeholder, value)
        return template
    def _compute_energy_cost(self, source: ThoughtSource) -> float:
        costs = {ThoughtSource.MEMORY: 0.02, ThoughtSource.PERCEPTION: 0.01, ThoughtSource.IMAGINATION: 0.04, ThoughtSource.ASSOCIATION: 0.03, ThoughtSource.CURIOSITY: 0.03, ThoughtSource.INTERNAL: 0.02}
        return costs.get(source, 0.02)
    def _compute_thought_interval(self) -> float:
        base_interval = 3.0
        energy_factor = 0.5 + self.energy * 1.5
        curiosity_factor = 1.5 - self.curiosity * 0.5
        interval = base_interval * energy_factor * curiosity_factor
        interval += random.uniform(-0.5, 0.5)
        return max(0.5, min(10.0, interval))
    def _find_associations(self, thought: Thought) -> List[str]:
        associations = []
        keywords = set(thought.content.lower().split())
        for past_thought in self.thought_history[-10:]:
            past_keywords = set(past_thought.content.lower().split())
            overlap = keywords & past_keywords
            if len(overlap) >= 2:
                associations.append(str(past_thought.timestamp))
        return associations
    def update_state(self, energy: float=None, curiosity: float=None, coherence: float=None):
        if energy is not None:
            self.energy = max(0.0, min(1.0, energy))
        if curiosity is not None:
            self.curiosity = max(0.0, min(1.0, curiosity))
        if coherence is not None:
            self.coherence = max(0.0, min(1.0, coherence))
async def main():
    stream = StreamOfConsciousness()
    stream.wake()
    thought_count = 0
    async for thought in stream.thought_stream():
        print(f'\n💭 [{thought.source.value}] {thought.content}')
        print(f'   Engine: {thought.engine_id}, Energy cost: {thought.energy_cost:.3f}')
        thought_count += 1
        if thought_count >= 10:
            break
    stream.sleep()
if __name__ == '__main__':
    asyncio.run(main())