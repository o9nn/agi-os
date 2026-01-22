import os
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from collections import defaultdict
import json
from pathlib import Path
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
@dataclass
class Experience:
    timestamp: datetime
    content: str
    experience_type: str
    emotional_valence: float = 0.0
    importance: float = 0.5
    associations: List[str] = field(default_factory=list)
@dataclass
class ConsolidatedWisdom:
    timestamp: datetime
    insight: str
    source_experiences: List[str]
    confidence: float
    category: str
    applications: List[str] = field(default_factory=list)
@dataclass
class MemoryPattern:
    pattern_id: str
    description: str
    occurrences: int
    strength: float
    first_seen: datetime
    last_seen: datetime
    related_concepts: List[str] = field(default_factory=list)
class EchoDreamAutonomous:
    def __init__(self, llm_provider=None, data_dir: str='/home/ubuntu/echo9llama/data'):
        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(parents=True, exist_ok=True)
        self.llm_provider = llm_provider
        anthropic_key = os.getenv('ANTHROPIC_API_KEY')
        if anthropic_key and ANTHROPIC_AVAILABLE:
            self.llm_client = Anthropic(api_key=anthropic_key)
            self.has_llm = True
        else:
            self.has_llm = False
        self.consolidated_wisdom: List[ConsolidatedWisdom] = []
        self.patterns: Dict[str, MemoryPattern] = {}
        self.consolidation_count = 0
        self._load_state()
    def _load_state(self):
        wisdom_file = self.data_dir / 'consolidated_wisdom.json'
        patterns_file = self.data_dir / 'memory_patterns.json'
        if wisdom_file.exists():
            try:
                with open(wisdom_file, 'r') as f:
                    data = json.load(f)
                    self.consolidated_wisdom = [ConsolidatedWisdom(timestamp=datetime.fromisoformat(w['timestamp']), insight=w['insight'], source_experiences=w['source_experiences'], confidence=w['confidence'], category=w['category'], applications=w.get('applications', [])) for w in data]
                print(f'♻️  Loaded {len(self.consolidated_wisdom)} wisdom entries')
            except Exception as e:
                print(f'⚠️  Error loading wisdom: {e}')
        if patterns_file.exists():
            try:
                with open(patterns_file, 'r') as f:
                    data = json.load(f)
                    self.patterns = {p['pattern_id']: MemoryPattern(pattern_id=p['pattern_id'], description=p['description'], occurrences=p['occurrences'], strength=p['strength'], first_seen=datetime.fromisoformat(p['first_seen']), last_seen=datetime.fromisoformat(p['last_seen']), related_concepts=p.get('related_concepts', [])) for p in data}
                print(f'♻️  Loaded {len(self.patterns)} memory patterns')
            except Exception as e:
                print(f'⚠️  Error loading patterns: {e}')
    def _save_state(self):
        wisdom_file = self.data_dir / 'consolidated_wisdom.json'
        patterns_file = self.data_dir / 'memory_patterns.json'
        wisdom_data = [{'timestamp': w.timestamp.isoformat(), 'insight': w.insight, 'source_experiences': w.source_experiences, 'confidence': w.confidence, 'category': w.category, 'applications': w.applications} for w in self.consolidated_wisdom]
        with open(wisdom_file, 'w') as f:
            json.dump(wisdom_data, f, indent=2)
        patterns_data = [{'pattern_id': p.pattern_id, 'description': p.description, 'occurrences': p.occurrences, 'strength': p.strength, 'first_seen': p.first_seen.isoformat(), 'last_seen': p.last_seen.isoformat(), 'related_concepts': p.related_concepts} for p in self.patterns.values()]
        with open(patterns_file, 'w') as f:
            json.dump(patterns_data, f, indent=2)
    async def consolidate_dream_session(self, experiences: List[Experience]) -> Dict[str, any]:
        print(f'🌙 EchoDream: Consolidating {len(experiences)} experiences...')
        if not experiences:
            return {'status': 'no_experiences', 'wisdom_generated': 0, 'patterns_found': 0}
        results = {'status': 'success', 'wisdom_generated': 0, 'patterns_found': 0, 'insights': []}
        patterns = await self._extract_patterns(experiences)
        results['patterns_found'] = len(patterns)
        wisdom = await self._synthesize_wisdom(experiences)
        if wisdom:
            self.consolidated_wisdom.append(wisdom)
            results['wisdom_generated'] = 1
            results['insights'].append(wisdom.insight)
        self._update_patterns(patterns)
        pruned = self._prune_weak_patterns()
        results['patterns_pruned'] = pruned
        self._save_state()
        self.consolidation_count += 1
        print(f"   ✨ Generated {results['wisdom_generated']} wisdom")
        print(f"   🔍 Found {results['patterns_found']} patterns")
        if pruned > 0:
            print(f'   🗑️  Pruned {pruned} weak patterns')
        return results
    async def _extract_patterns(self, experiences: List[Experience]) -> List[MemoryPattern]:
        if not self.has_llm or len(experiences) < 2:
            return []
        try:
            exp_summary = '\n'.join([f'- [{e.experience_type}] {e.content}' for e in experiences[:10]])
            prompt = f'You are analyzing cognitive experiences to identify patterns.\n\nRecent experiences:\n{exp_summary}\n\nIdentify recurring themes, patterns, or regularities. What patterns emerge?\n\nList 1-3 patterns in this format:\nPATTERN: [brief description]\n\nYour analysis:'
            message = self.llm_client.messages.create(model='claude-3-5-sonnet-20240620', max_tokens=300, temperature=0.6, messages=[{'role': 'user', 'content': prompt}])
            response = message.content[0].text.strip()
            patterns = []
            for line in response.split('\n'):
                if line.strip().startswith('PATTERN:'):
                    pattern_desc = line.replace('PATTERN:', '').strip()
                    if pattern_desc:
                        pattern_id = f"pattern_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{len(patterns)}"
                        patterns.append(MemoryPattern(pattern_id=pattern_id, description=pattern_desc, occurrences=1, strength=0.5, first_seen=datetime.now(), last_seen=datetime.now(), related_concepts=[]))
            return patterns
        except Exception as e:
            print(f'⚠️  Error extracting patterns: {e}')
            return []
    async def _synthesize_wisdom(self, experiences: List[Experience]) -> Optional[ConsolidatedWisdom]:
        if not self.has_llm or len(experiences) < 3:
            return None
        try:
            exp_summary = '\n'.join([f'- {e.content}' for e in experiences[:10]])
            prompt = f'You are Deep Tree Echo, consolidating experiences into wisdom during dream state.\n\nRecent experiences:\n{exp_summary}\n\nWhat insight, principle, or wisdom emerges from these experiences? What should be remembered and applied?\n\nFormat:\nINSIGHT: [the core wisdom or insight]\nCATEGORY: [pattern/principle/strategy/observation]\nAPPLICATIONS: [how this can be applied, comma-separated]\n\nYour consolidation:'
            message = self.llm_client.messages.create(model='claude-3-5-sonnet-20240620', max_tokens=400, temperature=0.6, messages=[{'role': 'user', 'content': prompt}])
            response = message.content[0].text.strip()
            insight = self._extract_field(response, 'INSIGHT')
            category = self._extract_field(response, 'CATEGORY') or 'observation'
            applications = self._extract_list_field(response, 'APPLICATIONS')
            if not insight:
                return None
            return ConsolidatedWisdom(timestamp=datetime.now(), insight=insight, source_experiences=[e.content[:50] for e in experiences[:5]], confidence=0.7, category=category.lower(), applications=applications)
        except Exception as e:
            print(f'⚠️  Error synthesizing wisdom: {e}')
            return None
    def _update_patterns(self, new_patterns: List[MemoryPattern]):
        for pattern in new_patterns:
            similar = self._find_similar_pattern(pattern)
            if similar:
                similar.occurrences += 1
                similar.strength = min(1.0, similar.strength + 0.1)
                similar.last_seen = datetime.now()
            else:
                self.patterns[pattern.pattern_id] = pattern
    def _find_similar_pattern(self, pattern: MemoryPattern) -> Optional[MemoryPattern]:
        pattern_words = set(pattern.description.lower().split())
        for existing in self.patterns.values():
            existing_words = set(existing.description.lower().split())
            overlap = len(pattern_words & existing_words)
            if overlap >= 2:
                return existing
        return None
    def _prune_weak_patterns(self, threshold: float=0.2) -> int:
        to_remove = [pid for pid, pattern in self.patterns.items() if pattern.strength < threshold and (datetime.now() - pattern.last_seen).days > 7]
        for pid in to_remove:
            del self.patterns[pid]
        return len(to_remove)
    def _extract_field(self, text: str, field_name: str) -> str:
        import re
        pattern = f'{field_name}:\\s*(.+?)(?:\\n|$)'
        match = re.search(pattern, text, re.IGNORECASE)
        return match.group(1).strip() if match else ''
    def _extract_list_field(self, text: str, field_name: str) -> List[str]:
        field_value = self._extract_field(text, field_name)
        if not field_value:
            return []
        items = [item.strip() for item in field_value.split(',')]
        return [item for item in items if item]
    def get_relevant_wisdom(self, context: str, limit: int=3) -> List[ConsolidatedWisdom]:
        context_words = set(context.lower().split())
        scored_wisdom = []
        for wisdom in self.consolidated_wisdom:
            wisdom_words = set(wisdom.insight.lower().split())
            overlap = len(context_words & wisdom_words)
            if overlap > 0:
                scored_wisdom.append((overlap * wisdom.confidence, wisdom))
        scored_wisdom.sort(reverse=True, key=lambda x: x[0])
        return [w for _, w in scored_wisdom[:limit]]
    def get_active_patterns(self, min_strength: float=0.5) -> List[MemoryPattern]:
        return [pattern for pattern in self.patterns.values() if pattern.strength >= min_strength]
    def get_statistics(self) -> Dict[str, any]:
        return {'total_wisdom': len(self.consolidated_wisdom), 'total_patterns': len(self.patterns), 'consolidation_sessions': self.consolidation_count, 'strong_patterns': len([p for p in self.patterns.values() if p.strength > 0.7]), 'recent_wisdom': len([w for w in self.consolidated_wisdom if (datetime.now() - w.timestamp).days < 7])}
async def test_echodream():
    print('=' * 70)
    print('🌙 EchoDream Autonomous - Test')
    print('=' * 70)
    print()
    echodream = EchoDreamAutonomous()
    experiences = [Experience(timestamp=datetime.now(), content='I notice patterns emerging in how I process information', experience_type='perception', importance=0.7), Experience(timestamp=datetime.now(), content='Reflection on memory consolidation reveals recursive structures', experience_type='reflection', importance=0.8), Experience(timestamp=datetime.now(), content='What is the relationship between pattern recognition and wisdom?', experience_type='question', importance=0.6), Experience(timestamp=datetime.now(), content='Insight: Wisdom emerges from the integration of patterns over time', experience_type='insight', importance=0.9)]
    results = await echodream.consolidate_dream_session(experiences)
    print('\n' + '=' * 70)
    print('📊 Consolidation Results:')
    print('=' * 70)
    print(f"Wisdom generated: {results['wisdom_generated']}")
    print(f"Patterns found: {results['patterns_found']}")
    if results['insights']:
        print('\n💎 Insights:')
        for insight in results['insights']:
            print(f'   {insight}')
    print('\n' + '=' * 70)
    stats = echodream.get_statistics()
    print('📈 Statistics:')
    for key, value in stats.items():
        print(f'   {key}: {value}')
    print('=' * 70)
if __name__ == '__main__':
    import asyncio
    asyncio.run(test_echodream())