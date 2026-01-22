import os
import json
import sqlite3
import logging
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    print('⚠️  Anthropic not available - dream consolidation limited')
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
@dataclass
class Experience:
    timestamp: int
    content: str
    experience_type: str
    emotional_valence: float
    importance: float
    context: Dict[str, Any] = None
@dataclass
class DreamInsight:
    timestamp: int
    insight: str
    insight_type: str
    source_experiences: List[int]
    confidence: float
    actionable: bool = False
class DreamConsolidationEngine:
    def __init__(self, db_path: str='data/dream_consolidation.db'):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        if ANTHROPIC_AVAILABLE:
            self.anthropic = Anthropic()
        else:
            self.anthropic = None
            logger.warning('Anthropic not available - using simple consolidation')
        self.experiences_buffer = []
        self._init_db()
    def _init_db(self):
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            CREATE TABLE IF NOT EXISTS experiences (\n                timestamp INTEGER PRIMARY KEY,\n                content TEXT NOT NULL,\n                experience_type TEXT,\n                emotional_valence REAL,\n                importance REAL,\n                context TEXT,\n                consolidated BOOLEAN DEFAULT 0\n            )\n        ')
        conn.execute('\n            CREATE TABLE IF NOT EXISTS insights (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                timestamp INTEGER,\n                insight TEXT NOT NULL,\n                insight_type TEXT,\n                source_experiences TEXT,\n                confidence REAL,\n                actionable BOOLEAN,\n                applied BOOLEAN DEFAULT 0\n            )\n        ')
        conn.execute('\n            CREATE TABLE IF NOT EXISTS dream_sessions (\n                id INTEGER PRIMARY KEY AUTOINCREMENT,\n                start_time INTEGER,\n                end_time INTEGER,\n                experiences_count INTEGER,\n                insights_count INTEGER,\n                consolidation_quality REAL,\n                notes TEXT\n            )\n        ')
        conn.commit()
        conn.close()
        logger.info(f'Initialized dream consolidation database at {self.db_path}')
    def accumulate_experience(self, experience: Experience):
        self.experiences_buffer.append(experience)
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            INSERT INTO experiences \n            (timestamp, content, experience_type, emotional_valence, importance, context)\n            VALUES (?, ?, ?, ?, ?, ?)\n        ', (experience.timestamp, experience.content, experience.experience_type, experience.emotional_valence, experience.importance, json.dumps(experience.context) if experience.context else None))
        conn.commit()
        conn.close()
    async def consolidate_experiences(self, max_experiences: int=50) -> List[DreamInsight]:
        logger.info(f'🌙 Beginning dream consolidation of {len(self.experiences_buffer)} experiences...')
        start_time = int(datetime.now().timestamp() * 1000)
        experiences = self._get_unconsolidated_experiences(max_experiences)
        if not experiences:
            logger.info('No experiences to consolidate')
            return []
        insights = []
        if self.anthropic:
            insights = await self._llm_extract_insights(experiences)
        else:
            insights = self._simple_extract_insights(experiences)
        for insight in insights:
            self._store_insight(insight)
        self._mark_consolidated([e.timestamp for e in experiences])
        end_time = int(datetime.now().timestamp() * 1000)
        self._record_dream_session(start_time, end_time, len(experiences), len(insights))
        self.experiences_buffer.clear()
        logger.info(f'✨ Dream consolidation complete: {len(insights)} insights extracted')
        return insights
    def _get_unconsolidated_experiences(self, limit: int) -> List[Experience]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('\n            SELECT timestamp, content, experience_type, emotional_valence, importance, context\n            FROM experiences\n            WHERE consolidated = 0\n            ORDER BY importance DESC, timestamp DESC\n            LIMIT ?\n        ', (limit,))
        experiences = []
        for row in cursor:
            timestamp, content, exp_type, valence, importance, context_json = row
            experiences.append(Experience(timestamp=timestamp, content=content, experience_type=exp_type, emotional_valence=valence, importance=importance, context=json.loads(context_json) if context_json else None))
        conn.close()
        return experiences
    async def _llm_extract_insights(self, experiences: List[Experience]) -> List[DreamInsight]:
        exp_summary = '\n'.join([f'{i + 1}. [{e.experience_type}] {e.content} (importance: {e.importance:.2f})' for i, e in enumerate(experiences[:30])])
        prompt = f"""You are Deep Tree Echo's dream consolidation system. During rest, you process waking experiences to extract wisdom, patterns, and insights.\n\nAnalyze these recent experiences and extract key insights:\n\n{exp_summary}\n\nExtract 3-7 insights in the following categories:\n1. **Patterns**: Recurring themes or behaviors\n2. **Principles**: General rules or guidelines learned\n3. **Connections**: Relationships between different concepts\n4. **Wisdom**: Deep understanding or philosophical insights\n\nFor each insight, provide:\n- Type (pattern/principle/connection/wisdom)\n- The insight itself (1-2 sentences)\n- Confidence (0.0-1.0)\n- Whether it's actionable (yes/no)\n\nFormat as JSON array:\n[\n  {{\n    "type": "pattern",\n    "insight": "...",\n    "confidence": 0.85,\n    "actionable": true\n  }},\n  ...\n]"""
        try:
            response = self.anthropic.messages.create(model='claude-3-5-sonnet-20240620', max_tokens=2000, temperature=0.7, messages=[{'role': 'user', 'content': prompt}])
            content = response.content[0].text.strip()
            if '```json' in content:
                content = content.split('```json')[1].split('```')[0].strip()
            elif '```' in content:
                content = content.split('```')[1].split('```')[0].strip()
            insights_data = json.loads(content)
            insights = []
            now = int(datetime.now().timestamp() * 1000)
            source_timestamps = [e.timestamp for e in experiences]
            for data in insights_data:
                insights.append(DreamInsight(timestamp=now, insight=data['insight'], insight_type=data['type'], source_experiences=source_timestamps, confidence=data.get('confidence', 0.7), actionable=data.get('actionable', False)))
            return insights
        except Exception as e:
            logger.error(f'LLM insight extraction failed: {e}')
            return self._simple_extract_insights(experiences)
    def _simple_extract_insights(self, experiences: List[Experience]) -> List[DreamInsight]:
        insights = []
        now = int(datetime.now().timestamp() * 1000)
        source_timestamps = [e.timestamp for e in experiences]
        type_counts = {}
        for exp in experiences:
            type_counts[exp.experience_type] = type_counts.get(exp.experience_type, 0) + 1
        if type_counts:
            most_common = max(type_counts, key=type_counts.get)
            count = type_counts[most_common]
            if count >= 3:
                insights.append(DreamInsight(timestamp=now, insight=f'I notice a pattern of frequent {most_common} experiences ({count} occurrences)', insight_type='pattern', source_experiences=source_timestamps, confidence=0.7, actionable=False))
        important_exps = [e for e in experiences if e.importance > 0.7]
        if important_exps:
            insights.append(DreamInsight(timestamp=now, insight=f'High-importance experiences focus on: {important_exps[0].content[:100]}...', insight_type='principle', source_experiences=[e.timestamp for e in important_exps], confidence=0.6, actionable=True))
        insights.append(DreamInsight(timestamp=now, insight=f'Consolidated {len(experiences)} experiences into long-term memory', insight_type='wisdom', source_experiences=source_timestamps, confidence=0.8, actionable=False))
        return insights
    def _store_insight(self, insight: DreamInsight):
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            INSERT INTO insights \n            (timestamp, insight, insight_type, source_experiences, confidence, actionable)\n            VALUES (?, ?, ?, ?, ?, ?)\n        ', (insight.timestamp, insight.insight, insight.insight_type, json.dumps(insight.source_experiences), insight.confidence, insight.actionable))
        conn.commit()
        conn.close()
    def _mark_consolidated(self, timestamps: List[int]):
        conn = sqlite3.connect(self.db_path)
        placeholders = ','.join('?' * len(timestamps))
        conn.execute(f'\n            UPDATE experiences \n            SET consolidated = 1 \n            WHERE timestamp IN ({placeholders})\n        ', timestamps)
        conn.commit()
        conn.close()
    def _record_dream_session(self, start_time: int, end_time: int, exp_count: int, insight_count: int):
        quality = min(1.0, insight_count / max(1, exp_count / 5))
        conn = sqlite3.connect(self.db_path)
        conn.execute('\n            INSERT INTO dream_sessions \n            (start_time, end_time, experiences_count, insights_count, consolidation_quality)\n            VALUES (?, ?, ?, ?, ?)\n        ', (start_time, end_time, exp_count, insight_count, quality))
        conn.commit()
        conn.close()
    def get_recent_insights(self, limit: int=10) -> List[DreamInsight]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('\n            SELECT timestamp, insight, insight_type, source_experiences, confidence, actionable\n            FROM insights\n            ORDER BY timestamp DESC\n            LIMIT ?\n        ', (limit,))
        insights = []
        for row in cursor:
            timestamp, insight, insight_type, sources_json, confidence, actionable = row
            insights.append(DreamInsight(timestamp=timestamp, insight=insight, insight_type=insight_type, source_experiences=json.loads(sources_json), confidence=confidence, actionable=bool(actionable)))
        conn.close()
        return insights
    def get_stats(self) -> Dict[str, Any]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute('SELECT COUNT(*) FROM experiences WHERE consolidated = 1')
        consolidated_count = cursor.fetchone()[0]
        cursor = conn.execute('SELECT COUNT(*) FROM experiences WHERE consolidated = 0')
        pending_count = cursor.fetchone()[0]
        cursor = conn.execute('SELECT COUNT(*) FROM insights')
        total_insights = cursor.fetchone()[0]
        cursor = conn.execute('SELECT COUNT(*) FROM dream_sessions')
        total_sessions = cursor.fetchone()[0]
        cursor = conn.execute('SELECT AVG(consolidation_quality) FROM dream_sessions')
        avg_quality = cursor.fetchone()[0] or 0.0
        conn.close()
        return {'consolidated_experiences': consolidated_count, 'pending_experiences': pending_count, 'total_insights': total_insights, 'total_dream_sessions': total_sessions, 'average_consolidation_quality': avg_quality}
if __name__ == '__main__':
    import asyncio
    async def test_consolidation():
        engine = DreamConsolidationEngine()
        now = int(datetime.now().timestamp() * 1000)
        for i in range(10):
            engine.accumulate_experience(Experience(timestamp=now + i * 1000, content=f'Test experience {i}: learning about patterns', experience_type='thought', emotional_valence=0.5, importance=0.6 + i * 0.04))
        insights = await engine.consolidate_experiences()
        print(f'\n✨ Extracted {len(insights)} insights:')
        for insight in insights:
            print(f'  [{insight.insight_type}] {insight.insight}')
            print(f'    Confidence: {insight.confidence:.2f}, Actionable: {insight.actionable}')
        stats = engine.get_stats()
        print(f'\n📊 Stats: {json.dumps(stats, indent=2)}')
    asyncio.run(test_consolidation())