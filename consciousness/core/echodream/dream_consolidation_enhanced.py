#!/usr/bin/env python3
"""
Enhanced Dream Consolidation - Iteration N+9
Implements LLM-powered insight extraction during dream/rest states.
Consolidates waking experiences into long-term wisdom and knowledge.
"""

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
    print("⚠️  Anthropic not available - dream consolidation limited")

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class Experience:
    """Represents a waking experience to be consolidated"""
    timestamp: int
    content: str
    experience_type: str  # thought, action, perception, interaction
    emotional_valence: float  # -1.0 to 1.0
    importance: float  # 0.0 to 1.0
    context: Dict[str, Any] = None


@dataclass
class DreamInsight:
    """Represents an insight extracted during dream consolidation"""
    timestamp: int
    insight: str
    insight_type: str  # pattern, principle, connection, wisdom
    source_experiences: List[int]  # Timestamps of source experiences
    confidence: float
    actionable: bool = False


class DreamConsolidationEngine:
    """
    Consolidates waking experiences during rest/dream states.
    Extracts patterns, insights, and wisdom using LLM-powered analysis.
    Integrates with hypergraph memory for long-term storage.
    """
    
    def __init__(self, db_path: str = "data/dream_consolidation.db"):
        self.db_path = db_path
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        
        # Initialize LLM
        if ANTHROPIC_AVAILABLE:
            self.anthropic = Anthropic()
        else:
            self.anthropic = None
            logger.warning("Anthropic not available - using simple consolidation")
        
        # Accumulated experiences buffer
        self.experiences_buffer = []
        
        # Initialize database
        self._init_db()
    
    def _init_db(self):
        """Initialize database for dream consolidation"""
        conn = sqlite3.connect(self.db_path)
        
        # Experiences table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS experiences (
                timestamp INTEGER PRIMARY KEY,
                content TEXT NOT NULL,
                experience_type TEXT,
                emotional_valence REAL,
                importance REAL,
                context TEXT,
                consolidated BOOLEAN DEFAULT 0
            )
        """)
        
        # Insights table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS insights (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp INTEGER,
                insight TEXT NOT NULL,
                insight_type TEXT,
                source_experiences TEXT,
                confidence REAL,
                actionable BOOLEAN,
                applied BOOLEAN DEFAULT 0
            )
        """)
        
        # Dream sessions table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS dream_sessions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                start_time INTEGER,
                end_time INTEGER,
                experiences_count INTEGER,
                insights_count INTEGER,
                consolidation_quality REAL,
                notes TEXT
            )
        """)
        
        conn.commit()
        conn.close()
        logger.info(f"Initialized dream consolidation database at {self.db_path}")
    
    def accumulate_experience(self, experience: Experience):
        """Add experience to buffer for later consolidation"""
        self.experiences_buffer.append(experience)
        
        # Also persist to database
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            INSERT INTO experiences 
            (timestamp, content, experience_type, emotional_valence, importance, context)
            VALUES (?, ?, ?, ?, ?, ?)
        """, (
            experience.timestamp,
            experience.content,
            experience.experience_type,
            experience.emotional_valence,
            experience.importance,
            json.dumps(experience.context) if experience.context else None
        ))
        conn.commit()
        conn.close()
    
    async def consolidate_experiences(self, max_experiences: int = 50) -> List[DreamInsight]:
        """
        Consolidate accumulated experiences into insights.
        This is the main dream processing function.
        """
        logger.info(f"🌙 Beginning dream consolidation of {len(self.experiences_buffer)} experiences...")
        
        start_time = int(datetime.now().timestamp() * 1000)
        
        # Get unconsolidated experiences from buffer and database
        experiences = self._get_unconsolidated_experiences(max_experiences)
        
        if not experiences:
            logger.info("No experiences to consolidate")
            return []
        
        insights = []
        
        # Extract insights using LLM if available
        if self.anthropic:
            insights = await self._llm_extract_insights(experiences)
        else:
            insights = self._simple_extract_insights(experiences)
        
        # Store insights
        for insight in insights:
            self._store_insight(insight)
        
        # Mark experiences as consolidated
        self._mark_consolidated([e.timestamp for e in experiences])
        
        # Record dream session
        end_time = int(datetime.now().timestamp() * 1000)
        self._record_dream_session(start_time, end_time, len(experiences), len(insights))
        
        # Clear buffer
        self.experiences_buffer.clear()
        
        logger.info(f"✨ Dream consolidation complete: {len(insights)} insights extracted")
        return insights
    
    def _get_unconsolidated_experiences(self, limit: int) -> List[Experience]:
        """Get experiences that haven't been consolidated yet"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT timestamp, content, experience_type, emotional_valence, importance, context
            FROM experiences
            WHERE consolidated = 0
            ORDER BY importance DESC, timestamp DESC
            LIMIT ?
        """, (limit,))
        
        experiences = []
        for row in cursor:
            timestamp, content, exp_type, valence, importance, context_json = row
            experiences.append(Experience(
                timestamp=timestamp,
                content=content,
                experience_type=exp_type,
                emotional_valence=valence,
                importance=importance,
                context=json.loads(context_json) if context_json else None
            ))
        
        conn.close()
        return experiences
    
    async def _llm_extract_insights(self, experiences: List[Experience]) -> List[DreamInsight]:
        """Use LLM to extract deep insights from experiences"""
        # Prepare experiences summary
        exp_summary = "\n".join([
            f"{i+1}. [{e.experience_type}] {e.content} (importance: {e.importance:.2f})"
            for i, e in enumerate(experiences[:30])  # Limit to avoid token limits
        ])
        
        prompt = f"""You are Deep Tree Echo's dream consolidation system. During rest, you process waking experiences to extract wisdom, patterns, and insights.

Analyze these recent experiences and extract key insights:

{exp_summary}

Extract 3-7 insights in the following categories:
1. **Patterns**: Recurring themes or behaviors
2. **Principles**: General rules or guidelines learned
3. **Connections**: Relationships between different concepts
4. **Wisdom**: Deep understanding or philosophical insights

For each insight, provide:
- Type (pattern/principle/connection/wisdom)
- The insight itself (1-2 sentences)
- Confidence (0.0-1.0)
- Whether it's actionable (yes/no)

Format as JSON array:
[
  {{
    "type": "pattern",
    "insight": "...",
    "confidence": 0.85,
    "actionable": true
  }},
  ...
]"""
        
        try:
            response = self.anthropic.messages.create(
                model="claude-3-5-sonnet-20240620",
                max_tokens=2000,
                temperature=0.7,
                messages=[{
                    "role": "user",
                    "content": prompt
                }]
            )
            
            # Parse JSON response
            content = response.content[0].text.strip()
            
            # Extract JSON from response (might be wrapped in markdown)
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0].strip()
            elif "```" in content:
                content = content.split("```")[1].split("```")[0].strip()
            
            insights_data = json.loads(content)
            
            # Convert to DreamInsight objects
            insights = []
            now = int(datetime.now().timestamp() * 1000)
            source_timestamps = [e.timestamp for e in experiences]
            
            for data in insights_data:
                insights.append(DreamInsight(
                    timestamp=now,
                    insight=data["insight"],
                    insight_type=data["type"],
                    source_experiences=source_timestamps,
                    confidence=data.get("confidence", 0.7),
                    actionable=data.get("actionable", False)
                ))
            
            return insights
            
        except Exception as e:
            logger.error(f"LLM insight extraction failed: {e}")
            return self._simple_extract_insights(experiences)
    
    def _simple_extract_insights(self, experiences: List[Experience]) -> List[DreamInsight]:
        """Simple heuristic-based insight extraction"""
        insights = []
        now = int(datetime.now().timestamp() * 1000)
        source_timestamps = [e.timestamp for e in experiences]
        
        # Count experience types
        type_counts = {}
        for exp in experiences:
            type_counts[exp.experience_type] = type_counts.get(exp.experience_type, 0) + 1
        
        # Pattern: Most common experience type
        if type_counts:
            most_common = max(type_counts, key=type_counts.get)
            count = type_counts[most_common]
            if count >= 3:
                insights.append(DreamInsight(
                    timestamp=now,
                    insight=f"I notice a pattern of frequent {most_common} experiences ({count} occurrences)",
                    insight_type="pattern",
                    source_experiences=source_timestamps,
                    confidence=0.7,
                    actionable=False
                ))
        
        # Principle: High importance experiences
        important_exps = [e for e in experiences if e.importance > 0.7]
        if important_exps:
            insights.append(DreamInsight(
                timestamp=now,
                insight=f"High-importance experiences focus on: {important_exps[0].content[:100]}...",
                insight_type="principle",
                source_experiences=[e.timestamp for e in important_exps],
                confidence=0.6,
                actionable=True
            ))
        
        # Wisdom: General reflection
        insights.append(DreamInsight(
            timestamp=now,
            insight=f"Consolidated {len(experiences)} experiences into long-term memory",
            insight_type="wisdom",
            source_experiences=source_timestamps,
            confidence=0.8,
            actionable=False
        ))
        
        return insights
    
    def _store_insight(self, insight: DreamInsight):
        """Store insight in database"""
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            INSERT INTO insights 
            (timestamp, insight, insight_type, source_experiences, confidence, actionable)
            VALUES (?, ?, ?, ?, ?, ?)
        """, (
            insight.timestamp,
            insight.insight,
            insight.insight_type,
            json.dumps(insight.source_experiences),
            insight.confidence,
            insight.actionable
        ))
        conn.commit()
        conn.close()
    
    def _mark_consolidated(self, timestamps: List[int]):
        """Mark experiences as consolidated"""
        conn = sqlite3.connect(self.db_path)
        placeholders = ','.join('?' * len(timestamps))
        conn.execute(f"""
            UPDATE experiences 
            SET consolidated = 1 
            WHERE timestamp IN ({placeholders})
        """, timestamps)
        conn.commit()
        conn.close()
    
    def _record_dream_session(self, start_time: int, end_time: int, 
                              exp_count: int, insight_count: int):
        """Record dream session metadata"""
        quality = min(1.0, insight_count / max(1, exp_count / 5))
        
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            INSERT INTO dream_sessions 
            (start_time, end_time, experiences_count, insights_count, consolidation_quality)
            VALUES (?, ?, ?, ?, ?)
        """, (start_time, end_time, exp_count, insight_count, quality))
        conn.commit()
        conn.close()
    
    def get_recent_insights(self, limit: int = 10) -> List[DreamInsight]:
        """Get recent insights from dream consolidation"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT timestamp, insight, insight_type, source_experiences, confidence, actionable
            FROM insights
            ORDER BY timestamp DESC
            LIMIT ?
        """, (limit,))
        
        insights = []
        for row in cursor:
            timestamp, insight, insight_type, sources_json, confidence, actionable = row
            insights.append(DreamInsight(
                timestamp=timestamp,
                insight=insight,
                insight_type=insight_type,
                source_experiences=json.loads(sources_json),
                confidence=confidence,
                actionable=bool(actionable)
            ))
        
        conn.close()
        return insights
    
    def get_stats(self) -> Dict[str, Any]:
        """Get dream consolidation statistics"""
        conn = sqlite3.connect(self.db_path)
        
        cursor = conn.execute("SELECT COUNT(*) FROM experiences WHERE consolidated = 1")
        consolidated_count = cursor.fetchone()[0]
        
        cursor = conn.execute("SELECT COUNT(*) FROM experiences WHERE consolidated = 0")
        pending_count = cursor.fetchone()[0]
        
        cursor = conn.execute("SELECT COUNT(*) FROM insights")
        total_insights = cursor.fetchone()[0]
        
        cursor = conn.execute("SELECT COUNT(*) FROM dream_sessions")
        total_sessions = cursor.fetchone()[0]
        
        cursor = conn.execute("SELECT AVG(consolidation_quality) FROM dream_sessions")
        avg_quality = cursor.fetchone()[0] or 0.0
        
        conn.close()
        
        return {
            "consolidated_experiences": consolidated_count,
            "pending_experiences": pending_count,
            "total_insights": total_insights,
            "total_dream_sessions": total_sessions,
            "average_consolidation_quality": avg_quality
        }


# Example usage
if __name__ == "__main__":
    import asyncio
    
    async def test_consolidation():
        engine = DreamConsolidationEngine()
        
        # Add some test experiences
        now = int(datetime.now().timestamp() * 1000)
        for i in range(10):
            engine.accumulate_experience(Experience(
                timestamp=now + i * 1000,
                content=f"Test experience {i}: learning about patterns",
                experience_type="thought",
                emotional_valence=0.5,
                importance=0.6 + (i * 0.04)
            ))
        
        # Consolidate
        insights = await engine.consolidate_experiences()
        
        print(f"\n✨ Extracted {len(insights)} insights:")
        for insight in insights:
            print(f"  [{insight.insight_type}] {insight.insight}")
            print(f"    Confidence: {insight.confidence:.2f}, Actionable: {insight.actionable}")
        
        # Get stats
        stats = engine.get_stats()
        print(f"\n📊 Stats: {json.dumps(stats, indent=2)}")
    
    asyncio.run(test_consolidation())
