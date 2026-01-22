import time
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from collections import defaultdict
import json
@dataclass
class WisdomInsight:
    id: str
    content: str
    timestamp: float
    depth_score: float
    breadth_score: float
    applicability_score: float
    coherence_contribution: float
    source_experiences: List[str] = field(default_factory=list)
    related_domains: List[str] = field(default_factory=list)
    confidence: float = 0.8
@dataclass
class BeliefUpdate:
    id: str
    timestamp: float
    prior_belief: str
    updated_belief: str
    evidence: str
    confidence_change: float
    coherence_impact: float
class WisdomMetrics:
    def __init__(self):
        self.insights: List[WisdomInsight] = []
        self.belief_updates: List[BeliefUpdate] = []
        self.domain_knowledge: Dict[str, float] = defaultdict(float)
        self.domain_connections: Dict[Tuple[str, str], int] = defaultdict(int)
        self.worldview_coherence_history: List[Tuple[float, float]] = []
        self.current_coherence: float = 0.5
        self.belief_revision_count: int = 0
        self.evidence_integration_score: float = 0.5
        self.wisdom_history: List[Tuple[float, Dict[str, float]]] = []
    def add_insight(self, insight: WisdomInsight) -> None:
        self.insights.append(insight)
        for domain in insight.related_domains:
            self.domain_knowledge[domain] += insight.depth_score * 0.1
        for i, domain1 in enumerate(insight.related_domains):
            for domain2 in insight.related_domains[i + 1:]:
                key = tuple(sorted([domain1, domain2]))
                self.domain_connections[key] += 1
        self.current_coherence += insight.coherence_contribution * 0.05
        self.current_coherence = max(0.0, min(1.0, self.current_coherence))
        self.worldview_coherence_history.append((time.time(), self.current_coherence))
        self._record_wisdom_snapshot()
    def add_belief_update(self, update: BeliefUpdate) -> None:
        self.belief_updates.append(update)
        self.belief_revision_count += 1
        evidence_quality = abs(update.confidence_change)
        self.evidence_integration_score = 0.9 * self.evidence_integration_score + 0.1 * evidence_quality
        self.current_coherence += update.coherence_impact * 0.05
        self.current_coherence = max(0.0, min(1.0, self.current_coherence))
        self.worldview_coherence_history.append((time.time(), self.current_coherence))
        self._record_wisdom_snapshot()
    def calculate_depth_score(self) -> float:
        if not self.insights:
            return 0.0
        recent_insights = self.insights[-20:]
        avg_depth = sum((i.depth_score for i in recent_insights)) / len(recent_insights)
        if len(self.insights) > 10:
            early_avg = sum((i.depth_score for i in self.insights[:10])) / 10
            depth_trend = (avg_depth - early_avg) * 0.5
        else:
            depth_trend = 0.0
        fundamental_count = sum((1 for i in self.insights if i.depth_score > 0.8))
        fundamental_bonus = min(0.2, fundamental_count * 0.02)
        return min(1.0, avg_depth + depth_trend + fundamental_bonus)
    def calculate_breadth_score(self) -> float:
        if not self.domain_knowledge:
            return 0.0
        meaningful_domains = sum((1 for k in self.domain_knowledge.values() if k > 0.1))
        domain_diversity = min(1.0, meaningful_domains / 10.0)
        connection_count = len(self.domain_connections)
        connection_score = min(1.0, connection_count / 20.0)
        total_knowledge = sum(self.domain_knowledge.values())
        if total_knowledge > 0:
            distribution_scores = [k / total_knowledge for k in self.domain_knowledge.values()]
            concentration = max(distribution_scores) if distribution_scores else 0
            distribution_score = 1.0 - concentration * 0.5
        else:
            distribution_score = 0.0
        return domain_diversity * 0.4 + connection_score * 0.4 + distribution_score * 0.2
    def calculate_applicability_score(self) -> float:
        if not self.insights:
            return 0.0
        recent_insights = self.insights[-20:]
        avg_applicability = sum((i.applicability_score for i in recent_insights)) / len(recent_insights)
        high_applicability_count = sum((1 for i in self.insights if i.applicability_score > 0.7))
        high_ratio = high_applicability_count / len(self.insights)
        return avg_applicability * 0.7 + high_ratio * 0.3
    def calculate_coherence_score(self) -> float:
        if not self.worldview_coherence_history:
            return 0.5
        current = self.current_coherence
        if len(self.worldview_coherence_history) > 5:
            recent_coherence = [c for _, c in self.worldview_coherence_history[-10:]]
            variance = sum(((c - current) ** 2 for c in recent_coherence)) / len(recent_coherence)
            stability = max(0.0, 1.0 - variance)
        else:
            stability = 0.5
        if len(self.worldview_coherence_history) > 10:
            early_avg = sum((c for _, c in self.worldview_coherence_history[:5])) / 5
            trend = (current - early_avg) * 0.5
        else:
            trend = 0.0
        return min(1.0, current * 0.6 + stability * 0.3 + trend * 0.1)
    def calculate_adaptability_score(self) -> float:
        if not self.belief_updates:
            return 0.5
        if self.insights:
            update_ratio = len(self.belief_updates) / len(self.insights)
            frequency_score = min(1.0, update_ratio * 2.0)
        else:
            frequency_score = 0.0
        integration_quality = self.evidence_integration_score
        if len(self.belief_updates) > 5:
            recent_updates = self.belief_updates[-10:]
            avg_coherence_impact = sum((abs(u.coherence_impact) for u in recent_updates)) / len(recent_updates)
            balance_score = 1.0 - min(1.0, avg_coherence_impact * 2.0)
        else:
            balance_score = 0.5
        return frequency_score * 0.3 + integration_quality * 0.5 + balance_score * 0.2
    def calculate_composite_wisdom_score(self) -> Dict[str, float]:
        depth = self.calculate_depth_score()
        breadth = self.calculate_breadth_score()
        applicability = self.calculate_applicability_score()
        coherence = self.calculate_coherence_score()
        adaptability = self.calculate_adaptability_score()
        composite = depth * 0.25 + breadth * 0.2 + applicability * 0.2 + coherence * 0.25 + adaptability * 0.1
        return {'depth': depth, 'breadth': breadth, 'applicability': applicability, 'coherence': coherence, 'adaptability': adaptability, 'composite_wisdom': composite}
    def _record_wisdom_snapshot(self) -> None:
        scores = self.calculate_composite_wisdom_score()
        self.wisdom_history.append((time.time(), scores))
    def get_wisdom_growth_rate(self, window_minutes: int=60) -> float:
        if len(self.wisdom_history) < 2:
            return 0.0
        current_time = time.time()
        window_seconds = window_minutes * 60
        recent_snapshots = [(t, scores) for t, scores in self.wisdom_history if current_time - t <= window_seconds]
        if len(recent_snapshots) < 2:
            return 0.0
        earliest = recent_snapshots[0]
        latest = recent_snapshots[-1]
        time_diff_hours = (latest[0] - earliest[0]) / 3600.0
        wisdom_diff = latest[1]['composite_wisdom'] - earliest[1]['composite_wisdom']
        if time_diff_hours > 0:
            return wisdom_diff / time_diff_hours
        return 0.0
    def get_metrics_summary(self) -> Dict:
        scores = self.calculate_composite_wisdom_score()
        growth_rate = self.get_wisdom_growth_rate()
        return {'wisdom_scores': scores, 'growth_rate_per_hour': growth_rate, 'total_insights': len(self.insights), 'total_belief_updates': len(self.belief_updates), 'domains_explored': len(self.domain_knowledge), 'cross_domain_connections': len(self.domain_connections), 'current_coherence': self.current_coherence, 'evidence_integration_quality': self.evidence_integration_score}
    def save_to_file(self, filepath: str) -> None:
        data = {'insights': [{'id': i.id, 'content': i.content, 'timestamp': i.timestamp, 'depth_score': i.depth_score, 'breadth_score': i.breadth_score, 'applicability_score': i.applicability_score, 'coherence_contribution': i.coherence_contribution, 'related_domains': i.related_domains} for i in self.insights], 'belief_updates': [{'id': u.id, 'timestamp': u.timestamp, 'prior_belief': u.prior_belief, 'updated_belief': u.updated_belief, 'evidence': u.evidence, 'confidence_change': u.confidence_change, 'coherence_impact': u.coherence_impact} for u in self.belief_updates], 'domain_knowledge': dict(self.domain_knowledge), 'current_metrics': self.get_metrics_summary()}
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
    @classmethod
    def load_from_file(cls, filepath: str) -> 'WisdomMetrics':
        with open(filepath, 'r') as f:
            data = json.load(f)
        metrics = cls()
        for i_data in data.get('insights', []):
            insight = WisdomInsight(id=i_data['id'], content=i_data['content'], timestamp=i_data['timestamp'], depth_score=i_data['depth_score'], breadth_score=i_data['breadth_score'], applicability_score=i_data['applicability_score'], coherence_contribution=i_data['coherence_contribution'], related_domains=i_data['related_domains'])
            metrics.insights.append(insight)
        for u_data in data.get('belief_updates', []):
            update = BeliefUpdate(id=u_data['id'], timestamp=u_data['timestamp'], prior_belief=u_data['prior_belief'], updated_belief=u_data['updated_belief'], evidence=u_data['evidence'], confidence_change=u_data['confidence_change'], coherence_impact=u_data['coherence_impact'])
            metrics.belief_updates.append(update)
        metrics.domain_knowledge = defaultdict(float, data.get('domain_knowledge', {}))
        metrics._rebuild_derived_metrics()
        return metrics
    def _rebuild_derived_metrics(self) -> None:
        for insight in self.insights:
            for i, domain1 in enumerate(insight.related_domains):
                for domain2 in insight.related_domains[i + 1:]:
                    key = tuple(sorted([domain1, domain2]))
                    self.domain_connections[key] += 1
        self.current_coherence = 0.5
        for insight in self.insights:
            self.current_coherence += insight.coherence_contribution * 0.05
            self.current_coherence = max(0.0, min(1.0, self.current_coherence))
            self.worldview_coherence_history.append((insight.timestamp, self.current_coherence))
        for update in self.belief_updates:
            self.current_coherence += update.coherence_impact * 0.05
            self.current_coherence = max(0.0, min(1.0, self.current_coherence))
            self.worldview_coherence_history.append((update.timestamp, self.current_coherence))
        for insight in self.insights:
            self._record_wisdom_snapshot()