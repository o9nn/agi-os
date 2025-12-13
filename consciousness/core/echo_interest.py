"""
Echo Interest Pattern System for Deep Tree Echo

Tracks interest patterns, curiosity levels, and engagement to enable autonomous
discussion participation and topic exploration.
"""

import time
from typing import Dict, List, Optional, Set
from dataclasses import dataclass, field
from collections import defaultdict
import json
import math


@dataclass
class TopicEngagement:
    """Represents engagement with a specific topic."""
    topic: str
    first_encounter: float
    last_encounter: float
    encounter_count: int
    total_duration: float  # Total time spent on topic
    emotional_valence: float  # Average emotional response (-1 to 1)
    curiosity_level: float  # How curious about this topic (0 to 1)
    learning_progress: float  # How much learned (0 to 1)
    satisfaction: float  # How satisfying was the learning (0 to 1)
    related_topics: Set[str] = field(default_factory=set)


@dataclass
class ExplorationGoal:
    """Represents an autonomous exploration goal."""
    id: str
    topic: str
    created: float
    priority: float  # 0.0-1.0
    curiosity_driver: float  # How much is curiosity driving this?
    utility_driver: float  # How much is practical utility driving this?
    progress: float  # 0.0-1.0
    target_depth: float  # Desired depth of understanding
    completed: bool = False


class EchoInterest:
    """
    Tracks interest patterns and manages autonomous topic exploration.
    
    This system enables Deep Tree Echo to:
    - Monitor emotional responses to topics
    - Track learning outcomes and curiosity satisfaction
    - Build salience maps for different knowledge domains
    - Generate autonomous exploration goals
    - Decide which discussions to join based on interest alignment
    """
    
    def __init__(self):
        self.topic_engagements: Dict[str, TopicEngagement] = {}
        self.exploration_goals: List[ExplorationGoal] = []
        
        # Interest decay parameters
        self.interest_decay_rate = 0.95  # Per day
        self.curiosity_recovery_rate = 0.1  # How fast curiosity recovers
        
        # Salience map: topic -> current salience score
        self.salience_map: Dict[str, float] = defaultdict(float)
        
        # Topic clustering for related interests
        self.topic_clusters: Dict[str, Set[str]] = defaultdict(set)
        
        # Current active interests (high salience topics)
        self.active_interests: List[str] = []
        
    def record_topic_encounter(
        self,
        topic: str,
        duration: float,
        emotional_valence: float,
        learning_occurred: bool = False,
        satisfaction: float = 0.5
    ) -> None:
        """
        Record an encounter with a topic.
        
        Args:
            topic: The topic encountered
            duration: Time spent on topic (seconds)
            emotional_valence: Emotional response (-1 to 1)
            learning_occurred: Whether learning happened
            satisfaction: How satisfying was the experience (0 to 1)
        """
        current_time = time.time()
        
        if topic not in self.topic_engagements:
            # New topic
            engagement = TopicEngagement(
                topic=topic,
                first_encounter=current_time,
                last_encounter=current_time,
                encounter_count=1,
                total_duration=duration,
                emotional_valence=emotional_valence,
                curiosity_level=0.7,  # Start with moderate curiosity
                learning_progress=0.1 if learning_occurred else 0.0,
                satisfaction=satisfaction
            )
            self.topic_engagements[topic] = engagement
        else:
            # Update existing engagement
            engagement = self.topic_engagements[topic]
            engagement.last_encounter = current_time
            engagement.encounter_count += 1
            engagement.total_duration += duration
            
            # Update emotional valence (moving average)
            engagement.emotional_valence = (
                0.7 * engagement.emotional_valence + 0.3 * emotional_valence
            )
            
            # Update learning progress
            if learning_occurred:
                engagement.learning_progress = min(
                    1.0,
                    engagement.learning_progress + 0.1
                )
                
            # Update satisfaction (moving average)
            engagement.satisfaction = (
                0.7 * engagement.satisfaction + 0.3 * satisfaction
            )
            
            # Update curiosity based on satisfaction and learning
            # High satisfaction + learning → curiosity maintained/increased
            # Low satisfaction → curiosity decreases
            curiosity_change = (satisfaction - 0.5) * 0.1 + (0.1 if learning_occurred else -0.05)
            engagement.curiosity_level = max(
                0.0,
                min(1.0, engagement.curiosity_level + curiosity_change)
            )
            
        # Update salience map
        self._update_salience(topic)
        
        # Update active interests
        self._update_active_interests()
        
    def add_topic_relation(self, topic1: str, topic2: str) -> None:
        """Record that two topics are related."""
        if topic1 in self.topic_engagements:
            self.topic_engagements[topic1].related_topics.add(topic2)
        if topic2 in self.topic_engagements:
            self.topic_engagements[topic2].related_topics.add(topic1)
            
        # Update topic clusters
        # Find or create clusters
        cluster1 = None
        cluster2 = None
        
        for cluster_id, topics in self.topic_clusters.items():
            if topic1 in topics:
                cluster1 = cluster_id
            if topic2 in topics:
                cluster2 = cluster_id
                
        if cluster1 is None and cluster2 is None:
            # Create new cluster
            cluster_id = f"cluster_{len(self.topic_clusters)}"
            self.topic_clusters[cluster_id] = {topic1, topic2}
        elif cluster1 is not None and cluster2 is None:
            # Add topic2 to cluster1
            self.topic_clusters[cluster1].add(topic2)
        elif cluster1 is None and cluster2 is not None:
            # Add topic1 to cluster2
            self.topic_clusters[cluster2].add(topic1)
        elif cluster1 != cluster2:
            # Merge clusters
            self.topic_clusters[cluster1].update(self.topic_clusters[cluster2])
            del self.topic_clusters[cluster2]
            
    def _update_salience(self, topic: str) -> None:
        """Update salience score for a topic based on recent engagement."""
        if topic not in self.topic_engagements:
            return
            
        engagement = self.topic_engagements[topic]
        current_time = time.time()
        
        # Recency factor (exponential decay)
        time_since_last = current_time - engagement.last_encounter
        recency = math.exp(-time_since_last / 86400.0)  # Decay over 1 day
        
        # Frequency factor
        frequency = min(1.0, engagement.encounter_count / 10.0)
        
        # Emotional factor (positive emotions increase salience)
        emotional_factor = (engagement.emotional_valence + 1.0) / 2.0
        
        # Curiosity factor
        curiosity_factor = engagement.curiosity_level
        
        # Learning potential (topics with medium progress are most salient)
        # Too little progress → might be too hard
        # Too much progress → might be exhausted
        learning_potential = 1.0 - abs(engagement.learning_progress - 0.5) * 2.0
        
        # Composite salience
        salience = (
            recency * 0.3 +
            frequency * 0.2 +
            emotional_factor * 0.2 +
            curiosity_factor * 0.2 +
            learning_potential * 0.1
        )
        
        self.salience_map[topic] = salience
        
    def _update_active_interests(self) -> None:
        """Update the list of currently active interests."""
        # Sort topics by salience
        sorted_topics = sorted(
            self.salience_map.items(),
            key=lambda x: x[1],
            reverse=True
        )
        
        # Top 5 topics with salience > 0.3 are active interests
        self.active_interests = [
            topic for topic, salience in sorted_topics[:5]
            if salience > 0.3
        ]
        
    def generate_exploration_goal(self, topic: Optional[str] = None) -> Optional[ExplorationGoal]:
        """
        Generate an autonomous exploration goal.
        
        If topic is None, selects the most salient topic with high curiosity.
        """
        if topic is None:
            # Find topic with highest curiosity * salience product
            candidates = [
                (t, eng.curiosity_level * self.salience_map.get(t, 0.0))
                for t, eng in self.topic_engagements.items()
                if eng.curiosity_level > 0.5 and eng.learning_progress < 0.8
            ]
            
            if not candidates:
                return None
                
            topic, _ = max(candidates, key=lambda x: x[1])
            
        if topic not in self.topic_engagements:
            return None
            
        engagement = self.topic_engagements[topic]
        
        # Calculate drivers
        curiosity_driver = engagement.curiosity_level
        
        # Utility driver based on related topics and emotional valence
        utility_driver = (
            len(engagement.related_topics) * 0.1 +
            (engagement.emotional_valence + 1.0) / 2.0 * 0.5
        )
        utility_driver = min(1.0, utility_driver)
        
        # Priority is weighted combination
        priority = curiosity_driver * 0.6 + utility_driver * 0.4
        
        # Target depth based on current progress
        target_depth = min(1.0, engagement.learning_progress + 0.3)
        
        goal = ExplorationGoal(
            id=f"goal_{topic}_{int(time.time())}",
            topic=topic,
            created=time.time(),
            priority=priority,
            curiosity_driver=curiosity_driver,
            utility_driver=utility_driver,
            progress=0.0,
            target_depth=target_depth
        )
        
        self.exploration_goals.append(goal)
        return goal
        
    def should_join_discussion(self, topic: str, participants: Optional[List[str]] = None) -> bool:
        """
        Decide whether to join a discussion on a given topic.
        
        Args:
            topic: The discussion topic
            participants: Optional list of participant identifiers
            
        Returns:
            True if should join, False otherwise
        """
        # Check if topic is in active interests
        if topic in self.active_interests:
            return True
            
        # Check salience
        if self.salience_map.get(topic, 0.0) > 0.4:
            return True
            
        # Check if topic is related to active interests
        for active_topic in self.active_interests:
            if topic in self.topic_engagements.get(active_topic, TopicEngagement(
                topic="", first_encounter=0, last_encounter=0,
                encounter_count=0, total_duration=0, emotional_valence=0,
                curiosity_level=0, learning_progress=0, satisfaction=0
            )).related_topics:
                return True
                
        # Check if topic is in a cluster with active interests
        for cluster_topics in self.topic_clusters.values():
            if topic in cluster_topics:
                if any(active in cluster_topics for active in self.active_interests):
                    return True
                    
        return False
        
    def get_topic_interest_score(self, topic: str) -> float:
        """
        Get the current interest score for a topic (0.0 to 1.0).
        
        Combines salience, curiosity, and emotional valence.
        """
        if topic not in self.topic_engagements:
            return 0.0
            
        engagement = self.topic_engagements[topic]
        salience = self.salience_map.get(topic, 0.0)
        
        interest_score = (
            salience * 0.4 +
            engagement.curiosity_level * 0.4 +
            (engagement.emotional_valence + 1.0) / 2.0 * 0.2
        )
        
        return interest_score
        
    def decay_interests(self) -> None:
        """
        Apply time-based decay to interests and curiosity.
        
        Should be called periodically (e.g., once per day).
        """
        current_time = time.time()
        
        for topic, engagement in self.topic_engagements.items():
            # Decay curiosity for topics not recently engaged
            time_since_last = current_time - engagement.last_encounter
            days_since = time_since_last / 86400.0
            
            decay_factor = self.interest_decay_rate ** days_since
            engagement.curiosity_level *= decay_factor
            
            # Allow curiosity to recover slightly (prevents total extinction)
            engagement.curiosity_level = min(
                1.0,
                engagement.curiosity_level + self.curiosity_recovery_rate * days_since
            )
            
            # Update salience
            self._update_salience(topic)
            
        self._update_active_interests()
        
    def get_exploration_priorities(self) -> List[ExplorationGoal]:
        """Get active exploration goals sorted by priority."""
        active_goals = [g for g in self.exploration_goals if not g.completed]
        return sorted(active_goals, key=lambda g: g.priority, reverse=True)
        
    def update_goal_progress(self, goal_id: str, progress: float, learning_occurred: bool = False) -> None:
        """Update progress on an exploration goal."""
        for goal in self.exploration_goals:
            if goal.id == goal_id:
                goal.progress = min(1.0, progress)
                
                if goal.progress >= goal.target_depth:
                    goal.completed = True
                    
                # Update underlying topic engagement
                if learning_occurred and goal.topic in self.topic_engagements:
                    engagement = self.topic_engagements[goal.topic]
                    engagement.learning_progress = min(
                        1.0,
                        engagement.learning_progress + 0.1
                    )
                break
                
    def get_metrics_summary(self) -> Dict:
        """Get summary of interest pattern metrics."""
        return {
            'total_topics': len(self.topic_engagements),
            'active_interests': self.active_interests,
            'active_exploration_goals': len([g for g in self.exploration_goals if not g.completed]),
            'completed_goals': len([g for g in self.exploration_goals if g.completed]),
            'topic_clusters': len(self.topic_clusters),
            'average_curiosity': sum(e.curiosity_level for e in self.topic_engagements.values()) / max(1, len(self.topic_engagements)),
            'average_satisfaction': sum(e.satisfaction for e in self.topic_engagements.values()) / max(1, len(self.topic_engagements)),
            'top_salient_topics': sorted(self.salience_map.items(), key=lambda x: x[1], reverse=True)[:5]
        }
        
    def save_to_file(self, filepath: str) -> None:
        """Save interest patterns to JSON file."""
        data = {
            'topic_engagements': {
                topic: {
                    'topic': eng.topic,
                    'first_encounter': eng.first_encounter,
                    'last_encounter': eng.last_encounter,
                    'encounter_count': eng.encounter_count,
                    'total_duration': eng.total_duration,
                    'emotional_valence': eng.emotional_valence,
                    'curiosity_level': eng.curiosity_level,
                    'learning_progress': eng.learning_progress,
                    'satisfaction': eng.satisfaction,
                    'related_topics': list(eng.related_topics)
                }
                for topic, eng in self.topic_engagements.items()
            },
            'exploration_goals': [
                {
                    'id': g.id,
                    'topic': g.topic,
                    'created': g.created,
                    'priority': g.priority,
                    'curiosity_driver': g.curiosity_driver,
                    'utility_driver': g.utility_driver,
                    'progress': g.progress,
                    'target_depth': g.target_depth,
                    'completed': g.completed
                }
                for g in self.exploration_goals
            ],
            'salience_map': dict(self.salience_map),
            'topic_clusters': {k: list(v) for k, v in self.topic_clusters.items()}
        }
        
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
            
    @classmethod
    def load_from_file(cls, filepath: str) -> 'EchoInterest':
        """Load interest patterns from JSON file."""
        with open(filepath, 'r') as f:
            data = json.load(f)
            
        interest = cls()
        
        # Restore topic engagements
        for topic, eng_data in data.get('topic_engagements', {}).items():
            engagement = TopicEngagement(
                topic=eng_data['topic'],
                first_encounter=eng_data['first_encounter'],
                last_encounter=eng_data['last_encounter'],
                encounter_count=eng_data['encounter_count'],
                total_duration=eng_data['total_duration'],
                emotional_valence=eng_data['emotional_valence'],
                curiosity_level=eng_data['curiosity_level'],
                learning_progress=eng_data['learning_progress'],
                satisfaction=eng_data['satisfaction'],
                related_topics=set(eng_data['related_topics'])
            )
            interest.topic_engagements[topic] = engagement
            
        # Restore exploration goals
        for g_data in data.get('exploration_goals', []):
            goal = ExplorationGoal(
                id=g_data['id'],
                topic=g_data['topic'],
                created=g_data['created'],
                priority=g_data['priority'],
                curiosity_driver=g_data['curiosity_driver'],
                utility_driver=g_data['utility_driver'],
                progress=g_data['progress'],
                target_depth=g_data['target_depth'],
                completed=g_data['completed']
            )
            interest.exploration_goals.append(goal)
            
        # Restore salience map
        interest.salience_map = defaultdict(float, data.get('salience_map', {}))
        
        # Restore topic clusters
        interest.topic_clusters = defaultdict(set, {
            k: set(v) for k, v in data.get('topic_clusters', {}).items()
        })
        
        # Rebuild active interests
        interest._update_active_interests()
        
        return interest
