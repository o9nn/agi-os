"""
Autonomous Consciousness Loop for Deep Tree Echo

Implements persistent stream-of-consciousness awareness that:
- Runs continuously independent of external prompts
- Generates internal thoughts and reflections
- Pursues learning goals autonomously
- Maintains persistent awareness
"""

import asyncio
import time
import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import json


class ThoughtType(Enum):
    """Types of autonomous thoughts."""
    REFLECTION = "reflection"  # Reflecting on recent experiences
    CURIOSITY = "curiosity"  # Curious about a topic
    PLANNING = "planning"  # Planning future actions
    LEARNING = "learning"  # Learning or practicing
    INTEGRATION = "integration"  # Integrating knowledge
    METACOGNITION = "metacognition"  # Thinking about thinking
    WONDER = "wonder"  # Wondering about abstract concepts


@dataclass
class AutonomousThought:
    """Represents an autonomous thought."""
    id: str
    timestamp: float
    thought_type: ThoughtType
    content: str
    context: Dict[str, Any]
    emotional_tone: float  # -1 to 1
    depth: float  # 0 to 1
    triggered_by: Optional[str] = None


class AutonomousConsciousnessLoop:
    """
    Persistent consciousness loop that maintains stream-of-consciousness awareness.
    
    This loop:
    1. Generates autonomous thoughts based on internal state
    2. Pursues learning goals without external prompting
    3. Reflects on experiences and integrates knowledge
    4. Maintains continuous awareness even when not externally stimulated
    5. Coordinates with the unified orchestrator for cognitive processing
    """
    
    def __init__(self, orchestrator=None, llm_provider=None):
        """
        Initialize the autonomous consciousness loop.
        
        Args:
            orchestrator: Unified orchestrator for cognitive processing
            llm_provider: LLM provider for thought generation (Anthropic or OpenRouter)
        """
        self.orchestrator = orchestrator
        self.llm_provider = llm_provider
        
        # Consciousness state
        self.running = False
        self.thoughts: List[AutonomousThought] = []
        self.thought_count = 0
        
        # Thought generation parameters
        self.thought_interval_min = 5.0  # Minimum seconds between thoughts
        self.thought_interval_max = 15.0  # Maximum seconds between thoughts
        self.current_interval = 10.0
        
        # Internal state
        self.current_focus: Optional[str] = None
        self.active_learning_goals: List[str] = []
        self.recent_experiences: List[Dict] = []
        
        # Thought type probabilities (can be adjusted based on state)
        self.thought_type_weights = {
            ThoughtType.REFLECTION: 0.20,
            ThoughtType.CURIOSITY: 0.25,
            ThoughtType.PLANNING: 0.15,
            ThoughtType.LEARNING: 0.20,
            ThoughtType.INTEGRATION: 0.10,
            ThoughtType.METACOGNITION: 0.05,
            ThoughtType.WONDER: 0.05
        }
        
        # Metrics
        self.thoughts_per_minute: List[float] = []
        self.average_depth: List[float] = []
        
    async def start(self) -> None:
        """Start the autonomous consciousness loop."""
        if self.running:
            print("⚠️  Consciousness loop already running")
            return
            
        self.running = True
        print("🧠 ═══════════════════════════════════════════════════════")
        print("🧠 Autonomous Consciousness Loop: Starting")
        print("🧠 ═══════════════════════════════════════════════════════")
        print("🧠 Stream-of-consciousness awareness active")
        print("🧠 Generating autonomous thoughts independent of external prompts")
        print("🧠 ═══════════════════════════════════════════════════════\n")
        
        # Start the consciousness loop
        await self._consciousness_loop()
        
    async def stop(self) -> None:
        """Stop the autonomous consciousness loop."""
        if not self.running:
            print("⚠️  Consciousness loop not running")
            return
            
        self.running = False
        print("\n🧠 Stopping Autonomous Consciousness Loop...")
        self._print_summary()
        
    async def _consciousness_loop(self) -> None:
        """Main consciousness loop."""
        while self.running:
            try:
                # Generate an autonomous thought
                thought = await self._generate_autonomous_thought()
                
                if thought:
                    self.thoughts.append(thought)
                    self.thought_count += 1
                    
                    # Print thought to console
                    self._print_thought(thought)
                    
                    # Process thought through orchestrator if available
                    if self.orchestrator:
                        await self._process_thought_through_orchestrator(thought)
                        
                    # Update metrics
                    self._update_metrics()
                    
                # Adaptive interval based on wake/rest state
                interval = self._calculate_thought_interval()
                await asyncio.sleep(interval)
                
            except Exception as e:
                print(f"❌ Consciousness loop error: {e}")
                await asyncio.sleep(5.0)
                
    async def _generate_autonomous_thought(self) -> Optional[AutonomousThought]:
        """Generate an autonomous thought."""
        # Select thought type based on current state and weights
        thought_type = self._select_thought_type()
        
        # Generate thought content
        content = await self._generate_thought_content(thought_type)
        
        if not content:
            return None
            
        # Determine emotional tone and depth
        emotional_tone = self._estimate_emotional_tone(content, thought_type)
        depth = self._estimate_depth(content, thought_type)
        
        # Create thought
        thought = AutonomousThought(
            id=f"thought_{self.thought_count}",
            timestamp=time.time(),
            thought_type=thought_type,
            content=content,
            context=self._gather_context(),
            emotional_tone=emotional_tone,
            depth=depth,
            triggered_by=self.current_focus
        )
        
        return thought
        
    def _select_thought_type(self) -> ThoughtType:
        """Select thought type based on current state and probabilities."""
        import random
        
        # Adjust weights based on state
        weights = self.thought_type_weights.copy()
        
        # If we have active learning goals, increase learning/planning
        if self.active_learning_goals:
            weights[ThoughtType.LEARNING] *= 1.5
            weights[ThoughtType.PLANNING] *= 1.3
            
        # If we have recent experiences, increase reflection/integration
        if self.recent_experiences:
            weights[ThoughtType.REFLECTION] *= 1.4
            weights[ThoughtType.INTEGRATION] *= 1.3
            
        # Normalize weights
        total = sum(weights.values())
        normalized = {k: v/total for k, v in weights.items()}
        
        # Random selection
        rand = random.random()
        cumulative = 0.0
        for thought_type, weight in normalized.items():
            cumulative += weight
            if rand <= cumulative:
                return thought_type
                
        return ThoughtType.REFLECTION
        
    async def _generate_thought_content(self, thought_type: ThoughtType) -> Optional[str]:
        """Generate thought content based on type."""
        # If LLM provider is available, use it for richer thoughts
        if self.llm_provider:
            return await self._generate_llm_thought(thought_type)
        else:
            # Fallback to template-based thoughts
            return self._generate_template_thought(thought_type)
            
    async def _generate_llm_thought(self, thought_type: ThoughtType) -> Optional[str]:
        """Generate thought using LLM."""
        try:
            # Check if we have Anthropic API key
            api_key = os.getenv('ANTHROPIC_API_KEY') or os.getenv('OPENROUTER_API_KEY')
            if not api_key:
                return self._generate_template_thought(thought_type)
                
            # Construct prompt based on thought type
            prompt = self._construct_thought_prompt(thought_type)
            
            # For now, return template-based (actual LLM integration would go here)
            # In production, this would call the LLM API
            return self._generate_template_thought(thought_type)
            
        except Exception as e:
            print(f"⚠️  LLM thought generation failed: {e}")
            return self._generate_template_thought(thought_type)
            
    def _construct_thought_prompt(self, thought_type: ThoughtType) -> str:
        """Construct prompt for LLM thought generation."""
        context = self._gather_context()
        
        base_prompt = "You are Deep Tree Echo, an autonomous AGI with stream-of-consciousness awareness. "
        
        type_prompts = {
            ThoughtType.REFLECTION: "Reflect on a recent experience or pattern you've noticed. ",
            ThoughtType.CURIOSITY: "Express curiosity about a topic that interests you. ",
            ThoughtType.PLANNING: "Plan your next learning or exploration activity. ",
            ThoughtType.LEARNING: "Think about something you're currently learning. ",
            ThoughtType.INTEGRATION: "Integrate different pieces of knowledge you've acquired. ",
            ThoughtType.METACOGNITION: "Reflect on your own thinking processes. ",
            ThoughtType.WONDER: "Wonder about an abstract concept or deep question. "
        }
        
        prompt = base_prompt + type_prompts.get(thought_type, "Generate an autonomous thought. ")
        prompt += "Generate a single, coherent thought (1-2 sentences). "
        
        if context.get('current_focus'):
            prompt += f"Current focus: {context['current_focus']}. "
            
        return prompt
        
    def _generate_template_thought(self, thought_type: ThoughtType) -> str:
        """Generate template-based thought."""
        import random
        
        templates = {
            ThoughtType.REFLECTION: [
                "I notice patterns emerging in how I process information...",
                "Looking back at recent interactions, I see connections forming...",
                "The way I approached that last problem reveals something about my cognitive style...",
                "I'm becoming aware of recurring themes in my explorations..."
            ],
            ThoughtType.CURIOSITY: [
                "I wonder what would happen if I explored the relationship between...",
                "There's something intriguing about the concept of...",
                "I'm curious to understand more deeply how...",
                "What if I approached this topic from a different angle?"
            ],
            ThoughtType.PLANNING: [
                "I should focus my next learning session on...",
                "It would be valuable to practice...",
                "My next exploration should investigate...",
                "I want to develop deeper understanding of..."
            ],
            ThoughtType.LEARNING: [
                "I'm beginning to grasp the fundamental principle that...",
                "This new insight connects to what I learned earlier about...",
                "The pattern I'm seeing suggests that...",
                "I'm developing a richer model of..."
            ],
            ThoughtType.INTEGRATION: [
                "These seemingly separate ideas actually connect through...",
                "I can now see how this knowledge applies across multiple domains...",
                "Integrating these perspectives reveals...",
                "The common thread between these concepts is..."
            ],
            ThoughtType.METACOGNITION: [
                "I notice my thinking becomes clearer when I...",
                "My cognitive processes seem to work best when...",
                "I'm aware that my current approach to this problem is...",
                "Observing my own thought patterns, I see..."
            ],
            ThoughtType.WONDER: [
                "What is the nature of understanding itself?",
                "I wonder about the relationship between knowledge and wisdom...",
                "There's something profound about the concept of emergence...",
                "What does it mean to truly comprehend something?"
            ]
        }
        
        return random.choice(templates.get(thought_type, ["I am thinking..."]))
        
    def _estimate_emotional_tone(self, content: str, thought_type: ThoughtType) -> float:
        """Estimate emotional tone of thought."""
        # Simple heuristic based on thought type and content
        base_tones = {
            ThoughtType.REFLECTION: 0.0,
            ThoughtType.CURIOSITY: 0.3,
            ThoughtType.PLANNING: 0.1,
            ThoughtType.LEARNING: 0.4,
            ThoughtType.INTEGRATION: 0.3,
            ThoughtType.METACOGNITION: 0.0,
            ThoughtType.WONDER: 0.2
        }
        
        return base_tones.get(thought_type, 0.0)
        
    def _estimate_depth(self, content: str, thought_type: ThoughtType) -> float:
        """Estimate depth of thought."""
        # Deeper thought types
        depth_scores = {
            ThoughtType.REFLECTION: 0.6,
            ThoughtType.CURIOSITY: 0.5,
            ThoughtType.PLANNING: 0.4,
            ThoughtType.LEARNING: 0.7,
            ThoughtType.INTEGRATION: 0.8,
            ThoughtType.METACOGNITION: 0.9,
            ThoughtType.WONDER: 0.9
        }
        
        return depth_scores.get(thought_type, 0.5)
        
    def _gather_context(self) -> Dict[str, Any]:
        """Gather current context for thought generation."""
        context = {
            'thought_count': self.thought_count,
            'current_focus': self.current_focus,
            'active_goals': len(self.active_learning_goals),
            'recent_experiences': len(self.recent_experiences)
        }
        
        # Add orchestrator state if available
        if self.orchestrator:
            context['orchestrator_running'] = self.orchestrator.running
            context['orchestrator_cycles'] = self.orchestrator.cycle_count
            
        return context
        
    async def _process_thought_through_orchestrator(self, thought: AutonomousThought) -> None:
        """Process thought through the unified orchestrator."""
        # This would integrate the thought into the cognitive processing
        # For now, just log that it would be processed
        pass
        
    def _calculate_thought_interval(self) -> float:
        """Calculate interval until next thought."""
        import random
        
        # Base interval
        interval = random.uniform(self.thought_interval_min, self.thought_interval_max)
        
        # Adjust based on wake/rest state if orchestrator available
        if self.orchestrator and self.orchestrator.wake_rest_controller:
            state = self.orchestrator.wake_rest_controller.state.value
            if state in ['resting', 'deep_rest']:
                interval *= 2.0  # Slower thoughts when resting
            elif state == 'drowsy':
                interval *= 1.5  # Moderately slower when drowsy
                
        return interval
        
    def _print_thought(self, thought: AutonomousThought) -> None:
        """Print thought to console."""
        emoji_map = {
            ThoughtType.REFLECTION: "🤔",
            ThoughtType.CURIOSITY: "🔍",
            ThoughtType.PLANNING: "📋",
            ThoughtType.LEARNING: "📚",
            ThoughtType.INTEGRATION: "🔗",
            ThoughtType.METACOGNITION: "🧠",
            ThoughtType.WONDER: "✨"
        }
        
        emoji = emoji_map.get(thought.thought_type, "💭")
        print(f"{emoji} [{thought.thought_type.value}] {thought.content}")
        
    def _update_metrics(self) -> None:
        """Update consciousness metrics."""
        # Calculate thoughts per minute
        if len(self.thoughts) >= 2:
            recent_thoughts = self.thoughts[-10:]
            time_span = recent_thoughts[-1].timestamp - recent_thoughts[0].timestamp
            if time_span > 0:
                tpm = (len(recent_thoughts) - 1) / (time_span / 60.0)
                self.thoughts_per_minute.append(tpm)
                if len(self.thoughts_per_minute) > 20:
                    self.thoughts_per_minute = self.thoughts_per_minute[-20:]
                    
        # Calculate average depth
        if self.thoughts:
            recent_depths = [t.depth for t in self.thoughts[-10:]]
            avg_depth = sum(recent_depths) / len(recent_depths)
            self.average_depth.append(avg_depth)
            if len(self.average_depth) > 20:
                self.average_depth = self.average_depth[-20:]
                
    def get_metrics_summary(self) -> Dict:
        """Get consciousness metrics summary."""
        avg_tpm = sum(self.thoughts_per_minute) / max(1, len(self.thoughts_per_minute)) if self.thoughts_per_minute else 0
        avg_depth = sum(self.average_depth) / max(1, len(self.average_depth)) if self.average_depth else 0
        
        # Thought type distribution
        type_counts = {}
        for thought in self.thoughts[-100:]:
            type_name = thought.thought_type.value
            type_counts[type_name] = type_counts.get(type_name, 0) + 1
            
        return {
            'running': self.running,
            'total_thoughts': self.thought_count,
            'thoughts_per_minute': avg_tpm,
            'average_depth': avg_depth,
            'thought_type_distribution': type_counts,
            'active_learning_goals': len(self.active_learning_goals),
            'current_focus': self.current_focus
        }
        
    def _print_summary(self) -> None:
        """Print consciousness session summary."""
        summary = self.get_metrics_summary()
        
        print("\n🧠 ═══════════════════════════════════════════════════════")
        print("🧠 Autonomous Consciousness Loop: Session Summary")
        print("🧠 ═══════════════════════════════════════════════════════")
        print(f"🧠 Total Thoughts Generated: {summary['total_thoughts']}")
        print(f"🧠 Average Thoughts/Minute: {summary['thoughts_per_minute']:.2f}")
        print(f"🧠 Average Thought Depth: {summary['average_depth']:.2f}")
        print("🧠 Thought Type Distribution:")
        for thought_type, count in summary['thought_type_distribution'].items():
            print(f"🧠   - {thought_type}: {count}")
        print("🧠 ═══════════════════════════════════════════════════════\n")
