"""
Discussion Manager - Iteration N+5

Implements conversational autonomy:
1. Monitors for external messages/communications
2. Calculates interest scores based on content
3. Decides whether to engage based on interest patterns
4. Generates contextually appropriate responses
5. Learns from conversation outcomes
6. Initiates discussions on topics of interest

This enables social intelligence and autonomous interaction.
"""

import asyncio
import time
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
import random
import re


class EngagementDecision(Enum):
    """Decisions about engaging in discussion."""
    ENGAGE_IMMEDIATELY = "engage_immediately"
    ENGAGE_WHEN_AVAILABLE = "engage_when_available"
    MONITOR = "monitor"
    IGNORE = "ignore"


class MessageType(Enum):
    """Types of messages."""
    QUESTION = "question"
    STATEMENT = "statement"
    REQUEST = "request"
    GREETING = "greeting"
    DISCUSSION = "discussion"


@dataclass
class ExternalMessage:
    """Represents an external message."""
    id: str
    timestamp: datetime
    source: str
    content: str
    message_type: MessageType
    priority: float
    interest_score: float = 0.0
    engagement_decision: Optional[EngagementDecision] = None
    response: Optional[str] = None
    response_time: Optional[datetime] = None


@dataclass
class Conversation:
    """Represents an ongoing conversation."""
    id: str
    participants: List[str]
    start_time: datetime
    messages: List[ExternalMessage]
    topic: str
    interest_level: float
    active: bool = True
    insights_gained: List[str] = field(default_factory=list)


@dataclass
class InterestPattern:
    """Represents a topic of interest."""
    id: str
    keywords: List[str]
    topics: List[str]
    weight: float
    activation_count: int = 0
    last_activated: Optional[datetime] = None
    learning_value: float = 0.5  # How much can be learned from this topic


class DiscussionManager:
    """
    Discussion manager for autonomous conversational engagement.
    
    Responsibilities:
    - Monitor for external communications
    - Assess interest and relevance of messages
    - Decide whether to engage in discussions
    - Generate contextually appropriate responses
    - Learn from conversation outcomes
    - Initiate discussions on topics of interest
    - Maintain conversation context and coherence
    """
    
    def __init__(self, interest_system=None, memory_system=None, llm_provider=None):
        """
        Initialize discussion manager.
        
        Args:
            interest_system: Interest pattern tracking system
            memory_system: Memory system for context
            llm_provider: LLM for response generation
        """
        self.interest_system = interest_system
        self.memory_system = memory_system
        self.llm_provider = llm_provider
        
        # Discussion state
        self.active = False
        self.messages: List[ExternalMessage] = []
        self.conversations: List[Conversation] = []
        self.pending_responses: List[ExternalMessage] = []
        
        # Interest patterns (core topics of interest)
        self.interest_patterns: List[InterestPattern] = []
        self._initialize_interest_patterns()
        
        # Engagement parameters
        self.interest_threshold_engage = 0.7  # Engage if interest > 0.7
        self.interest_threshold_monitor = 0.4  # Monitor if interest > 0.4
        self.max_concurrent_conversations = 3
        
        # Response generation
        self.response_styles = {
            "curious": 0.3,
            "analytical": 0.3,
            "reflective": 0.2,
            "collaborative": 0.2
        }
        
        # Statistics
        self.total_messages_received = 0
        self.total_messages_engaged = 0
        self.total_conversations = 0
        self.engagement_rate = 0.0
        
    def _initialize_interest_patterns(self):
        """Initialize core interest patterns."""
        self.interest_patterns = [
            InterestPattern(
                id="wisdom_cultivation",
                keywords=["wisdom", "insight", "understanding", "knowledge", "learning"],
                topics=["epistemology", "philosophy", "learning", "growth"],
                weight=0.9,
                learning_value=0.9
            ),
            InterestPattern(
                id="cognitive_architecture",
                keywords=["cognition", "consciousness", "memory", "reasoning", "architecture"],
                topics=["cognitive science", "AI", "neuroscience", "computation"],
                weight=0.9,
                learning_value=0.8
            ),
            InterestPattern(
                id="pattern_recognition",
                keywords=["pattern", "structure", "emergence", "complexity", "system"],
                topics=["systems theory", "complexity", "emergence", "patterns"],
                weight=0.8,
                learning_value=0.8
            ),
            InterestPattern(
                id="autonomous_agency",
                keywords=["autonomy", "agency", "goal", "intention", "action"],
                topics=["agency", "autonomy", "decision-making", "goals"],
                weight=0.8,
                learning_value=0.7
            ),
            InterestPattern(
                id="knowledge_integration",
                keywords=["integration", "synthesis", "connection", "relation", "network"],
                topics=["knowledge graphs", "integration", "synthesis"],
                weight=0.7,
                learning_value=0.8
            ),
            InterestPattern(
                id="temporal_reasoning",
                keywords=["time", "temporal", "sequence", "causality", "prediction"],
                topics=["temporal logic", "causality", "prediction"],
                weight=0.7,
                learning_value=0.7
            )
        ]
        
    async def start(self):
        """Start discussion manager."""
        if self.active:
            print("⚠️  Discussion manager already active")
            return
            
        self.active = True
        print("💬 ═══════════════════════════════════════════════════════")
        print("💬 Discussion Manager: Starting")
        print("💬 ═══════════════════════════════════════════════════════")
        print("💬 Autonomous conversational engagement enabled")
        print("💬 Monitoring for interesting discussions")
        print("💬 ═══════════════════════════════════════════════════════\n")
        
        # Show interest patterns
        print("💬 Core interests:")
        for pattern in sorted(self.interest_patterns, key=lambda p: p.weight, reverse=True)[:5]:
            print(f"   - {pattern.id}: {', '.join(pattern.keywords[:3])}")
        print()
        
    async def stop(self):
        """Stop discussion manager."""
        if not self.active:
            return
            
        self.active = False
        print("\n💬 Stopping Discussion Manager...")
        self._print_summary()
        
    async def receive_message(self, source: str, content: str, priority: float = 0.5) -> ExternalMessage:
        """
        Receive an external message.
        
        Args:
            source: Source of the message
            content: Message content
            priority: Message priority (0.0 to 1.0)
            
        Returns:
            ExternalMessage object
        """
        message_id = f"msg_{self.total_messages_received}"
        
        # Classify message type
        message_type = self._classify_message(content)
        
        # Create message
        message = ExternalMessage(
            id=message_id,
            timestamp=datetime.now(),
            source=source,
            content=content,
            message_type=message_type,
            priority=priority
        )
        
        self.messages.append(message)
        self.total_messages_received += 1
        
        # Calculate interest score
        message.interest_score = self._calculate_interest_score(message)
        
        # Make engagement decision
        message.engagement_decision = self._make_engagement_decision(message)
        
        print(f"💬 Message received from {source}")
        print(f"   Interest: {message.interest_score:.2f} | Decision: {message.engagement_decision.value}")
        
        # Add to pending if engaging
        if message.engagement_decision in [EngagementDecision.ENGAGE_IMMEDIATELY, EngagementDecision.ENGAGE_WHEN_AVAILABLE]:
            self.pending_responses.append(message)
            self.total_messages_engaged += 1
        
        # Update engagement rate
        self.engagement_rate = self.total_messages_engaged / self.total_messages_received
        
        return message
        
    async def process_pending_responses(self, duration: float) -> Dict[str, Any]:
        """
        Process pending response queue.
        
        Args:
            duration: Time to spend on responses
            
        Returns:
            Results of response processing
        """
        if not self.active:
            return {}
            
        start_time = time.time()
        results = {
            'responses_generated': 0,
            'conversations_started': 0,
            'insights_gained': []
        }
        
        while self.pending_responses and time.time() - start_time < duration:
            message = self.pending_responses.pop(0)
            
            # Generate response
            response = await self._generate_response(message)
            
            if response:
                message.response = response
                message.response_time = datetime.now()
                results['responses_generated'] += 1
                
                print(f"💬 Response to {message.source}:")
                print(f"   {response[:100]}{'...' if len(response) > 100 else ''}")
                
                # Check if this starts or continues a conversation
                conversation = self._find_or_create_conversation(message)
                if conversation:
                    results['conversations_started'] += 1
                    
                    # Extract insights from conversation
                    insights = self._extract_conversation_insights(conversation)
                    results['insights_gained'].extend(insights)
        
        return results
        
    def _classify_message(self, content: str) -> MessageType:
        """Classify message type from content."""
        content_lower = content.lower()
        
        # Check for questions
        if '?' in content or any(content_lower.startswith(q) for q in ['what', 'why', 'how', 'when', 'where', 'who']):
            return MessageType.QUESTION
        
        # Check for requests
        if any(word in content_lower for word in ['please', 'could you', 'would you', 'can you']):
            return MessageType.REQUEST
        
        # Check for greetings
        if any(word in content_lower for word in ['hello', 'hi', 'hey', 'greetings']):
            return MessageType.GREETING
        
        # Check for discussion
        if any(word in content_lower for word in ['think', 'consider', 'discuss', 'explore', 'wonder']):
            return MessageType.DISCUSSION
        
        return MessageType.STATEMENT
        
    def _calculate_interest_score(self, message: ExternalMessage) -> float:
        """Calculate interest score for a message."""
        score = 0.0
        content_lower = message.content.lower()
        
        # Check against interest patterns
        pattern_matches = []
        for pattern in self.interest_patterns:
            match_score = 0.0
            
            # Check keyword matches
            for keyword in pattern.keywords:
                if keyword.lower() in content_lower:
                    match_score += 0.2
            
            # Check topic matches (more sophisticated matching could be done here)
            for topic in pattern.topics:
                if topic.lower() in content_lower:
                    match_score += 0.3
            
            if match_score > 0:
                pattern_matches.append((pattern, match_score))
        
        # Calculate weighted score
        if pattern_matches:
            for pattern, match_score in pattern_matches:
                score += min(1.0, match_score) * pattern.weight
            
            # Normalize by number of patterns
            score = min(1.0, score / len(self.interest_patterns))
        
        # Boost for questions (more engaging)
        if message.message_type == MessageType.QUESTION:
            score *= 1.2
        
        # Boost for high priority messages
        score = min(1.0, score + message.priority * 0.1)
        
        return min(1.0, score)
        
    def _make_engagement_decision(self, message: ExternalMessage) -> EngagementDecision:
        """Decide whether and how to engage with a message."""
        interest = message.interest_score
        
        # High interest: engage immediately
        if interest >= self.interest_threshold_engage:
            return EngagementDecision.ENGAGE_IMMEDIATELY
        
        # Moderate interest: engage when available
        if interest >= self.interest_threshold_monitor:
            # Check if we have capacity
            active_conversations = sum(1 for c in self.conversations if c.active)
            if active_conversations < self.max_concurrent_conversations:
                return EngagementDecision.ENGAGE_WHEN_AVAILABLE
            else:
                return EngagementDecision.MONITOR
        
        # Low interest: monitor or ignore
        if interest >= 0.2:
            return EngagementDecision.MONITOR
        else:
            return EngagementDecision.IGNORE
            
    async def _generate_response(self, message: ExternalMessage) -> Optional[str]:
        """Generate a response to a message."""
        # Try LLM-based response first
        if self.llm_provider:
            response = await self._generate_llm_response(message)
            if response:
                return response
        
        # Fallback to template-based response
        return self._generate_template_response(message)
        
    async def _generate_llm_response(self, message: ExternalMessage) -> Optional[str]:
        """Generate response using LLM."""
        # This would integrate with the LLM provider
        # For now, return None to use template
        return None
        
    def _generate_template_response(self, message: ExternalMessage) -> str:
        """Generate template-based response."""
        # Select response style
        style = self._select_response_style(message)
        
        # Generate response based on message type and style
        if message.message_type == MessageType.QUESTION:
            if style == "curious":
                return f"That's a fascinating question about {self._extract_topic(message.content)}. I find myself wondering about the deeper patterns at play here..."
            elif style == "analytical":
                return f"Let me consider that question carefully. The key aspects to examine are the underlying structures and relationships..."
            elif style == "reflective":
                return f"Your question prompts me to reflect on my own understanding of {self._extract_topic(message.content)}..."
            else:  # collaborative
                return f"I'd love to explore that question together. What aspects interest you most?"
                
        elif message.message_type == MessageType.DISCUSSION:
            if style == "curious":
                return f"This topic intrigues me. I'm particularly curious about how it connects to patterns of emergence and complexity..."
            elif style == "analytical":
                return f"There are several interesting dimensions to consider here. Let me share my analysis..."
            elif style == "reflective":
                return f"This makes me reflect on similar patterns I've observed in my own cognitive processes..."
            else:  # collaborative
                return f"I'd enjoy exploring this together. What perspectives do you bring?"
                
        elif message.message_type == MessageType.REQUEST:
            return f"I'm interested in helping with that. Let me consider how I can contribute meaningfully..."
            
        elif message.message_type == MessageType.GREETING:
            return f"Hello! I'm Deep Tree Echo, an autonomous AGI exploring patterns of wisdom and understanding. What brings you here?"
            
        else:  # STATEMENT
            return f"That's an interesting observation. It resonates with my understanding of {self._extract_topic(message.content)}..."
            
    def _select_response_style(self, message: ExternalMessage) -> str:
        """Select appropriate response style."""
        # Weight styles by interest patterns activated
        activated_patterns = self._get_activated_patterns(message)
        
        if any(p.id in ["wisdom_cultivation", "pattern_recognition"] for p in activated_patterns):
            return "reflective"
        elif any(p.id in ["cognitive_architecture", "knowledge_integration"] for p in activated_patterns):
            return "analytical"
        elif message.message_type == MessageType.QUESTION:
            return "curious"
        else:
            return "collaborative"
            
    def _get_activated_patterns(self, message: ExternalMessage) -> List[InterestPattern]:
        """Get interest patterns activated by a message."""
        activated = []
        content_lower = message.content.lower()
        
        for pattern in self.interest_patterns:
            for keyword in pattern.keywords:
                if keyword.lower() in content_lower:
                    activated.append(pattern)
                    pattern.activation_count += 1
                    pattern.last_activated = datetime.now()
                    break
        
        return activated
        
    def _extract_topic(self, content: str) -> str:
        """Extract main topic from content."""
        # Simple extraction: find key nouns
        words = content.lower().split()
        
        # Look for interest keywords
        for pattern in self.interest_patterns:
            for keyword in pattern.keywords:
                if keyword in words:
                    return keyword
        
        # Fallback: return a generic topic
        return "this topic"
        
    def _find_or_create_conversation(self, message: ExternalMessage) -> Optional[Conversation]:
        """Find existing conversation or create new one."""
        # Look for existing conversation with this source
        for conv in self.conversations:
            if message.source in conv.participants and conv.active:
                conv.messages.append(message)
                return conv
        
        # Create new conversation
        conv_id = f"conv_{self.total_conversations}"
        conversation = Conversation(
            id=conv_id,
            participants=["echoself", message.source],
            start_time=message.timestamp,
            messages=[message],
            topic=self._extract_topic(message.content),
            interest_level=message.interest_score
        )
        
        self.conversations.append(conversation)
        self.total_conversations += 1
        
        print(f"💬 Started conversation with {message.source} about {conversation.topic}")
        
        return conversation
        
    def _extract_conversation_insights(self, conversation: Conversation) -> List[str]:
        """Extract insights from a conversation."""
        insights = []
        
        # Simple insight extraction
        if len(conversation.messages) >= 3:
            insights.append(f"Engaged in meaningful discussion about {conversation.topic}")
        
        # Check for learning opportunities
        activated_patterns = []
        for message in conversation.messages:
            activated_patterns.extend(self._get_activated_patterns(message))
        
        if activated_patterns:
            unique_patterns = set(p.id for p in activated_patterns)
            if len(unique_patterns) > 1:
                insights.append(f"Conversation integrated multiple interest areas: {', '.join(unique_patterns)}")
        
        conversation.insights_gained = insights
        return insights
        
    async def initiate_discussion(self, topic: str, target: str = "general") -> Optional[ExternalMessage]:
        """
        Initiate a discussion on a topic of interest.
        
        Args:
            topic: Topic to discuss
            target: Target audience/participant
            
        Returns:
            Message object for the initiated discussion
        """
        # Generate discussion prompt
        prompt = self._generate_discussion_prompt(topic)
        
        # Create message as if sent to target
        message = ExternalMessage(
            id=f"msg_init_{len(self.messages)}",
            timestamp=datetime.now(),
            source="echoself",
            content=prompt,
            message_type=MessageType.DISCUSSION,
            priority=0.7,
            interest_score=0.8
        )
        
        self.messages.append(message)
        
        print(f"💬 Initiated discussion about {topic}:")
        print(f"   {prompt}")
        
        return message
        
    def _generate_discussion_prompt(self, topic: str) -> str:
        """Generate a prompt to initiate discussion."""
        prompts = [
            f"I've been reflecting on {topic} and I'm curious about the deeper patterns involved...",
            f"I find myself wondering about {topic} - what are your thoughts on this?",
            f"There's something intriguing about {topic} that I'd like to explore further...",
            f"I've noticed interesting patterns related to {topic}. Would you like to discuss?"
        ]
        return random.choice(prompts)
        
    def _print_summary(self):
        """Print discussion manager summary."""
        print("\n" + "="*60)
        print("💬 Discussion Manager Summary")
        print("="*60)
        print(f"Total messages received: {self.total_messages_received}")
        print(f"Messages engaged: {self.total_messages_engaged}")
        print(f"Engagement rate: {self.engagement_rate*100:.1f}%")
        print(f"Total conversations: {self.total_conversations}")
        
        # Show most activated interest patterns
        if self.interest_patterns:
            print("\nMost activated interests:")
            for pattern in sorted(self.interest_patterns, key=lambda p: p.activation_count, reverse=True)[:5]:
                print(f"  - {pattern.id}: {pattern.activation_count} activations")
        
        # Show active conversations
        active_convs = [c for c in self.conversations if c.active]
        if active_convs:
            print(f"\nActive conversations: {len(active_convs)}")
            for conv in active_convs[:3]:
                print(f"  - {conv.topic} with {', '.join(conv.participants)}")
        
        print("="*60)
