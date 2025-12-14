"""
Communication Protocols for Social Cognition

Implements structured communication protocols that enable agents to collaborate
effectively through standardized messaging, negotiation, and coordination mechanisms.

This component supports the communication requirements for Task 2.3.2 of the
Deep Tree Echo development roadmap.
"""

import asyncio
import logging
import time
import uuid
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class MessageType(Enum):
    """Types of messages in collaborative communication."""
    REQUEST = "request"
    RESPONSE = "response"
    NOTIFICATION = "notification"
    PROPOSAL = "proposal"
    NEGOTIATION = "negotiation"
    AGREEMENT = "agreement"
    COORDINATION = "coordination"
    KNOWLEDGE_SHARE = "knowledge_share"
    TASK_DELEGATION = "task_delegation"
    STATUS_UPDATE = "status_update"


class ProtocolType(Enum):
    """Types of communication protocols."""
    DIRECT_MESSAGE = "direct_message"
    BROADCAST = "broadcast"
    CONSENSUS_BUILDING = "consensus_building"
    NEGOTIATION = "negotiation"
    TASK_COORDINATION = "task_coordination"
    KNOWLEDGE_EXCHANGE = "knowledge_exchange"
    CONTRACT_NET = "contract_net"  # Contract Net Protocol for task allocation
    AUCTION = "auction"


class MessagePriority(Enum):
    """Priority levels for messages."""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"
    URGENT = "urgent"


@dataclass
class Message:
    """Structured message for agent communication."""
    message_id: str
    sender_id: str
    recipient_id: str  # Can be "*" for broadcast
    message_type: MessageType
    protocol_type: ProtocolType
    content: Dict[str, Any]
    priority: MessagePriority = MessagePriority.NORMAL
    timestamp: float = field(default_factory=time.time)
    correlation_id: Optional[str] = None  # For request-response tracking
    deadline: Optional[float] = None
    requires_response: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ProtocolSession:
    """Active communication protocol session."""
    session_id: str
    protocol_type: ProtocolType
    participants: List[str]
    initiator: str
    status: str = "active"
    created_at: float = field(default_factory=time.time)
    messages: List[Message] = field(default_factory=list)
    session_data: Dict[str, Any] = field(default_factory=dict)
    completion_criteria: Dict[str, Any] = field(default_factory=dict)


class CommunicationProtocols:
    """Manages structured communication protocols for multi-agent collaboration."""
    
    def __init__(self):
        # Message routing and queues
        self.agent_message_queues: Dict[str, asyncio.Queue] = {}
        self.message_history: List[Message] = []
        self.max_message_history = 10000
        
        # Protocol sessions
        self.active_sessions: Dict[str, ProtocolSession] = {}
        self.protocol_handlers: Dict[ProtocolType, Callable] = {}
        
        # Communication metrics
        self.communication_stats = {
            'total_messages': 0,
            'successful_protocols': 0,
            'failed_protocols': 0,
            'avg_response_time': 0.0,
            'protocol_efficiency': {}
        }
        
        # Register built-in protocol handlers
        self._register_protocol_handlers()
        
        logger.info("Communication Protocols initialized")
    
    def _register_protocol_handlers(self) -> None:
        """Register built-in protocol handlers."""
        self.protocol_handlers = {
            ProtocolType.DIRECT_MESSAGE: self._handle_direct_message,
            ProtocolType.BROADCAST: self._handle_broadcast,
            ProtocolType.CONSENSUS_BUILDING: self._handle_consensus_building,
            ProtocolType.NEGOTIATION: self._handle_negotiation,
            ProtocolType.TASK_COORDINATION: self._handle_task_coordination,
            ProtocolType.KNOWLEDGE_EXCHANGE: self._handle_knowledge_exchange,
            ProtocolType.CONTRACT_NET: self._handle_contract_net,
            ProtocolType.AUCTION: self._handle_auction
        }
    
    async def register_agent(self, agent_id: str) -> None:
        """Register agent for communication."""
        if agent_id not in self.agent_message_queues:
            self.agent_message_queues[agent_id] = asyncio.Queue()
            logger.debug(f"Registered agent {agent_id} for communication")
    
    async def send_message(self, message: Message) -> bool:
        """Send message through appropriate protocol."""
        try:
            # Validate message
            if not await self._validate_message(message):
                logger.warning(f"Message validation failed: {message.message_id}")
                return False
            
            # Route message based on protocol
            protocol_handler = self.protocol_handlers.get(message.protocol_type)
            if protocol_handler:
                success = await protocol_handler(message)
            else:
                # Default routing
                success = await self._route_message_default(message)
            
            if success:
                # Store in history
                self.message_history.append(message)
                self._trim_message_history()
                
                # Update stats
                self.communication_stats['total_messages'] += 1
                
                logger.debug(f"Message {message.message_id} sent successfully via {message.protocol_type.value}")
            
            return success
            
        except Exception as e:
            logger.error(f"Error sending message {message.message_id}: {e}")
            return False
    
    async def receive_message(self, agent_id: str, timeout: float = 1.0) -> Optional[Message]:
        """Receive message for specific agent."""
        if agent_id not in self.agent_message_queues:
            await self.register_agent(agent_id)
        
        try:
            message = await asyncio.wait_for(
                self.agent_message_queues[agent_id].get(),
                timeout=timeout
            )
            logger.debug(f"Agent {agent_id} received message {message.message_id}")
            return message
        
        except asyncio.TimeoutError:
            return None
        except Exception as e:
            logger.error(f"Error receiving message for agent {agent_id}: {e}")
            return None
    
    async def initiate_protocol(self,
                              initiator_id: str,
                              protocol_type: ProtocolType,
                              participants: List[str],
                              session_data: Dict[str, Any]) -> str:
        """Initiate a structured communication protocol session."""
        
        session_id = f"proto_{uuid.uuid4().hex[:8]}"
        
        # Create protocol session
        session = ProtocolSession(
            session_id=session_id,
            protocol_type=protocol_type,
            participants=participants,
            initiator=initiator_id,
            session_data=session_data.copy()
        )
        
        self.active_sessions[session_id] = session
        
        # Send initialization message to participants
        init_message = Message(
            message_id=f"init_{uuid.uuid4().hex[:8]}",
            sender_id=initiator_id,
            recipient_id="*",  # Broadcast to participants
            message_type=MessageType.NOTIFICATION,
            protocol_type=protocol_type,
            content={
                'session_id': session_id,
                'protocol_type': protocol_type.value,
                'participants': participants,
                'session_data': session_data,
                'action': 'protocol_initiation'
            },
            priority=MessagePriority.HIGH,
            metadata={'session_id': session_id}
        )
        
        await self.send_message(init_message)
        
        logger.info(f"Initiated {protocol_type.value} protocol {session_id} with {len(participants)} participants")
        return session_id
    
    async def participate_in_protocol(self,
                                    agent_id: str,
                                    session_id: str,
                                    action: str,
                                    data: Dict[str, Any]) -> bool:
        """Participate in an active protocol session."""
        
        if session_id not in self.active_sessions:
            logger.warning(f"Protocol session {session_id} not found")
            return False
        
        session = self.active_sessions[session_id]
        
        # Check if agent is a participant
        if agent_id not in session.participants:
            logger.warning(f"Agent {agent_id} not a participant in session {session_id}")
            return False
        
        # Create participation message
        participation_message = Message(
            message_id=f"part_{uuid.uuid4().hex[:8]}",
            sender_id=agent_id,
            recipient_id=session.initiator,
            message_type=MessageType.RESPONSE,
            protocol_type=session.protocol_type,
            content={
                'session_id': session_id,
                'action': action,
                'data': data,
                'participant_id': agent_id
            },
            correlation_id=session_id,
            metadata={'session_id': session_id}
        )
        
        # Add to session messages
        session.messages.append(participation_message)
        
        # Process through protocol handler
        protocol_handler = self.protocol_handlers.get(session.protocol_type)
        if protocol_handler:
            await protocol_handler(participation_message)
        
        logger.debug(f"Agent {agent_id} participated in protocol {session_id} with action '{action}'")
        return True
    
    async def _validate_message(self, message: Message) -> bool:
        """Validate message format and content."""
        
        # Check required fields
        if not all([message.message_id, message.sender_id, message.recipient_id]):
            return False
        
        # Check if sender is registered
        if message.sender_id not in self.agent_message_queues:
            await self.register_agent(message.sender_id)
        
        # Check if recipient exists (unless broadcast)
        if message.recipient_id != "*" and message.recipient_id not in self.agent_message_queues:
            await self.register_agent(message.recipient_id)
        
        # Check deadline
        if message.deadline and time.time() > message.deadline:
            logger.warning(f"Message {message.message_id} exceeded deadline")
            return False
        
        return True
    
    async def _route_message_default(self, message: Message) -> bool:
        """Default message routing logic."""
        
        if message.recipient_id == "*":
            # Broadcast message
            success_count = 0
            for agent_id, queue in self.agent_message_queues.items():
                if agent_id != message.sender_id:  # Don't send to self
                    try:
                        await queue.put(message)
                        success_count += 1
                    except Exception as e:
                        logger.error(f"Failed to deliver message to {agent_id}: {e}")
            
            return success_count > 0
        
        else:
            # Direct message
            if message.recipient_id in self.agent_message_queues:
                try:
                    await self.agent_message_queues[message.recipient_id].put(message)
                    return True
                except Exception as e:
                    logger.error(f"Failed to deliver message to {message.recipient_id}: {e}")
            
            return False
    
    # Protocol Handlers
    
    async def _handle_direct_message(self, message: Message) -> bool:
        """Handle direct message protocol."""
        return await self._route_message_default(message)
    
    async def _handle_broadcast(self, message: Message) -> bool:
        """Handle broadcast protocol."""
        # Ensure recipient is set to broadcast
        message.recipient_id = "*"
        return await self._route_message_default(message)
    
    async def _handle_consensus_building(self, message: Message) -> bool:
        """Handle consensus building protocol."""
        session_id = message.metadata.get('session_id')
        if not session_id or session_id not in self.active_sessions:
            return await self._route_message_default(message)
        
        session = self.active_sessions[session_id]
        
        # Process consensus-related messages
        action = message.content.get('action', '')
        
        if action == 'proposal':
            # Handle proposal submission
            await self._process_consensus_proposal(session, message)
        elif action == 'vote':
            # Handle voting
            await self._process_consensus_vote(session, message)
        elif action == 'consensus_check':
            # Check if consensus reached
            await self._check_consensus_completion(session)
        
        # Route message to participants
        return await self._route_to_session_participants(session, message)
    
    async def _handle_negotiation(self, message: Message) -> bool:
        """Handle negotiation protocol."""
        session_id = message.metadata.get('session_id')
        if not session_id or session_id not in self.active_sessions:
            return await self._route_message_default(message)
        
        session = self.active_sessions[session_id]
        
        # Track negotiation rounds
        if 'negotiation_rounds' not in session.session_data:
            session.session_data['negotiation_rounds'] = []
        
        action = message.content.get('action', '')
        
        if action == 'offer':
            await self._process_negotiation_offer(session, message)
        elif action == 'counteroffer':
            await self._process_negotiation_counteroffer(session, message)
        elif action == 'accept':
            await self._process_negotiation_acceptance(session, message)
        elif action == 'reject':
            await self._process_negotiation_rejection(session, message)
        
        return await self._route_to_session_participants(session, message)
    
    async def _handle_task_coordination(self, message: Message) -> bool:
        """Handle task coordination protocol."""
        session_id = message.metadata.get('session_id')
        if not session_id or session_id not in self.active_sessions:
            return await self._route_message_default(message)
        
        session = self.active_sessions[session_id]
        
        action = message.content.get('action', '')
        
        if action == 'task_assignment':
            await self._process_task_assignment(session, message)
        elif action == 'status_update':
            await self._process_task_status_update(session, message)
        elif action == 'resource_request':
            await self._process_resource_request(session, message)
        elif action == 'coordination_sync':
            await self._process_coordination_sync(session, message)
        
        return await self._route_to_session_participants(session, message)
    
    async def _handle_knowledge_exchange(self, message: Message) -> bool:
        """Handle knowledge exchange protocol."""
        action = message.content.get('action', '')
        
        if action == 'knowledge_request':
            # Handle knowledge requests
            await self._process_knowledge_request(message)
        elif action == 'knowledge_share':
            # Handle knowledge sharing
            await self._process_knowledge_share(message)
        elif action == 'knowledge_validation':
            # Handle knowledge validation
            await self._process_knowledge_validation(message)
        
        return await self._route_message_default(message)
    
    async def _handle_contract_net(self, message: Message) -> bool:
        """Handle Contract Net Protocol for task allocation."""
        session_id = message.metadata.get('session_id')
        if not session_id or session_id not in self.active_sessions:
            return await self._route_message_default(message)
        
        session = self.active_sessions[session_id]
        
        action = message.content.get('action', '')
        
        if action == 'call_for_proposals':
            await self._process_cfp(session, message)
        elif action == 'proposal_submission':
            await self._process_proposal_submission(session, message)
        elif action == 'proposal_evaluation':
            await self._process_proposal_evaluation(session, message)
        elif action == 'contract_award':
            await self._process_contract_award(session, message)
        
        return await self._route_to_session_participants(session, message)
    
    async def _handle_auction(self, message: Message) -> bool:
        """Handle auction protocol."""
        session_id = message.metadata.get('session_id')
        if not session_id or session_id not in self.active_sessions:
            return await self._route_message_default(message)
        
        session = self.active_sessions[session_id]
        
        action = message.content.get('action', '')
        
        if action == 'bid':
            await self._process_auction_bid(session, message)
        elif action == 'bid_update':
            await self._process_bid_update(session, message)
        elif action == 'auction_close':
            await self._process_auction_close(session, message)
        
        return await self._route_to_session_participants(session, message)
    
    # Helper methods for protocol processing
    
    async def _route_to_session_participants(self, session: ProtocolSession, message: Message) -> bool:
        """Route message to all session participants."""
        success_count = 0
        
        for participant_id in session.participants:
            if participant_id != message.sender_id:  # Don't echo to sender
                participant_message = Message(
                    message_id=f"relay_{uuid.uuid4().hex[:8]}",
                    sender_id=message.sender_id,
                    recipient_id=participant_id,
                    message_type=message.message_type,
                    protocol_type=message.protocol_type,
                    content=message.content,
                    priority=message.priority,
                    correlation_id=message.correlation_id,
                    metadata=message.metadata
                )
                
                if participant_id in self.agent_message_queues:
                    try:
                        await self.agent_message_queues[participant_id].put(participant_message)
                        success_count += 1
                    except Exception as e:
                        logger.error(f"Failed to route message to participant {participant_id}: {e}")
        
        return success_count > 0
    
    async def _process_consensus_proposal(self, session: ProtocolSession, message: Message) -> None:
        """Process consensus proposal."""
        if 'proposals' not in session.session_data:
            session.session_data['proposals'] = []
        
        proposal = {
            'proposal_id': message.message_id,
            'proposer': message.sender_id,
            'content': message.content.get('proposal', {}),
            'votes': {},
            'timestamp': time.time()
        }
        
        session.session_data['proposals'].append(proposal)
    
    async def _process_consensus_vote(self, session: ProtocolSession, message: Message) -> None:
        """Process consensus vote."""
        proposal_id = message.content.get('proposal_id')
        vote = message.content.get('vote')  # 'yes', 'no', 'abstain'
        
        if proposal_id and vote:
            for proposal in session.session_data.get('proposals', []):
                if proposal['proposal_id'] == proposal_id:
                    proposal['votes'][message.sender_id] = vote
                    break
    
    async def _check_consensus_completion(self, session: ProtocolSession) -> None:
        """Check if consensus has been reached."""
        proposals = session.session_data.get('proposals', [])
        threshold = len(session.participants) * 0.67  # 2/3 majority
        
        for proposal in proposals:
            votes = proposal['votes']
            yes_votes = sum(1 for vote in votes.values() if vote == 'yes')
            
            if yes_votes >= threshold:
                session.status = 'completed'
                session.session_data['consensus_reached'] = True
                session.session_data['accepted_proposal'] = proposal
                break
    
    async def _process_negotiation_offer(self, session: ProtocolSession, message: Message) -> None:
        """Process negotiation offer."""
        if 'offers' not in session.session_data:
            session.session_data['offers'] = []
        
        offer = {
            'offer_id': message.message_id,
            'offeror': message.sender_id,
            'terms': message.content.get('terms', {}),
            'timestamp': time.time(),
            'status': 'pending'
        }
        
        session.session_data['offers'].append(offer)
    
    async def _process_negotiation_counteroffer(self, session: ProtocolSession, message: Message) -> None:
        """Process negotiation counteroffer."""
        original_offer_id = message.content.get('original_offer_id')
        
        counteroffer = {
            'offer_id': message.message_id,
            'offeror': message.sender_id,
            'original_offer_id': original_offer_id,
            'terms': message.content.get('terms', {}),
            'timestamp': time.time(),
            'status': 'pending'
        }
        
        if 'offers' not in session.session_data:
            session.session_data['offers'] = []
        session.session_data['offers'].append(counteroffer)
    
    async def _process_negotiation_acceptance(self, session: ProtocolSession, message: Message) -> None:
        """Process negotiation acceptance."""
        offer_id = message.content.get('offer_id')
        
        for offer in session.session_data.get('offers', []):
            if offer['offer_id'] == offer_id:
                offer['status'] = 'accepted'
                session.status = 'completed'
                session.session_data['agreement_reached'] = True
                session.session_data['final_agreement'] = offer
                break
    
    async def _process_negotiation_rejection(self, session: ProtocolSession, message: Message) -> None:
        """Process negotiation rejection."""
        offer_id = message.content.get('offer_id')
        
        for offer in session.session_data.get('offers', []):
            if offer['offer_id'] == offer_id:
                offer['status'] = 'rejected'
                break
    
    async def _process_task_assignment(self, session: ProtocolSession, message: Message) -> None:
        """Process task assignment."""
        if 'task_assignments' not in session.session_data:
            session.session_data['task_assignments'] = {}
        
        assignee = message.content.get('assignee')
        task = message.content.get('task', {})
        
        if assignee:
            session.session_data['task_assignments'][assignee] = {
                'task': task,
                'assigned_by': message.sender_id,
                'assigned_at': time.time(),
                'status': 'assigned'
            }
    
    async def _process_task_status_update(self, session: ProtocolSession, message: Message) -> None:
        """Process task status update."""
        message.content.get('task_id')
        status = message.content.get('status')
        progress = message.content.get('progress', 0.0)
        
        # Update task status in session data
        assignments = session.session_data.get('task_assignments', {})
        agent_id = message.sender_id
        
        if agent_id in assignments:
            assignments[agent_id]['status'] = status
            assignments[agent_id]['progress'] = progress
            assignments[agent_id]['last_update'] = time.time()
    
    async def _process_resource_request(self, session: ProtocolSession, message: Message) -> None:
        """Process resource request."""
        if 'resource_requests' not in session.session_data:
            session.session_data['resource_requests'] = []
        
        request = {
            'request_id': message.message_id,
            'requester': message.sender_id,
            'resource_type': message.content.get('resource_type'),
            'amount': message.content.get('amount', 1),
            'urgency': message.content.get('urgency', 'normal'),
            'timestamp': time.time(),
            'status': 'pending'
        }
        
        session.session_data['resource_requests'].append(request)
    
    async def _process_coordination_sync(self, session: ProtocolSession, message: Message) -> None:
        """Process coordination synchronization."""
        if 'sync_points' not in session.session_data:
            session.session_data['sync_points'] = {}
        
        sync_point = message.content.get('sync_point')
        if sync_point:
            if sync_point not in session.session_data['sync_points']:
                session.session_data['sync_points'][sync_point] = []
            
            session.session_data['sync_points'][sync_point].append({
                'agent_id': message.sender_id,
                'timestamp': time.time(),
                'data': message.content.get('data', {})
            })
    
    async def _process_knowledge_request(self, message: Message) -> None:
        """Process knowledge request."""
        # Create response message template
        knowledge_type = message.content.get('knowledge_type')
        query = message.content.get('query')
        
        # This would typically query a knowledge base
        # For now, we'll create a placeholder response
        response_message = Message(
            message_id=f"know_resp_{uuid.uuid4().hex[:8]}",
            sender_id="knowledge_system",
            recipient_id=message.sender_id,
            message_type=MessageType.RESPONSE,
            protocol_type=ProtocolType.KNOWLEDGE_EXCHANGE,
            content={
                'action': 'knowledge_response',
                'knowledge_type': knowledge_type,
                'query': query,
                'response': f"Knowledge response for {knowledge_type}",
                'confidence': 0.8
            },
            correlation_id=message.message_id
        )
        
        await self.send_message(response_message)
    
    async def _process_knowledge_share(self, message: Message) -> None:
        """Process knowledge sharing."""
        message.content.get('knowledge', {})
        knowledge_type = message.content.get('knowledge_type')
        
        # Store or process shared knowledge
        logger.info(f"Agent {message.sender_id} shared {knowledge_type} knowledge")
    
    async def _process_knowledge_validation(self, message: Message) -> None:
        """Process knowledge validation."""
        knowledge_id = message.content.get('knowledge_id')
        validation_result = message.content.get('validation_result')
        
        logger.info(f"Knowledge {knowledge_id} validated by {message.sender_id}: {validation_result}")
    
    async def _process_cfp(self, session: ProtocolSession, message: Message) -> None:
        """Process Call for Proposals in Contract Net Protocol."""
        if 'call_for_proposals' not in session.session_data:
            session.session_data['call_for_proposals'] = {}
        
        cfp = {
            'task_description': message.content.get('task_description'),
            'requirements': message.content.get('requirements', {}),
            'deadline': message.content.get('deadline'),
            'evaluation_criteria': message.content.get('evaluation_criteria', {}),
            'proposals_received': [],
            'status': 'open'
        }
        
        session.session_data['call_for_proposals'] = cfp
    
    async def _process_proposal_submission(self, session: ProtocolSession, message: Message) -> None:
        """Process proposal submission in Contract Net Protocol."""
        cfp = session.session_data.get('call_for_proposals')
        if cfp and cfp['status'] == 'open':
            proposal = {
                'proposal_id': message.message_id,
                'proposer': message.sender_id,
                'proposal_content': message.content.get('proposal', {}),
                'cost': message.content.get('cost'),
                'timeline': message.content.get('timeline'),
                'submitted_at': time.time()
            }
            
            cfp['proposals_received'].append(proposal)
    
    async def _process_proposal_evaluation(self, session: ProtocolSession, message: Message) -> None:
        """Process proposal evaluation in Contract Net Protocol."""
        proposal_id = message.content.get('proposal_id')
        evaluation_score = message.content.get('evaluation_score', 0.0)
        
        cfp = session.session_data.get('call_for_proposals')
        if cfp:
            for proposal in cfp['proposals_received']:
                if proposal['proposal_id'] == proposal_id:
                    proposal['evaluation_score'] = evaluation_score
                    break
    
    async def _process_contract_award(self, session: ProtocolSession, message: Message) -> None:
        """Process contract award in Contract Net Protocol."""
        winning_proposal_id = message.content.get('winning_proposal_id')
        
        cfp = session.session_data.get('call_for_proposals')
        if cfp:
            for proposal in cfp['proposals_received']:
                if proposal['proposal_id'] == winning_proposal_id:
                    proposal['status'] = 'awarded'
                    cfp['winning_proposal'] = proposal
                    cfp['status'] = 'awarded'
                    session.status = 'completed'
                    break
    
    async def _process_auction_bid(self, session: ProtocolSession, message: Message) -> None:
        """Process auction bid."""
        if 'bids' not in session.session_data:
            session.session_data['bids'] = []
        
        bid = {
            'bid_id': message.message_id,
            'bidder': message.sender_id,
            'amount': message.content.get('amount'),
            'timestamp': time.time(),
            'status': 'active'
        }
        
        session.session_data['bids'].append(bid)
        
        # Update current highest bid
        all_bids = [b for b in session.session_data['bids'] if b['status'] == 'active']
        if all_bids:
            highest_bid = max(all_bids, key=lambda b: b['amount'])
            session.session_data['highest_bid'] = highest_bid
    
    async def _process_bid_update(self, session: ProtocolSession, message: Message) -> None:
        """Process bid update."""
        bid_id = message.content.get('bid_id')
        new_amount = message.content.get('new_amount')
        
        for bid in session.session_data.get('bids', []):
            if bid['bid_id'] == bid_id and bid['bidder'] == message.sender_id:
                bid['amount'] = new_amount
                bid['timestamp'] = time.time()
                break
    
    async def _process_auction_close(self, session: ProtocolSession, message: Message) -> None:
        """Process auction close."""
        session.status = 'completed'
        
        # Determine winning bid
        active_bids = [b for b in session.session_data.get('bids', []) if b['status'] == 'active']
        if active_bids:
            winning_bid = max(active_bids, key=lambda b: b['amount'])
            session.session_data['winning_bid'] = winning_bid
            
            # Mark other bids as lost
            for bid in active_bids:
                if bid != winning_bid:
                    bid['status'] = 'lost'
            winning_bid['status'] = 'won'
    
    def _trim_message_history(self) -> None:
        """Trim message history to maintain reasonable size."""
        if len(self.message_history) > self.max_message_history:
            # Keep most recent messages
            self.message_history = self.message_history[-self.max_message_history//2:]
    
    async def complete_protocol_session(self, session_id: str) -> Dict[str, Any]:
        """Complete and archive a protocol session."""
        if session_id not in self.active_sessions:
            return {'error': 'Session not found'}
        
        session = self.active_sessions[session_id]
        session.status = 'completed'
        
        # Calculate session metrics
        duration = time.time() - session.created_at
        message_count = len(session.messages)
        
        completion_result = {
            'session_id': session_id,
            'protocol_type': session.protocol_type.value,
            'duration': duration,
            'message_count': message_count,
            'participants': session.participants,
            'completion_data': session.session_data,
            'status': session.status
        }
        
        # Update communication stats
        if session.status == 'completed':
            self.communication_stats['successful_protocols'] += 1
        else:
            self.communication_stats['failed_protocols'] += 1
        
        # Update protocol efficiency tracking
        protocol_type = session.protocol_type.value
        if protocol_type not in self.communication_stats['protocol_efficiency']:
            self.communication_stats['protocol_efficiency'][protocol_type] = {'total_sessions': 0, 'successful': 0, 'avg_duration': 0.0}
        
        efficiency_data = self.communication_stats['protocol_efficiency'][protocol_type]
        efficiency_data['total_sessions'] += 1
        if session.status == 'completed':
            efficiency_data['successful'] += 1
        
        # Update average duration
        old_avg = efficiency_data['avg_duration']
        total_sessions = efficiency_data['total_sessions']
        new_avg = ((old_avg * (total_sessions - 1)) + duration) / total_sessions
        efficiency_data['avg_duration'] = new_avg
        
        # Remove from active sessions
        del self.active_sessions[session_id]
        
        logger.info(f"Completed protocol session {session_id} ({session.protocol_type.value}) in {duration:.2f}s")
        return completion_result
    
    def get_communication_stats(self) -> Dict[str, Any]:
        """Get comprehensive communication statistics."""
        active_sessions_by_protocol = {}
        for session in self.active_sessions.values():
            protocol = session.protocol_type.value
            active_sessions_by_protocol[protocol] = active_sessions_by_protocol.get(protocol, 0) + 1
        
        return {
            'communication_stats': self.communication_stats,
            'active_sessions': {
                'total': len(self.active_sessions),
                'by_protocol': active_sessions_by_protocol
            },
            'registered_agents': len(self.agent_message_queues),
            'message_history_size': len(self.message_history),
            'supported_protocols': [p.value for p in ProtocolType]
        }
    
    async def shutdown(self) -> None:
        """Gracefully shutdown communication protocols."""
        logger.info("Shutting down Communication Protocols...")
        
        # Complete any active sessions
        active_session_ids = list(self.active_sessions.keys())
        for session_id in active_session_ids:
            await self.complete_protocol_session(session_id)
        
        # Clear message queues
        for queue in self.agent_message_queues.values():
            try:
                while not queue.empty():
                    queue.get_nowait()
            except asyncio.QueueEmpty:
                pass
        
        self.agent_message_queues.clear()
        self.message_history.clear()
        
        logger.info("Communication Protocols shutdown complete")