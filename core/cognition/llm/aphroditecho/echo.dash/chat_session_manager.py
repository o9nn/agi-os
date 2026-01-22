import json
import uuid
import time
import logging
import threading
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field, asdict
from enum import Enum
HypergraphMemory = None
MemoryType = None
MemoryNode = None
TreeNode = None
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
class ChatPlatform(Enum):
    CHATGPT = 'chatgpt'
    CLAUDE = 'claude'
    WINDSURF = 'windsurf'
    BROWSER = 'browser'
    API = 'api'
    UNKNOWN = 'unknown'
class SessionStatus(Enum):
    ACTIVE = 'active'
    PAUSED = 'paused'
    ENDED = 'ended'
    ARCHIVED = 'archived'
@dataclass
class ChatMessage:
    id: str
    session_id: str
    timestamp: float
    platform: ChatPlatform
    role: str
    content: str
    metadata: Dict[str, Any] = field(default_factory=dict)
    echo_value: float = 0.0
    salience: float = 0.5
    parent_id: Optional[str] = None
    conversation_id: Optional[str] = None
    def to_dict(self) -> Dict:
        data = asdict(self)
        data['platform'] = self.platform.value
        return data
    @classmethod
    def from_dict(cls, data: Dict) -> 'ChatMessage':
        data['platform'] = ChatPlatform(data['platform'])
        return cls(**data)
@dataclass
class ChatSession:
    id: str
    platform: ChatPlatform
    title: str
    start_time: float
    end_time: Optional[float] = None
    status: SessionStatus = SessionStatus.ACTIVE
    messages: List[ChatMessage] = field(default_factory=list)
    conversation_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    tags: List[str] = field(default_factory=list)
    total_messages: int = 0
    avg_echo_value: float = 0.0
    avg_salience: float = 0.5
    def to_dict(self) -> Dict:
        data = asdict(self)
        data['platform'] = self.platform.value
        data['status'] = self.status.value
        data['messages'] = [msg.to_dict() for msg in self.messages]
        return data
    @classmethod
    def from_dict(cls, data: Dict) -> 'ChatSession':
        data['platform'] = ChatPlatform(data['platform'])
        data['status'] = SessionStatus(data['status'])
        data['messages'] = [ChatMessage.from_dict(msg) for msg in data['messages']]
        return cls(**data)
    def add_message(self, message: ChatMessage):
        self.messages.append(message)
        self.total_messages = len(self.messages)
        self._update_statistics()
    def _update_statistics(self):
        if self.messages:
            self.avg_echo_value = sum((msg.echo_value for msg in self.messages)) / len(self.messages)
            self.avg_salience = sum((msg.salience for msg in self.messages)) / len(self.messages)
class ChatSessionManager:
    def __init__(self, storage_dir: str='memory_storage'):
        self.logger = logging.getLogger(__name__)
        self.storage_dir = Path(storage_dir)
        self.sessions_dir = self.storage_dir / 'chat_sessions'
        self.indices_dir = self.storage_dir / 'session_indices'
        self.sessions_dir.mkdir(parents=True, exist_ok=True)
        self.indices_dir.mkdir(parents=True, exist_ok=True)
        if HypergraphMemory:
            self.memory_system = HypergraphMemory(storage_dir=storage_dir)
        else:
            self.memory_system = None
        self.active_sessions: Dict[str, ChatSession] = {}
        self.session_indices: Dict[str, Dict] = {}
        self.auto_save_interval = 30
        self.auto_save_thread = None
        self.running = False
        self._load_session_indices()
        self._load_active_sessions()
    def start_auto_save(self):
        if not self.running:
            self.running = True
            self.auto_save_thread = threading.Thread(target=self._auto_save_loop, daemon=True)
            self.auto_save_thread.start()
            self.logger.info('Started automatic session saving')
    def stop_auto_save(self):
        self.running = False
        if self.auto_save_thread:
            self.auto_save_thread.join()
        self.logger.info('Stopped automatic session saving')
    def _auto_save_loop(self):
        while self.running:
            try:
                self._save_active_sessions()
                time.sleep(self.auto_save_interval)
            except Exception as e:
                self.logger.error(f'Error in auto-save loop: {str(e)}')
    def create_session(self, platform: ChatPlatform, title: str=None, conversation_id: str=None, metadata: Dict=None) -> str:
        session_id = str(uuid.uuid4())
        if title is None:
            title = f"{platform.value.title()} Session - {datetime.now().strftime('%Y-%m-%d %H:%M')}"
        session = ChatSession(id=session_id, platform=platform, title=title, start_time=time.time(), conversation_id=conversation_id, metadata=metadata or {})
        self.active_sessions[session_id] = session
        self._update_session_index(session)
        self.logger.info(f'Created new {platform.value} session: {session_id}')
        return session_id
    def add_message(self, session_id: str, role: str, content: str, platform: ChatPlatform=None, metadata: Dict=None, conversation_id: str=None, parent_id: str=None) -> str:
        if session_id not in self.active_sessions:
            if platform is None:
                platform = ChatPlatform.UNKNOWN
            session_id = self.create_session(platform)
        session = self.active_sessions[session_id]
        message_id = str(uuid.uuid4())
        message = ChatMessage(id=message_id, session_id=session_id, timestamp=time.time(), platform=session.platform, role=role, content=content, metadata=metadata or {}, conversation_id=conversation_id, parent_id=parent_id)
        message.echo_value = self._calculate_echo_value(content)
        message.salience = self._calculate_salience(content, role)
        session.add_message(message)
        self._store_in_memory_system(message)
        self.logger.debug(f'Added message to session {session_id}: {role} - {len(content)} chars')
        return message_id
    def end_session(self, session_id: str):
        if session_id in self.active_sessions:
            session = self.active_sessions[session_id]
            session.end_time = time.time()
            session.status = SessionStatus.ENDED
            self._save_session(session)
            self._update_session_index(session)
            del self.active_sessions[session_id]
            self.logger.info(f'Ended session {session_id} with {session.total_messages} messages')
    def get_session(self, session_id: str) -> Optional[ChatSession]:
        if session_id in self.active_sessions:
            return self.active_sessions[session_id]
        return self._load_session(session_id)
    def search_sessions(self, query: str=None, platform: ChatPlatform=None, start_date: datetime=None, end_date: datetime=None, tags: List[str]=None, limit: int=50) -> List[Dict]:
        results = []
        for session_info in self.session_indices.values():
            if platform and session_info.get('platform') != platform.value:
                continue
            if start_date and session_info.get('start_time', 0) < start_date.timestamp():
                continue
            if end_date and session_info.get('start_time', 0) > end_date.timestamp():
                continue
            if tags and (not any((tag in session_info.get('tags', []) for tag in tags))):
                continue
            if query and query.lower() not in session_info.get('title', '').lower():
                continue
            results.append(session_info)
            if len(results) >= limit:
                break
        results.sort(key=lambda x: x.get('start_time', 0), reverse=True)
        return results
    def get_conversation_history(self, platform: ChatPlatform=None, days: int=7) -> List[ChatMessage]:
        cutoff_time = time.time() - days * 24 * 3600
        messages = []
        for session in self.active_sessions.values():
            if platform is None or session.platform == platform:
                for msg in session.messages:
                    if msg.timestamp >= cutoff_time:
                        messages.append(msg)
        recent_sessions = self.search_sessions(start_date=datetime.fromtimestamp(cutoff_time), platform=platform, limit=100)
        for session_info in recent_sessions:
            session = self._load_session(session_info['id'])
            if session:
                for msg in session.messages:
                    if msg.timestamp >= cutoff_time:
                        messages.append(msg)
        messages.sort(key=lambda x: x.timestamp)
        return messages
    def aggregate_conversations(self, target_session_id: str=None) -> str:
        if target_session_id is None:
            target_session_id = self.create_session(ChatPlatform.UNKNOWN, f"Aggregated Session - {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        all_messages = self.get_conversation_history(days=1)
        threads = self._group_messages_by_thread(all_messages)
        target_session = self.active_sessions[target_session_id]
        for thread in threads:
            for msg in thread:
                aggregated_msg = ChatMessage(id=str(uuid.uuid4()), session_id=target_session_id, timestamp=msg.timestamp, platform=msg.platform, role=msg.role, content=msg.content, metadata={**msg.metadata, 'original_session': msg.session_id}, echo_value=msg.echo_value, salience=msg.salience, parent_id=msg.parent_id, conversation_id=msg.conversation_id)
                target_session.add_message(aggregated_msg)
        self.logger.info(f'Aggregated {len(all_messages)} messages into session {target_session_id}')
        return target_session_id
    def integrate_with_existing_storage(self):
        selenium_memory_file = Path('activity_logs/browser/chat_memory.json')
        if selenium_memory_file.exists():
            self._import_selenium_memory(selenium_memory_file)
        self._import_cognitive_architecture_memory()
        self._import_evolution_memory()
    def _import_selenium_memory(self, memory_file: Path):
        try:
            with open(memory_file, 'r') as f:
                conversations = json.load(f)
            if conversations:
                session_id = self.create_session(ChatPlatform.BROWSER, 'Imported Browser Session', metadata={'imported_from': str(memory_file)})
                for conv in conversations:
                    if 'message' in conv:
                        self.add_message(session_id, 'user', conv['message'], platform=ChatPlatform.BROWSER, metadata={'timestamp': conv.get('timestamp'), 'url': conv.get('url')})
                    if 'response' in conv and conv['response']:
                        self.add_message(session_id, 'assistant', conv['response'], platform=ChatPlatform.BROWSER, metadata={'timestamp': conv.get('timestamp'), 'url': conv.get('url')})
                self.logger.info(f'Imported {len(conversations)} conversations from selenium memory')
        except Exception as e:
            self.logger.error(f'Error importing selenium memory: {str(e)}')
    def _import_cognitive_architecture_memory(self):
        try:
            memory_dir = Path('memory_storage')
            if memory_dir.exists():
                activities_file = memory_dir / 'activities.json'
                if activities_file.exists():
                    with open(activities_file, 'r') as f:
                        activities = json.load(f)
                    chat_activities = [a for a in activities if 'chat' in a.get('type', '').lower()]
                    if chat_activities:
                        session_id = self.create_session(ChatPlatform.UNKNOWN, 'Imported Cognitive Activities', metadata={'imported_from': 'cognitive_architecture'})
                        for activity in chat_activities:
                            self.add_message(session_id, 'system', str(activity.get('details', '')), metadata={'activity_type': activity.get('type')})
        except Exception as e:
            self.logger.error(f'Error importing cognitive architecture memory: {str(e)}')
    def _import_evolution_memory(self):
        try:
            evolution_file = Path('memory_storage/evolution_memory.json')
            if evolution_file.exists():
                with open(evolution_file, 'r') as f:
                    evolution_data = json.load(f)
                cycles = evolution_data.get('cycles', [])
                if cycles:
                    session_id = self.create_session(ChatPlatform.UNKNOWN, 'Evolution Memory Cycles', metadata={'imported_from': 'evolution_memory'})
                    for cycle in cycles:
                        self.add_message(session_id, 'system', json.dumps(cycle), metadata={'cycle_type': 'evolution'})
        except Exception as e:
            self.logger.error(f'Error importing evolution memory: {str(e)}')
    def _calculate_echo_value(self, content: str) -> float:
        try:
            if TreeNode:
                TreeNode(content)
                word_count = len(content.split())
                complexity = min(word_count / 100.0, 1.0)
                return complexity * 0.8
            else:
                word_count = len(content.split())
                complexity = min(word_count / 100.0, 1.0)
                if any((term in content.lower() for term in ['error', 'code', 'function', 'class', 'import'])):
                    complexity += 0.2
                if '?' in content:
                    complexity += 0.1
                return min(complexity * 0.8, 1.0)
        except Exception as e:
            self.logger.debug(f'Error calculating echo value: {str(e)}')
            return 0.5
    def _calculate_salience(self, content: str, role: str) -> float:
        salience = 0.5
        if '?' in content:
            salience += 0.1
        word_count = len(content.split())
        if word_count > 50:
            salience += 0.1
        if role == 'user':
            salience += 0.1
        important_keywords = ['error', 'problem', 'help', 'important', 'critical', 'urgent']
        if any((keyword in content.lower() for keyword in important_keywords)):
            salience += 0.2
        return min(salience, 1.0)
    def _store_in_memory_system(self, message: ChatMessage):
        try:
            if not self.memory_system:
                self.logger.debug(f'Memory system not available - would store: {message.role} message')
                return
            if message.role == 'user':
                memory_type = MemoryType.EPISODIC
            elif message.role == 'assistant':
                memory_type = MemoryType.SEMANTIC
            else:
                memory_type = MemoryType.WORKING
            memory_node = MemoryNode(id=message.id, content=message.content, memory_type=memory_type, creation_time=message.timestamp, salience=message.salience, echo_value=message.echo_value, source=f'{message.platform.value}_chat', metadata={'session_id': message.session_id, 'role': message.role, 'platform': message.platform.value, 'conversation_id': message.conversation_id, **message.metadata})
            self.memory_system.add_node(memory_node)
            self._create_message_associations(message)
        except Exception as e:
            self.logger.error(f'Error storing message in memory system: {str(e)}')
    def _create_message_associations(self, message: ChatMessage):
        try:
            if not self.memory_system:
                session = self.active_sessions.get(message.session_id)
                if session and len(session.messages) > 1:
                    prev_message = session.messages[-2]
                    self.logger.debug(f'Would create association: {prev_message.id} -> {message.id}')
                return
            session = self.active_sessions.get(message.session_id)
            if session and len(session.messages) > 1:
                prev_message = session.messages[-2]
                self.memory_system.add_edge(prev_message.id, message.id, 'conversation_flow', strength=0.8, metadata={'temporal_distance': message.timestamp - prev_message.timestamp})
                if prev_message.role == 'user' and message.role == 'assistant':
                    self.memory_system.add_edge(prev_message.id, message.id, 'question_answer', strength=0.9, metadata={'response_type': 'answer'})
        except Exception as e:
            self.logger.error(f'Error creating message associations: {str(e)}')
    def _group_messages_by_thread(self, messages: List[ChatMessage]) -> List[List[ChatMessage]]:
        threads = []
        current_thread = []
        for message in sorted(messages, key=lambda x: x.timestamp):
            if current_thread and (message.timestamp - current_thread[-1].timestamp > 3600 or message.platform != current_thread[-1].platform):
                threads.append(current_thread)
                current_thread = [message]
            else:
                current_thread.append(message)
        if current_thread:
            threads.append(current_thread)
        return threads
    def _save_session(self, session: ChatSession):
        try:
            session_file = self.sessions_dir / f'{session.id}.json'
            with open(session_file, 'w') as f:
                json.dump(session.to_dict(), f, indent=2)
        except Exception as e:
            self.logger.error(f'Error saving session {session.id}: {str(e)}')
    def _load_session(self, session_id: str) -> Optional[ChatSession]:
        try:
            session_file = self.sessions_dir / f'{session_id}.json'
            if session_file.exists():
                with open(session_file, 'r') as f:
                    data = json.load(f)
                return ChatSession.from_dict(data)
        except Exception as e:
            self.logger.error(f'Error loading session {session_id}: {str(e)}')
        return None
    def _save_active_sessions(self):
        for session in self.active_sessions.values():
            self._save_session(session)
        self._save_session_indices()
    def _load_active_sessions(self):
        cutoff_time = time.time() - 24 * 3600
        for session_info in self.session_indices.values():
            if session_info.get('status') == SessionStatus.ACTIVE.value and session_info.get('start_time', 0) > cutoff_time:
                session = self._load_session(session_info['id'])
                if session:
                    self.active_sessions[session.id] = session
        self.logger.info(f'Loaded {len(self.active_sessions)} active sessions')
    def _update_session_index(self, session: ChatSession):
        self.session_indices[session.id] = {'id': session.id, 'platform': session.platform.value, 'title': session.title, 'start_time': session.start_time, 'end_time': session.end_time, 'status': session.status.value, 'total_messages': session.total_messages, 'avg_echo_value': session.avg_echo_value, 'avg_salience': session.avg_salience, 'tags': session.tags, 'metadata': session.metadata}
    def _save_session_indices(self):
        try:
            indices_file = self.indices_dir / 'sessions.json'
            with open(indices_file, 'w') as f:
                json.dump(self.session_indices, f, indent=2)
        except Exception as e:
            self.logger.error(f'Error saving session indices: {str(e)}')
    def _load_session_indices(self):
        try:
            indices_file = self.indices_dir / 'sessions.json'
            if indices_file.exists():
                with open(indices_file, 'r') as f:
                    self.session_indices = json.load(f)
                self.logger.info(f'Loaded {len(self.session_indices)} session indices')
        except Exception as e:
            self.logger.error(f'Error loading session indices: {str(e)}')
            self.session_indices = {}
    def get_statistics(self) -> Dict:
        stats = {'total_sessions': len(self.session_indices), 'active_sessions': len(self.active_sessions), 'platforms': {}, 'total_messages': 0, 'avg_session_length': 0, 'most_active_platform': None, 'recent_activity': {}}
        for session_info in self.session_indices.values():
            platform = session_info.get('platform', 'unknown')
            if platform not in stats['platforms']:
                stats['platforms'][platform] = {'sessions': 0, 'messages': 0}
            stats['platforms'][platform]['sessions'] += 1
            stats['platforms'][platform]['messages'] += session_info.get('total_messages', 0)
            stats['total_messages'] += session_info.get('total_messages', 0)
        if stats['total_sessions'] > 0:
            stats['avg_session_length'] = stats['total_messages'] / stats['total_sessions']
        if stats['platforms']:
            stats['most_active_platform'] = max(stats['platforms'].keys(), key=lambda p: stats['platforms'][p]['messages'])
        cutoff_time = time.time() - 7 * 24 * 3600
        recent_sessions = [s for s in self.session_indices.values() if s.get('start_time', 0) > cutoff_time]
        stats['recent_activity'] = {'sessions': len(recent_sessions), 'messages': sum((s.get('total_messages', 0) for s in recent_sessions))}
        return stats
session_manager = ChatSessionManager()
def initialize_session_manager():
    session_manager.integrate_with_existing_storage()
    session_manager.start_auto_save()
    logger.info('Chat session manager initialized')
def create_chat_session(platform: str, title: str=None, **kwargs) -> str:
    platform_enum = ChatPlatform(platform.lower()) if platform else ChatPlatform.UNKNOWN
    return session_manager.create_session(platform_enum, title, **kwargs)
def log_chat_message(session_id: str, role: str, content: str, **kwargs) -> str:
    return session_manager.add_message(session_id, role, content, **kwargs)
def end_chat_session(session_id: str):
    session_manager.end_session(session_id)
def get_chat_history(platform: str=None, days: int=7) -> List[Dict]:
    platform_enum = ChatPlatform(platform.lower()) if platform else None
    messages = session_manager.get_conversation_history(platform_enum, days)
    return [msg.to_dict() for msg in messages]
if __name__ == '__main__':
    initialize_session_manager()
    session_id = create_chat_session('chatgpt', 'Test Session')
    log_chat_message(session_id, 'user', 'Hello, how are you?')
    log_chat_message(session_id, 'assistant', "I'm doing well, thank you for asking!")
    stats = session_manager.get_statistics()
    print(f'Session statistics: {json.dumps(stats, indent=2)}')
    end_chat_session(session_id)
    logger.info('Test completed successfully')