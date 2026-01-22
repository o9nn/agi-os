import logging
import uuid
from datetime import datetime, timedelta
from typing import Dict, List, Any
logger = logging.getLogger(__name__)
class UserTopics:
    def __init__(self):
        self.user_id = 'default_user'
        self.forums = {}
        self.threads = {}
        self.messages = {}
        self.message_reactions = {}
        self.forum_threads = {}
        self.thread_messages = {}
        self.message_child_messages = {}
        self.message_reactions_map = {}
        logger.info('User topics module initialized')
    def create_forum(self, name: str, forum_type: str='discussion', description: str=None, visibility: str='public', tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        forum_id = str(uuid.uuid4())
        self.forums[forum_id] = {'id': forum_id, 'name': name, 'forum_type': forum_type, 'user_id': self.user_id, 'description': description or f'Forum: {name}', 'visibility': visibility, 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.forum_threads[forum_id] = []
        logger.info(f"Created forum '{name}' with ID {forum_id}")
        return forum_id
    def create_thread(self, forum_id: str, title: str, content: str=None, thread_type: str='discussion', tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if forum_id not in self.forums:
            logger.error(f'Forum {forum_id} not found')
            return None
        thread_id = str(uuid.uuid4())
        self.threads[thread_id] = {'id': thread_id, 'title': title, 'forum_id': forum_id, 'user_id': self.user_id, 'thread_type': thread_type, 'is_pinned': False, 'is_locked': False, 'views': 0, 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now(), 'last_activity_at': datetime.now()}
        self.forum_threads[forum_id].append(thread_id)
        self.thread_messages[thread_id] = []
        if content:
            self._create_message(thread_id, content, None, attributes)
        logger.info(f"Created thread '{title}' in forum '{self.forums[forum_id]['name']}'")
        return thread_id
    def _create_message(self, thread_id: str, content: str, parent_message_id: str=None, attributes: Dict[str, Any]=None) -> str:
        message_id = str(uuid.uuid4())
        self.messages[message_id] = {'id': message_id, 'thread_id': thread_id, 'parent_message_id': parent_message_id, 'user_id': self.user_id, 'content': content, 'is_edited': False, 'is_deleted': False, 'is_accepted_answer': False, 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.thread_messages[thread_id].append(message_id)
        if parent_message_id:
            if parent_message_id not in self.message_child_messages:
                self.message_child_messages[parent_message_id] = []
            self.message_child_messages[parent_message_id].append(message_id)
        self.message_reactions_map[message_id] = []
        self.threads[thread_id]['last_activity_at'] = datetime.now()
        self.threads[thread_id]['updated_at'] = datetime.now()
        return message_id
    def add_message(self, thread_id: str, content: str, parent_message_id: str=None, attributes: Dict[str, Any]=None) -> str:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return None
        if parent_message_id and parent_message_id not in self.messages:
            logger.error(f'Parent message {parent_message_id} not found')
            return None
        if self.threads[thread_id]['is_locked']:
            logger.error(f'Cannot add message to locked thread {thread_id}')
            return None
        message_id = self._create_message(thread_id, content, parent_message_id, attributes)
        logger.info(f"Added message to thread '{self.threads[thread_id]['title']}'")
        return message_id
    def add_reaction(self, message_id: str, reaction_type: str, attributes: Dict[str, Any]=None) -> str:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return None
        reaction_id = str(uuid.uuid4())
        self.message_reactions[reaction_id] = {'id': reaction_id, 'message_id': message_id, 'user_id': self.user_id, 'reaction_type': reaction_type, 'attributes': attributes or {}, 'created_at': datetime.now()}
        self.message_reactions_map[message_id].append(reaction_id)
        logger.info(f'Added {reaction_type} reaction to message')
        return reaction_id
    def edit_message(self, message_id: str, new_content: str) -> bool:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return False
        message = self.messages[message_id]
        if message['is_deleted']:
            logger.error(f'Cannot edit deleted message {message_id}')
            return False
        message['content'] = new_content
        message['is_edited'] = True
        message['updated_at'] = datetime.now()
        thread_id = message['thread_id']
        self.threads[thread_id]['last_activity_at'] = datetime.now()
        self.threads[thread_id]['updated_at'] = datetime.now()
        logger.info(f'Edited message {message_id}')
        return True
    def delete_message(self, message_id: str) -> bool:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return False
        message = self.messages[message_id]
        message['is_deleted'] = True
        message['updated_at'] = datetime.now()
        thread_id = message['thread_id']
        self.threads[thread_id]['last_activity_at'] = datetime.now()
        self.threads[thread_id]['updated_at'] = datetime.now()
        logger.info(f'Deleted message {message_id}')
        return True
    def pin_thread(self, thread_id: str, pinned: bool=True) -> bool:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return False
        self.threads[thread_id]['is_pinned'] = pinned
        self.threads[thread_id]['updated_at'] = datetime.now()
        action = 'Pinned' if pinned else 'Unpinned'
        logger.info(f"{action} thread '{self.threads[thread_id]['title']}'")
        return True
    def lock_thread(self, thread_id: str, locked: bool=True) -> bool:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return False
        self.threads[thread_id]['is_locked'] = locked
        self.threads[thread_id]['updated_at'] = datetime.now()
        action = 'Locked' if locked else 'Unlocked'
        logger.info(f"{action} thread '{self.threads[thread_id]['title']}'")
        return True
    def mark_as_answer(self, message_id: str, is_answer: bool=True) -> bool:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return False
        message = self.messages[message_id]
        thread_id = message['thread_id']
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return False
        if self.threads[thread_id]['thread_type'] != 'question':
            logger.error(f'Thread {thread_id} is not a question thread')
            return False
        if is_answer:
            for other_message_id in self.thread_messages[thread_id]:
                if other_message_id != message_id:
                    other_message = self.messages[other_message_id]
                    if other_message['is_accepted_answer']:
                        other_message['is_accepted_answer'] = False
                        other_message['updated_at'] = datetime.now()
        message['is_accepted_answer'] = is_answer
        message['updated_at'] = datetime.now()
        action = 'Marked' if is_answer else 'Unmarked'
        logger.info(f'{action} message {message_id} as accepted answer')
        return True
    def increment_view_count(self, thread_id: str) -> bool:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return False
        self.threads[thread_id]['views'] += 1
        return True
    def get_forum(self, forum_id: str) -> Dict[str, Any]:
        if forum_id not in self.forums:
            logger.error(f'Forum {forum_id} not found')
            return None
        return self.forums[forum_id]
    def get_thread(self, thread_id: str) -> Dict[str, Any]:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return None
        return self.threads[thread_id]
    def get_message(self, message_id: str) -> Dict[str, Any]:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return None
        return self.messages[message_id]
    def get_reaction(self, reaction_id: str) -> Dict[str, Any]:
        if reaction_id not in self.message_reactions:
            logger.error(f'Reaction {reaction_id} not found')
            return None
        return self.message_reactions[reaction_id]
    def get_forum_threads(self, forum_id: str, include_pinned_first: bool=True) -> List[Dict[str, Any]]:
        if forum_id not in self.forums:
            logger.error(f'Forum {forum_id} not found')
            return []
        thread_ids = self.forum_threads.get(forum_id, [])
        threads = [self.threads[thread_id] for thread_id in thread_ids if thread_id in self.threads]
        if include_pinned_first:
            return sorted(threads, key=lambda t: (not t['is_pinned'], -t['last_activity_at'].timestamp()))
        else:
            return sorted(threads, key=lambda t: -t['last_activity_at'].timestamp())
    def get_thread_messages(self, thread_id: str, hierarchical: bool=False) -> List[Dict[str, Any]]:
        if thread_id not in self.threads:
            logger.error(f'Thread {thread_id} not found')
            return []
        message_ids = self.thread_messages.get(thread_id, [])
        messages = [self.messages[msg_id] for msg_id in message_ids if msg_id in self.messages]
        if not hierarchical:
            return sorted(messages, key=lambda m: m['created_at'])
        else:
            root_messages = [m for m in messages if m['parent_message_id'] is None]
            root_messages = sorted(root_messages, key=lambda m: m['created_at'])
            def add_children(parent):
                result = dict(parent)
                child_ids = self.message_child_messages.get(parent['id'], [])
                children = [self.messages[child_id] for child_id in child_ids if child_id in self.messages]
                children = sorted(children, key=lambda m: m['created_at'])
                result['children'] = [add_children(child) for child in children]
                return result
            return [add_children(root) for root in root_messages]
    def get_message_reactions(self, message_id: str) -> Dict[str, int]:
        if message_id not in self.messages:
            logger.error(f'Message {message_id} not found')
            return {}
        reaction_ids = self.message_reactions_map.get(message_id, [])
        reactions = [self.message_reactions[r_id] for r_id in reaction_ids if r_id in self.message_reactions]
        counts = {}
        for reaction in reactions:
            r_type = reaction['reaction_type']
            counts[r_type] = counts.get(r_type, 0) + 1
        return counts
    def get_topics_state(self) -> Dict[str, Any]:
        return {'forum_count': len(self.forums), 'thread_count': len(self.threads), 'message_count': len(self.messages), 'reaction_count': len(self.message_reactions), 'active_threads': sum((1 for t in self.threads.values() if (datetime.now() - t['last_activity_at']).days < 30)), 'pinned_threads': sum((1 for t in self.threads.values() if t['is_pinned'])), 'locked_threads': sum((1 for t in self.threads.values() if t['is_locked'])), 'updated_at': datetime.now()}
    def search_threads(self, query: str) -> List[Dict[str, Any]]:
        query = query.lower()
        results = []
        for thread in self.threads.values():
            if query in thread['title'].lower():
                results.append(thread)
                continue
            thread_id = thread['id']
            message_ids = self.thread_messages.get(thread_id, [])
            for msg_id in message_ids:
                if msg_id in self.messages:
                    message = self.messages[msg_id]
                    if not message['is_deleted'] and query in message['content'].lower():
                        results.append(thread)
                        break
        return results
    def find_threads_by_tag(self, tag: str) -> List[Dict[str, Any]]:
        tag = tag.lower()
        return [t for t in self.threads.values() if tag in [t.lower() for t in t['tags']]]
    def find_threads_by_type(self, thread_type: str) -> List[Dict[str, Any]]:
        return [t for t in self.threads.values() if t['thread_type'] == thread_type]
    def get_most_active_threads(self, days: int=7, limit: int=10) -> List[Dict[str, Any]]:
        cutoff_date = datetime.now() - timedelta(days=days)
        recent_threads = [t for t in self.threads.values() if t['last_activity_at'] >= cutoff_date]
        sorted_threads = sorted(recent_threads, key=lambda t: -t['last_activity_at'].timestamp())
        return sorted_threads[:limit]
_topics_instance = UserTopics()
def get_topics() -> UserTopics:
    return _topics_instance