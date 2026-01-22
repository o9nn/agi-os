import json
import logging
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional, Any, Union
from enum import Enum
logger = logging.getLogger(__name__)
class CommunicativeIntent(Enum):
    REQUEST = 'request'
    INFORM = 'inform'
    COORDINATE = 'coordinate'
    DELEGATE = 'delegate'
    QUERY = 'query'
    CONFIRM = 'confirm'
    REJECT = 'reject'
    NEGOTIATE = 'negotiate'
class CognitiveRole(Enum):
    AGENT = 'agent'
    PATIENT = 'patient'
    EXPERIENCER = 'experiencer'
    INSTRUMENT = 'instrument'
    LOCATION = 'location'
    TIME = 'time'
    MANNER = 'manner'
    PURPOSE = 'purpose'
class CognitiveFrame(Enum):
    TASK_DELEGATION = 'task_delegation'
    INFORMATION_SHARING = 'information_sharing'
    COORDINATION = 'coordination'
    CAPABILITY_NEGOTIATION = 'capability_negotiation'
    RESOURCE_ALLOCATION = 'resource_allocation'
    ERROR_HANDLING = 'error_handling'
    STATUS_REPORTING = 'status_reporting'
@dataclass
class CognitiveMessage:
    intent: CommunicativeIntent
    frame: CognitiveFrame
    roles: Dict[CognitiveRole, Any]
    content: Dict[str, Any]
    agent_id: str
    timestamp: Optional[float] = None
    message_id: Optional[str] = None
    response_to: Optional[str] = None
    def to_natural_language(self) -> str:
        return CognitiveGrammarFramework.generate_natural_language(self)
    def to_json(self) -> str:
        try:
            data = asdict(self)
            data['intent'] = self.intent.value
            data['frame'] = self.frame.value
            data['roles'] = {role.value: value for role, value in self.roles.items()}
            return json.dumps(data, indent=2)
        except Exception as e:
            logger.error(f'Error serializing cognitive message: {e}')
            raise
    @classmethod
    def from_json(cls, json_str: str) -> 'CognitiveMessage':
        try:
            data = json.loads(json_str)
            intent = CommunicativeIntent(data['intent'])
            frame = CognitiveFrame(data['frame'])
            roles = {CognitiveRole(role): value for role, value in data['roles'].items()}
            return cls(intent=intent, frame=frame, roles=roles, content=data['content'], agent_id=data['agent_id'], timestamp=data.get('timestamp'), message_id=data.get('message_id'), response_to=data.get('response_to'))
        except Exception as e:
            logger.error(f'Error deserializing cognitive message: {e}')
            raise
class CognitiveGrammarFramework:
    @staticmethod
    def create_message(intent: CommunicativeIntent, frame: CognitiveFrame, agent_id: str, content: Dict[str, Any], roles: Optional[Dict[CognitiveRole, Any]]=None) -> CognitiveMessage:
        import time
        import uuid
        if roles is None:
            roles = {}
        return CognitiveMessage(intent=intent, frame=frame, roles=roles, content=content, agent_id=agent_id, timestamp=time.time(), message_id=str(uuid.uuid4()))
    @staticmethod
    def generate_natural_language(message: CognitiveMessage) -> str:
        try:
            templates = {CommunicativeIntent.REQUEST: {CognitiveFrame.TASK_DELEGATION: 'Agent {agent} requests {patient} to {action}', CognitiveFrame.INFORMATION_SHARING: 'Agent {agent} requests information about {topic}', CognitiveFrame.COORDINATION: 'Agent {agent} requests coordination on {task}', CognitiveFrame.CAPABILITY_NEGOTIATION: 'Agent {agent} requests capability {capability}', CognitiveFrame.RESOURCE_ALLOCATION: 'Agent {agent} requests resource {resource}'}, CommunicativeIntent.INFORM: {CognitiveFrame.INFORMATION_SHARING: 'Agent {agent} informs about {topic}: {information}', CognitiveFrame.STATUS_REPORTING: 'Agent {agent} reports status: {status}', CognitiveFrame.ERROR_HANDLING: 'Agent {agent} reports error: {error}'}, CommunicativeIntent.COORDINATE: {CognitiveFrame.COORDINATION: 'Agent {agent} coordinates {task} with {participants}', CognitiveFrame.TASK_DELEGATION: 'Agent {agent} coordinates task delegation for {task}'}, CommunicativeIntent.DELEGATE: {CognitiveFrame.TASK_DELEGATION: 'Agent {agent} delegates {task} to {patient}'}, CommunicativeIntent.QUERY: {CognitiveFrame.INFORMATION_SHARING: 'Agent {agent} queries {target} about {topic}', CognitiveFrame.CAPABILITY_NEGOTIATION: 'Agent {agent} queries available capabilities'}, CommunicativeIntent.CONFIRM: {CognitiveFrame.COORDINATION: 'Agent {agent} confirms {action}', CognitiveFrame.TASK_DELEGATION: 'Agent {agent} confirms task completion: {task}'}, CommunicativeIntent.REJECT: {CognitiveFrame.TASK_DELEGATION: 'Agent {agent} rejects task: {task}', CognitiveFrame.CAPABILITY_NEGOTIATION: 'Agent {agent} rejects capability request'}, CommunicativeIntent.NEGOTIATE: {CognitiveFrame.CAPABILITY_NEGOTIATION: 'Agent {agent} negotiates capabilities', CognitiveFrame.RESOURCE_ALLOCATION: 'Agent {agent} negotiates resource allocation'}}
            template = templates.get(message.intent, {}).get(message.frame)
            if not template:
                return f'Agent {message.agent_id} {message.intent.value}s regarding {message.frame.value}'
            format_values = {'agent': message.agent_id}
            for role, value in message.roles.items():
                format_values[role.value] = value
            for key, value in message.content.items():
                format_values[key] = value
            available_keys = set(format_values.keys())
            template_keys = set()
            import re
            for match in re.finditer('\\{(\\w+)\\}', template):
                template_keys.add(match.group(1))
            safe_format_values = {k: v for k, v in format_values.items() if k in template_keys}
            for key in template_keys:
                if key not in safe_format_values:
                    if key in format_values:
                        safe_format_values[key] = format_values[key]
                    else:
                        safe_format_values[key] = f'<{key}>'
            return template.format(**safe_format_values)
        except Exception as e:
            logger.error(f'Error generating natural language: {e}')
            return f'Agent {message.agent_id} communicates about {message.frame.value}'
    @staticmethod
    def validate_message(message: CognitiveMessage) -> bool:
        try:
            if not message.agent_id:
                return False
            if not isinstance(message.intent, CommunicativeIntent):
                return False
            if not isinstance(message.frame, CognitiveFrame):
                return False
            if not isinstance(message.content, dict):
                return False
            if not isinstance(message.roles, dict):
                return False
            for role in message.roles.keys():
                if not isinstance(role, CognitiveRole):
                    return False
            return True
        except Exception as e:
            logger.error(f'Error validating message: {e}')
            return False
    @staticmethod
    def create_task_delegation_message(agent_id: str, task_description: str, assignee: str, deadline: Optional[str]=None, priority: Optional[str]=None) -> CognitiveMessage:
        roles = {CognitiveRole.AGENT: agent_id, CognitiveRole.PATIENT: assignee}
        content = {'task': task_description, 'action': 'complete task'}
        if deadline:
            content['deadline'] = deadline
            roles[CognitiveRole.TIME] = deadline
        if priority:
            content['priority'] = priority
        return CognitiveGrammarFramework.create_message(intent=CommunicativeIntent.DELEGATE, frame=CognitiveFrame.TASK_DELEGATION, agent_id=agent_id, content=content, roles=roles)
    @staticmethod
    def create_information_sharing_message(agent_id: str, topic: str, information: str, recipient: Optional[str]=None) -> CognitiveMessage:
        roles = {CognitiveRole.AGENT: agent_id}
        if recipient:
            roles[CognitiveRole.PATIENT] = recipient
        content = {'topic': topic, 'information': information}
        return CognitiveGrammarFramework.create_message(intent=CommunicativeIntent.INFORM, frame=CognitiveFrame.INFORMATION_SHARING, agent_id=agent_id, content=content, roles=roles)
    @staticmethod
    def create_coordination_message(agent_id: str, task: str, participants: List[str], coordination_type: str='synchronous') -> CognitiveMessage:
        roles = {CognitiveRole.AGENT: agent_id, CognitiveRole.PATIENT: participants}
        content = {'task': task, 'participants': participants, 'coordination_type': coordination_type}
        return CognitiveGrammarFramework.create_message(intent=CommunicativeIntent.COORDINATE, frame=CognitiveFrame.COORDINATION, agent_id=agent_id, content=content, roles=roles)
    @staticmethod
    def create_capability_query_message(agent_id: str, required_capabilities: List[str], target_agent: Optional[str]=None) -> CognitiveMessage:
        roles = {CognitiveRole.AGENT: agent_id}
        if target_agent:
            roles[CognitiveRole.PATIENT] = target_agent
        content = {'required_capabilities': required_capabilities, 'topic': 'capability inquiry'}
        return CognitiveGrammarFramework.create_message(intent=CommunicativeIntent.QUERY, frame=CognitiveFrame.CAPABILITY_NEGOTIATION, agent_id=agent_id, content=content, roles=roles)
    @staticmethod
    def parse_natural_language(text: str, agent_id: str) -> Optional[CognitiveMessage]:
        try:
            text = text.lower().strip()
            if 'request' in text or 'please' in text:
                intent = CommunicativeIntent.REQUEST
            elif 'inform' in text or 'tell' in text or 'report' in text:
                intent = CommunicativeIntent.INFORM
            elif 'coordinate' in text:
                intent = CommunicativeIntent.COORDINATE
            elif 'delegate' in text or 'assign' in text:
                intent = CommunicativeIntent.DELEGATE
            elif 'query' in text or 'what' in text or '?' in text:
                intent = CommunicativeIntent.QUERY
            elif 'confirm' in text or 'yes' in text:
                intent = CommunicativeIntent.CONFIRM
            elif 'reject' in text or 'no' in text or 'refuse' in text:
                intent = CommunicativeIntent.REJECT
            elif 'negotiate' in text:
                intent = CommunicativeIntent.NEGOTIATE
            else:
                intent = CommunicativeIntent.INFORM
            if 'task' in text:
                frame = CognitiveFrame.TASK_DELEGATION
            elif 'coordinate' in text:
                frame = CognitiveFrame.COORDINATION
            elif 'capability' in text:
                frame = CognitiveFrame.CAPABILITY_NEGOTIATION
            elif 'resource' in text:
                frame = CognitiveFrame.RESOURCE_ALLOCATION
            elif 'error' in text:
                frame = CognitiveFrame.ERROR_HANDLING
            elif 'status' in text:
                frame = CognitiveFrame.STATUS_REPORTING
            else:
                frame = CognitiveFrame.INFORMATION_SHARING
            content = {'original_text': text, 'parsed': True}
            return CognitiveGrammarFramework.create_message(intent=intent, frame=frame, agent_id=agent_id, content=content)
        except Exception as e:
            logger.error(f'Error parsing natural language: {e}')
            return None
def example_usage():
    print('=== Cognitive Grammar Framework Example ===\n')
    task_msg = CognitiveGrammarFramework.create_task_delegation_message(agent_id='agent_001', task_description='Implement distributed microservices architecture', assignee='agent_002', deadline='2024-01-15', priority='high')
    print('Task Delegation Message:')
    print(f'Natural Language: {task_msg.to_natural_language()}')
    print(f'JSON: {task_msg.to_json()}\n')
    info_msg = CognitiveGrammarFramework.create_information_sharing_message(agent_id='agent_002', topic='system status', information='All services are operational', recipient='agent_001')
    print('Information Sharing Message:')
    print(f'Natural Language: {info_msg.to_natural_language()}')
    print(f'JSON: {info_msg.to_json()}\n')
    coord_msg = CognitiveGrammarFramework.create_coordination_message(agent_id='agent_001', task='database migration', participants=['agent_002', 'agent_003'], coordination_type='synchronous')
    print('Coordination Message:')
    print(f'Natural Language: {coord_msg.to_natural_language()}')
    print(f'JSON: {coord_msg.to_json()}\n')
    json_str = task_msg.to_json()
    restored_msg = CognitiveMessage.from_json(json_str)
    print('JSON Serialization Test:')
    print(f'Original valid: {CognitiveGrammarFramework.validate_message(task_msg)}')
    print(f'Restored valid: {CognitiveGrammarFramework.validate_message(restored_msg)}')
    print(f'Messages equal: {task_msg.content == restored_msg.content}')
if __name__ == '__main__':
    example_usage()