import logging
import uuid
from datetime import datetime, timedelta
from typing import Dict, List, Any
logger = logging.getLogger(__name__)
class UserTimelines:
    def __init__(self):
        self.user_id = 'default_user'
        self.timelines = {}
        self.phases = {}
        self.events = {}
        self.reminders = {}
        self.timeline_phases = {}
        self.timeline_events = {}
        self.phase_events = {}
        self.event_reminders = {}
        logger.info('User timelines module initialized')
    def create_timeline(self, name: str, timeline_type: str='personal', description: str=None, tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        timeline_id = str(uuid.uuid4())
        self.timelines[timeline_id] = {'id': timeline_id, 'name': name, 'timeline_type': timeline_type, 'user_id': self.user_id, 'description': description or f'Timeline: {name}', 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now(), 'start_date': None, 'end_date': None}
        self.timeline_phases[timeline_id] = []
        self.timeline_events[timeline_id] = []
        logger.info(f"Created timeline '{name}' with ID {timeline_id}")
        return timeline_id
    def create_phase(self, timeline_id: str, name: str, start_date: datetime=None, end_date: datetime=None, description: str=None, phase_type: str='standard', tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return None
        phase_id = str(uuid.uuid4())
        self.phases[phase_id] = {'id': phase_id, 'name': name, 'timeline_id': timeline_id, 'user_id': self.user_id, 'start_date': start_date, 'end_date': end_date, 'phase_type': phase_type, 'description': description or f'Phase: {name}', 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.timeline_phases[timeline_id].append(phase_id)
        self.phase_events[phase_id] = []
        logger.info(f"Created phase '{name}' in timeline '{self.timelines[timeline_id]['name']}'")
        return phase_id
    def add_event(self, timeline_id: str, title: str, timestamp: datetime=None, duration: timedelta=None, event_type: str='standard', description: str=None, phase_id: str=None, tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return None
        if phase_id and phase_id not in self.phases:
            logger.error(f'Phase {phase_id} not found')
            return None
        event_id = str(uuid.uuid4())
        self.events[event_id] = {'id': event_id, 'title': title, 'timeline_id': timeline_id, 'phase_id': phase_id, 'user_id': self.user_id, 'timestamp': timestamp or datetime.now(), 'duration': duration, 'event_type': event_type, 'description': description or f'Event: {title}', 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.timeline_events[timeline_id].append(event_id)
        if phase_id:
            self.phase_events[phase_id].append(event_id)
        self.event_reminders[event_id] = []
        logger.info(f"Added event '{title}' to timeline '{self.timelines[timeline_id]['name']}'")
        return event_id
    def add_reminder(self, event_id: str, remind_at: datetime, description: str=None, reminder_type: str='notification', attributes: Dict[str, Any]=None) -> str:
        if event_id not in self.events:
            logger.error(f'Event {event_id} not found')
            return None
        reminder_id = str(uuid.uuid4())
        self.reminders[reminder_id] = {'id': reminder_id, 'event_id': event_id, 'user_id': self.user_id, 'remind_at': remind_at, 'reminder_type': reminder_type, 'description': description or f"Reminder for event: {self.events[event_id]['title']}", 'is_triggered': False, 'is_dismissed': False, 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.event_reminders[event_id].append(reminder_id)
        logger.info(f"Added reminder for event '{self.events[event_id]['title']}'")
        return reminder_id
    def update_phase_dates(self, phase_id: str, start_date: datetime=None, end_date: datetime=None) -> bool:
        if phase_id not in self.phases:
            logger.error(f'Phase {phase_id} not found')
            return False
        phase = self.phases[phase_id]
        if start_date:
            phase['start_date'] = start_date
        if end_date:
            phase['end_date'] = end_date
        phase['updated_at'] = datetime.now()
        logger.info(f"Updated dates for phase '{phase['name']}'")
        return True
    def update_timeline_dates(self, timeline_id: str, start_date: datetime=None, end_date: datetime=None) -> bool:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return False
        timeline = self.timelines[timeline_id]
        if start_date:
            timeline['start_date'] = start_date
        if end_date:
            timeline['end_date'] = end_date
        timeline['updated_at'] = datetime.now()
        logger.info(f"Updated dates for timeline '{timeline['name']}'")
        return True
    def trigger_reminder(self, reminder_id: str) -> bool:
        if reminder_id not in self.reminders:
            logger.error(f'Reminder {reminder_id} not found')
            return False
        self.reminders[reminder_id]['is_triggered'] = True
        self.reminders[reminder_id]['updated_at'] = datetime.now()
        logger.info(f"Triggered reminder '{self.reminders[reminder_id]['description']}'")
        return True
    def dismiss_reminder(self, reminder_id: str) -> bool:
        if reminder_id not in self.reminders:
            logger.error(f'Reminder {reminder_id} not found')
            return False
        self.reminders[reminder_id]['is_dismissed'] = True
        self.reminders[reminder_id]['updated_at'] = datetime.now()
        logger.info(f"Dismissed reminder '{self.reminders[reminder_id]['description']}'")
        return True
    def get_timeline(self, timeline_id: str) -> Dict[str, Any]:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return None
        return self.timelines[timeline_id]
    def get_phase(self, phase_id: str) -> Dict[str, Any]:
        if phase_id not in self.phases:
            logger.error(f'Phase {phase_id} not found')
            return None
        return self.phases[phase_id]
    def get_event(self, event_id: str) -> Dict[str, Any]:
        if event_id not in self.events:
            logger.error(f'Event {event_id} not found')
            return None
        return self.events[event_id]
    def get_reminder(self, reminder_id: str) -> Dict[str, Any]:
        if reminder_id not in self.reminders:
            logger.error(f'Reminder {reminder_id} not found')
            return None
        return self.reminders[reminder_id]
    def get_timeline_phases(self, timeline_id: str) -> List[Dict[str, Any]]:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return []
        phase_ids = self.timeline_phases.get(timeline_id, [])
        return [self.phases[phase_id] for phase_id in phase_ids if phase_id in self.phases]
    def get_timeline_events(self, timeline_id: str) -> List[Dict[str, Any]]:
        if timeline_id not in self.timelines:
            logger.error(f'Timeline {timeline_id} not found')
            return []
        event_ids = self.timeline_events.get(timeline_id, [])
        return [self.events[event_id] for event_id in event_ids if event_id in self.events]
    def get_phase_events(self, phase_id: str) -> List[Dict[str, Any]]:
        if phase_id not in self.phases:
            logger.error(f'Phase {phase_id} not found')
            return []
        event_ids = self.phase_events.get(phase_id, [])
        return [self.events[event_id] for event_id in event_ids if event_id in self.events]
    def get_event_reminders(self, event_id: str) -> List[Dict[str, Any]]:
        if event_id not in self.events:
            logger.error(f'Event {event_id} not found')
            return []
        reminder_ids = self.event_reminders.get(event_id, [])
        return [self.reminders[reminder_id] for reminder_id in reminder_ids if reminder_id in self.reminders]
    def get_all_active_reminders(self) -> List[Dict[str, Any]]:
        return [r for r in self.reminders.values() if r['is_triggered'] and (not r['is_dismissed'])]
    def get_all_pending_reminders(self) -> List[Dict[str, Any]]:
        return [r for r in self.reminders.values() if not r['is_triggered']]
    def get_events_in_timerange(self, start_time: datetime, end_time: datetime) -> List[Dict[str, Any]]:
        return [e for e in self.events.values() if e['timestamp'] and e['timestamp'] >= start_time and (e['timestamp'] <= end_time)]
    def get_upcoming_events(self, days: int=7) -> List[Dict[str, Any]]:
        now = datetime.now()
        end_time = now + timedelta(days=days)
        return self.get_events_in_timerange(now, end_time)
    def get_timelines_state(self) -> Dict[str, Any]:
        return {'timeline_count': len(self.timelines), 'phase_count': len(self.phases), 'event_count': len(self.events), 'reminder_count': len(self.reminders), 'active_reminders': len(self.get_all_active_reminders()), 'pending_reminders': len(self.get_all_pending_reminders()), 'upcoming_events': len(self.get_upcoming_events()), 'updated_at': datetime.now()}
    def search_events(self, query: str) -> List[Dict[str, Any]]:
        query = query.lower()
        results = []
        for event in self.events.values():
            if query in event['title'].lower() or (event['description'] and query in event['description'].lower()):
                results.append(event)
        return results
    def find_events_by_tag(self, tag: str) -> List[Dict[str, Any]]:
        tag = tag.lower()
        return [e for e in self.events.values() if tag in [t.lower() for t in e['tags']]]
    def find_events_by_type(self, event_type: str) -> List[Dict[str, Any]]:
        return [e for e in self.events.values() if e['event_type'] == event_type]
    def find_timelines_by_type(self, timeline_type: str) -> List[Dict[str, Any]]:
        return [t for t in self.timelines.values() if t['timeline_type'] == timeline_type]
_timelines_instance = UserTimelines()
def get_timelines() -> UserTimelines:
    return _timelines_instance