import logging
import uuid
from datetime import datetime, timedelta
from typing import Dict, List, Any
from collections import defaultdict
logger = logging.getLogger(__name__)
class WorkspaceScheduling:
    def __init__(self):
        self.schedules = {}
        self.tasks = {}
        self.events = {}
        self.schedule_tasks = defaultdict(list)
        self.task_dependencies = defaultdict(list)
        self.task_dependents = defaultdict(list)
        self.schedule_types = defaultdict(list)
        self.task_types = defaultdict(list)
        self.event_types = defaultdict(list)
        self.timeline = []
        self.active_tasks = []
        self.completed_tasks = []
        self.task_progress = {}
        self.schedule_progress = {}
    def create_schedule(self, name: str, schedule_type: str='generic', start_time: datetime=None, end_time: datetime=None, attributes: Dict[str, Any]=None) -> str:
        schedule_id = str(uuid.uuid4())
        created_at = datetime.now()
        if not start_time:
            start_time = created_at
        if not end_time and schedule_type != 'recurring':
            end_time = created_at + timedelta(days=30)
        self.schedules[schedule_id] = {'id': schedule_id, 'name': name, 'type': schedule_type, 'start_time': start_time, 'end_time': end_time, 'created_at': created_at, 'updated_at': created_at, 'status': 'active'}
        self.schedule_types[schedule_type].append(schedule_id)
        if attributes:
            self.schedules[schedule_id]['attributes'] = attributes
        self.timeline.append((created_at, 'schedule', schedule_id, 'created'))
        logger.info(f"Created schedule '{name}' of type '{schedule_type}' with ID {schedule_id}")
        return schedule_id
    def create_task(self, name: str, schedule_id: str, task_type: str='generic', start_time: datetime=None, duration: int=None, due_time: datetime=None, recurrence: str=None, dependencies: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if schedule_id not in self.schedules:
            logger.error(f'Cannot create task: schedule {schedule_id} not found')
            return None
        task_id = str(uuid.uuid4())
        created_at = datetime.now()
        if not start_time:
            start_time = created_at
        if not due_time and duration:
            due_time = start_time + timedelta(minutes=duration)
        self.tasks[task_id] = {'id': task_id, 'name': name, 'schedule_id': schedule_id, 'type': task_type, 'start_time': start_time, 'duration': duration, 'due_time': due_time, 'recurrence': recurrence, 'created_at': created_at, 'updated_at': created_at, 'status': 'pending'}
        self.schedule_tasks[schedule_id].append(task_id)
        self.task_types[task_type].append(task_id)
        if attributes:
            self.tasks[task_id]['attributes'] = attributes
        if dependencies:
            for dep_name in dependencies:
                for other_task_id, task in self.tasks.items():
                    if task['name'] == dep_name and task['schedule_id'] == schedule_id:
                        self.task_dependencies[task_id].append(other_task_id)
                        self.task_dependents[other_task_id].append(task_id)
                        break
        self.task_progress[task_id] = 0.0
        self.timeline.append((created_at, 'task', task_id, 'created'))
        self._update_schedule_progress(schedule_id)
        logger.info(f"Created task '{name}' in schedule '{self.schedules[schedule_id]['name']}'")
        return task_id
    def create_event(self, name: str, event_time: datetime, event_type: str='generic', duration: int=None, related_task_id: str=None, attributes: Dict[str, Any]=None) -> str:
        event_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.events[event_id] = {'id': event_id, 'name': name, 'type': event_type, 'event_time': event_time, 'duration': duration, 'related_task_id': related_task_id, 'created_at': created_at, 'status': 'pending'}
        self.event_types[event_type].append(event_id)
        if attributes:
            self.events[event_id]['attributes'] = attributes
        self.timeline.append((event_time, 'event', event_id, 'scheduled'))
        logger.info(f"Created event '{name}' of type '{event_type}' at {event_time}")
        return event_id
    def update_task(self, task_id: str, name: str=None, start_time: datetime=None, duration: int=None, due_time: datetime=None, status: str=None, attributes: Dict[str, Any]=None) -> bool:
        if task_id not in self.tasks:
            logger.error(f'Cannot update task: {task_id} not found')
            return False
        task = self.tasks[task_id]
        if name is not None:
            task['name'] = name
        if start_time is not None:
            task['start_time'] = start_time
        if duration is not None:
            task['duration'] = duration
            if task.get('start_time'):
                task['due_time'] = task['start_time'] + timedelta(minutes=duration)
        if due_time is not None:
            task['due_time'] = due_time
        if status is not None:
            old_status = task['status']
            task['status'] = status
            if status == 'active' and old_status != 'active':
                if task_id not in self.active_tasks:
                    self.active_tasks.append(task_id)
                self.timeline.append((datetime.now(), 'task', task_id, 'started'))
            elif status == 'completed' and old_status != 'completed':
                if task_id in self.active_tasks:
                    self.active_tasks.remove(task_id)
                if task_id not in self.completed_tasks:
                    self.completed_tasks.append(task_id)
                self.task_progress[task_id] = 1.0
                self.timeline.append((datetime.now(), 'task', task_id, 'completed'))
                for dependent_id in self.task_dependents.get(task_id, []):
                    self._check_task_dependencies(dependent_id)
                self._update_schedule_progress(task['schedule_id'])
        if attributes is not None:
            task.setdefault('attributes', {}).update(attributes)
        task['updated_at'] = datetime.now()
        logger.info(f"Updated task '{task['name']}' ({task_id})")
        return True
    def update_schedule(self, schedule_id: str, name: str=None, start_time: datetime=None, end_time: datetime=None, status: str=None, attributes: Dict[str, Any]=None) -> bool:
        if schedule_id not in self.schedules:
            logger.error(f'Cannot update schedule: {schedule_id} not found')
            return False
        schedule = self.schedules[schedule_id]
        if name is not None:
            schedule['name'] = name
        if start_time is not None:
            schedule['start_time'] = start_time
        if end_time is not None:
            schedule['end_time'] = end_time
        if status is not None:
            old_status = schedule['status']
            schedule['status'] = status
            if status == 'completed' and old_status != 'completed':
                self.timeline.append((datetime.now(), 'schedule', schedule_id, 'completed'))
                for task_id in self.schedule_tasks.get(schedule_id, []):
                    if self.tasks[task_id]['status'] != 'completed':
                        self.update_task(task_id, status='completed')
        if attributes is not None:
            schedule.setdefault('attributes', {}).update(attributes)
        schedule['updated_at'] = datetime.now()
        logger.info(f"Updated schedule '{schedule['name']}' ({schedule_id})")
        return True
    def set_task_progress(self, task_id: str, progress: float) -> bool:
        if task_id not in self.tasks:
            return False
        progress = max(0.0, min(1.0, progress))
        previous = self.task_progress.get(task_id, 0.0)
        self.task_progress[task_id] = progress
        if progress >= 1.0 and self.tasks[task_id]['status'] != 'completed':
            self.update_task(task_id, status='completed')
        elif progress > 0.0 and self.tasks[task_id]['status'] == 'pending':
            self.update_task(task_id, status='active')
        logger.info(f"Updated task '{self.tasks[task_id]['name']}' progress: {previous:.2f} -> {progress:.2f}")
        schedule_id = self.tasks[task_id]['schedule_id']
        self._update_schedule_progress(schedule_id)
        return True
    def get_task(self, task_id: str) -> Dict[str, Any]:
        if task_id not in self.tasks:
            return None
        task = dict(self.tasks[task_id])
        task['progress'] = self.task_progress.get(task_id, 0.0)
        task['dependencies'] = list(self.task_dependencies.get(task_id, []))
        task['dependents'] = list(self.task_dependents.get(task_id, []))
        return task
    def get_schedule(self, schedule_id: str) -> Dict[str, Any]:
        if schedule_id not in self.schedules:
            return None
        schedule = dict(self.schedules[schedule_id])
        schedule['progress'] = self.schedule_progress.get(schedule_id, 0.0)
        schedule['tasks'] = list(self.schedule_tasks.get(schedule_id, []))
        total_tasks = len(schedule['tasks'])
        completed_tasks = sum((1 for t_id in schedule['tasks'] if self.tasks[t_id]['status'] == 'completed'))
        active_tasks = sum((1 for t_id in schedule['tasks'] if self.tasks[t_id]['status'] == 'active'))
        schedule['task_stats'] = {'total': total_tasks, 'completed': completed_tasks, 'active': active_tasks, 'pending': total_tasks - completed_tasks - active_tasks}
        return schedule
    def get_upcoming_events(self, limit: int=10) -> List[Dict[str, Any]]:
        now = datetime.now()
        upcoming = [event for event in self.events.values() if event['event_time'] >= now and event['status'] != 'cancelled']
        upcoming.sort(key=lambda x: x['event_time'])
        if limit:
            upcoming = upcoming[:limit]
        return upcoming
    def get_active_tasks(self) -> List[Dict[str, Any]]:
        return [self.get_task(task_id) for task_id in self.active_tasks]
    def get_timeline_events(self, start_time: datetime=None, end_time: datetime=None, limit: int=None) -> List[Dict[str, Any]]:
        if not start_time:
            start_time = datetime.now() - timedelta(days=7)
        if not end_time:
            end_time = datetime.now() + timedelta(days=30)
        filtered_events = [entry for entry in self.timeline if start_time <= entry[0] <= end_time]
        filtered_events.sort(key=lambda x: x[0])
        if limit:
            filtered_events = filtered_events[:limit]
        result = []
        for timestamp, entity_type, entity_id, action in filtered_events:
            entity_name = ''
            if entity_type == 'task' and entity_id in self.tasks:
                entity_name = self.tasks[entity_id]['name']
            elif entity_type == 'schedule' and entity_id in self.schedules:
                entity_name = self.schedules[entity_id]['name']
            elif entity_type == 'event' and entity_id in self.events:
                entity_name = self.events[entity_id]['name']
            result.append({'timestamp': timestamp, 'entity_type': entity_type, 'entity_id': entity_id, 'entity_name': entity_name, 'action': action})
        return result
    def get_scheduling_state(self) -> Dict[str, Any]:
        now = datetime.now()
        active_schedules = sum((1 for s in self.schedules.values() if s['status'] == 'active'))
        completed_schedules = sum((1 for s in self.schedules.values() if s['status'] == 'completed'))
        active_task_count = len(self.active_tasks)
        completed_task_count = len(self.completed_tasks)
        total_task_count = len(self.tasks)
        upcoming_events = len([e for e in self.events.values() if e['event_time'] > now and e['status'] != 'cancelled'])
        overdue_tasks = [task_id for task_id, task in self.tasks.items() if task.get('due_time') and task['due_time'] < now and (task['status'] != 'completed')]
        state = {'schedule_count': len(self.schedules), 'active_schedules': active_schedules, 'completed_schedules': completed_schedules, 'task_count': total_task_count, 'active_tasks': active_task_count, 'completed_tasks': completed_task_count, 'event_count': len(self.events), 'upcoming_events': upcoming_events, 'overdue_tasks': len(overdue_tasks), 'timeline_entries': len(self.timeline), 'schedule_types': {type_name: len(schedules) for type_name, schedules in self.schedule_types.items()}, 'task_types': {type_name: len(tasks) for type_name, tasks in self.task_types.items()}}
        return state
    def _check_task_dependencies(self, task_id: str):
        if task_id not in self.tasks:
            return
        if self.tasks[task_id]['status'] in ['active', 'completed']:
            return
        dependencies = self.task_dependencies.get(task_id, [])
        if not dependencies:
            self.update_task(task_id, status='active')
            return
        all_completed = all((self.tasks.get(dep_id, {}).get('status') == 'completed' for dep_id in dependencies))
        if all_completed:
            self.update_task(task_id, status='active')
    def _update_schedule_progress(self, schedule_id: str):
        if schedule_id not in self.schedules:
            return
        tasks = self.schedule_tasks.get(schedule_id, [])
        if not tasks:
            self.schedule_progress[schedule_id] = 0.0
            return
        total_progress = sum((self.task_progress.get(task_id, 0.0) for task_id in tasks))
        average_progress = total_progress / len(tasks)
        self.schedule_progress[schedule_id] = average_progress
        if average_progress >= 1.0 and self.schedules[schedule_id]['status'] != 'completed':
            self.update_schedule(schedule_id, status='completed')
workspace_scheduling = WorkspaceScheduling()
def get_scheduling() -> WorkspaceScheduling:
    return workspace_scheduling