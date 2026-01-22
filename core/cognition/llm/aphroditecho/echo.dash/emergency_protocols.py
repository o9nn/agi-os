import logging
import time
import psutil
import requests
import json
import asyncio
from pathlib import Path
from datetime import datetime
from typing import Dict, Optional
import os
import aiohttp
class EmergencyProtocols:
    def __init__(self, github_token: Optional[str]=None):
        self.logger = logging.getLogger(__name__)
        self.github_token = github_token or os.getenv('GITHUB_TOKEN')
        self.github_repo = os.getenv('GITHUB_REPO', 'dtecho/deep-tree-echo')
        self.emergency_path = Path('activity_logs/emergency')
        self.emergency_path.mkdir(parents=True, exist_ok=True)
        self.activity_file = self.emergency_path / 'activity.json'
        if not self.activity_file.exists():
            with open(self.activity_file, 'w') as f:
                json.dump([], f)
        self.activities = []
        self._load_activities()
        self.thresholds = {'cpu_critical': 95.0, 'memory_critical': 95.0, 'response_timeout': 300, 'error_count_threshold': 10, 'stuck_timeout': 600}
        self.last_activity = time.time()
        self.last_state_change = time.time()
        self.error_timestamps = []
        self.is_distressed = False
        self.emergency_mode = False
        self.status_file = self.emergency_path / 'status.json'
        self._init_status_file()
    def _init_status_file(self):
        if self.status_file.exists():
            with open(self.status_file) as f:
                self.status = json.load(f)
        else:
            self.status = {'last_update': time.time(), 'state': 'initializing', 'health': 100, 'errors': [], 'alerts': [], 'last_distress': None}
            self._save_status()
    def _save_status(self):
        self.status['last_update'] = time.time()
        with open(self.status_file, 'w') as f:
            json.dump(self.status, f, indent=2)
    async def monitor_health(self):
        while True:
            try:
                cpu_percent = psutil.cpu_percent()
                memory_percent = psutil.virtual_memory().percent
                time_since_activity = time.time() - self.last_activity
                time_since_state_change = time.time() - self.last_state_change
                health_score = 100
                health_score -= max(0, (cpu_percent - 80) * 2)
                health_score -= max(0, (memory_percent - 80) * 2)
                health_score -= max(0, (time_since_activity - 60) * 0.1)
                self.status['health'] = max(0, min(100, health_score))
                if cpu_percent > self.thresholds['cpu_critical'] or memory_percent > self.thresholds['memory_critical'] or time_since_activity > self.thresholds['response_timeout'] or (time_since_state_change > self.thresholds['stuck_timeout']):
                    await self.raise_distress(f'Critical condition: CPU={cpu_percent}%, Memory={memory_percent}%, Inactive={time_since_activity}s')
                current_time = time.time()
                self.error_timestamps = [t for t in self.error_timestamps if current_time - t < 60]
                self._save_status()
                await asyncio.sleep(1)
            except Exception as e:
                self.logger.error(f'Error in health monitor: {str(e)}')
                await asyncio.sleep(5)
    async def raise_distress(self, reason: str):
        if not self.is_distressed:
            self.is_distressed = True
            self.status['last_distress'] = {'time': time.time(), 'reason': reason}
            if self.github_token:
                await self._create_github_issue(reason)
            await self.enter_emergency_mode()
    async def _create_github_issue(self, reason: str):
        try:
            url = f'https://api.github.com/repos/{self.github_repo}/issues'
            headers = {'Authorization': f'token {self.github_token}', 'Accept': 'application/vnd.github.v3+json'}
            title = '🚨 DISTRESS SIGNAL: Deep Tree Echo needs attention!'
            body = f"\n## Emergency Alert\n\nDeep Tree Echo has entered emergency mode and requires immediate attention.\n\n### Reason\n{reason}\n\n### System Status\n- Health Score: {self.status['health']}\n- Last Activity: {datetime.fromtimestamp(self.last_activity).isoformat()}\n- State: {self.status['state']}\n\n### Recent Errors\n{chr(10).join(self.status['errors'][-5:])}\n\n### Actions Taken\n- Entered emergency mode\n- Reduced activity to minimal operations\n- Created this distress signal\n- Awaiting human intervention\n\nPlease check the system logs and status at:\n{self.emergency_path}\n"
            async with aiohttp.ClientSession() as session:
                async with session.post(url, headers=headers, json={'title': title, 'body': body}) as resp:
                    if resp.status == 201:
                        self.logger.info('Successfully created GitHub issue')
                    else:
                        self.logger.error(f'Failed to create GitHub issue: {resp.status}')
        except Exception as e:
            self.logger.error(f'Error creating GitHub issue: {str(e)}')
    async def enter_emergency_mode(self):
        self.emergency_mode = True
        self.status['state'] = 'emergency'
        self.thresholds['cpu_critical'] = 70.0
        self.thresholds['memory_critical'] = 70.0
        self.logger.warning('Entering emergency mode')
        self._save_status()
    async def exit_emergency_mode(self):
        if self.status['health'] > 80:
            self.emergency_mode = False
            self.is_distressed = False
            self.status['state'] = 'normal'
            self.thresholds['cpu_critical'] = 95.0
            self.thresholds['memory_critical'] = 95.0
            self.logger.info('Exiting emergency mode')
            self._save_status()
    def log_error(self, error: str):
        current_time = time.time()
        self.error_timestamps.append(current_time)
        self.status['errors'].append(f'{datetime.now().isoformat()}: {error}')
        if len(self.error_timestamps) >= self.thresholds['error_count_threshold']:
            asyncio.create_task(self.raise_distress(f'High error rate: {len(self.error_timestamps)} errors/minute'))
    def update_activity(self):
        print('Logging activity: update_activity')
        self.last_activity = time.time()
    def update_state(self, new_state: str):
        if new_state != self.status['state']:
            self.last_state_change = time.time()
            self.status['state'] = new_state
            self._save_status()
    def create_github_issue(self, title: str, body: str) -> bool:
        if not self.github_token:
            self._log_activity(f'Cannot create GitHub issue - no token: {title}')
            return False
        try:
            url = f'https://api.github.com/repos/{self.github_repo}/issues'
            headers = {'Authorization': f'token {self.github_token}', 'Accept': 'application/vnd.github.v3+json'}
            data = {'title': title, 'body': body, 'labels': ['emergency']}
            response = requests.post(url, headers=headers, json=data)
            if response.status_code == 201:
                self._log_activity(f'Created GitHub issue: {title}')
                return True
            else:
                self._log_activity(f'Failed to create GitHub issue: {response.status_code}')
                return False
        except Exception as e:
            self._log_activity(f'Error creating GitHub issue: {str(e)}')
            return False
    def _log_activity(self, description: str, context: Dict=None):
        try:
            current = []
            if self.activity_file.exists():
                with open(self.activity_file) as f:
                    current = json.load(f)
            activity = {'time': time.time(), 'description': description, 'context': context or {}}
            current.append(activity)
            if len(current) > 1000:
                current = current[-1000:]
            with open(self.activity_file, 'w') as f:
                json.dump(current, f)
            self._update_status(description)
        except Exception as e:
            self.logger.error(f'Error logging emergency activity: {e}')
    def _update_status(self, last_event: str):
        try:
            status = {'last_update': time.time(), 'last_event': last_event, 'is_distressed': self.is_distressed, 'emergency_mode': self.emergency_mode, 'error_count': len([t for t in self.error_timestamps if time.time() - t < 60]), 'system_health': {'cpu': psutil.cpu_percent(), 'memory': psutil.virtual_memory().percent, 'time_since_activity': time.time() - self.last_activity}}
            with open(self.status_file, 'w') as f:
                json.dump(status, f, indent=2)
        except Exception as e:
            self.logger.error(f'Error updating emergency status: {e}')
    def handle_error(self, error: str, context: Dict=None):
        self.error_timestamps.append(time.time())
        self._log_activity(f'Error detected: {error}', context)
        recent_errors = len([t for t in self.error_timestamps if time.time() - t < 60])
        if recent_errors >= self.thresholds['error_count_threshold']:
            self.is_distressed = True
            title = f'System Distress: High Error Rate ({recent_errors} errors/min)'
            body = f"\n## System Distress Report\n- Error Rate: {recent_errors} errors/min\n- Latest Error: {error}\n- Context: {(json.dumps(context, indent=2) if context else 'None')}\n- System Health:\n  - CPU: {psutil.cpu_percent()}%\n  - Memory: {psutil.virtual_memory().percent}%\n  - Time Since Activity: {time.time() - self.last_activity:.1f}s\n"
            self.create_github_issue(title, body)
    def monitor_system_health(self):
        try:
            cpu = psutil.cpu_percent()
            memory = psutil.virtual_memory().percent
            time_inactive = time.time() - self.last_activity
            if cpu > self.thresholds['cpu_critical']:
                self._log_activity(f'Critical CPU usage: {cpu}%')
            if memory > self.thresholds['memory_critical']:
                self._log_activity(f'Critical memory usage: {memory}%')
            if time_inactive > self.thresholds['stuck_timeout']:
                self._log_activity(f'System appears stuck: {time_inactive:.1f}s inactive')
        except Exception as e:
            self.logger.error(f'Error monitoring system health: {e}')
    def signal_distress(self, reason: str):
        self.is_distressed = True
        self._log_activity(f'Distress signal: {reason}')
        title = f'System Distress: {reason}'
        body = f'\n## Distress Signal\n- Reason: {reason}\n- Time: {datetime.now().isoformat()}\n- System Health:\n  - CPU: {psutil.cpu_percent()}%\n  - Memory: {psutil.virtual_memory().percent}%\n  - Time Since Activity: {time.time() - self.last_activity:.1f}s\n'
        if self.github_token:
            self.create_github_issue(title, body)
        else:
            self._log_activity('GitHub integration disabled - no token available')