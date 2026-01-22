import os
import sys
import time
import threading
import logging
import signal
import json
from datetime import datetime
import psutil
from pathlib import Path
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', filename='adaptive_heartbeat.log', filemode='a')
logger = logging.getLogger('adaptive_heartbeat')
class AdaptiveHeartbeat:
    _instance = None
    @classmethod
    def get_instance(cls):
        if cls._instance is None:
            cls._instance = AdaptiveHeartbeat()
        return cls._instance
    def __init__(self, min_interval=60.0, max_interval=1.0, hyper_drive_interval=0.1, activity_threshold_low=2, activity_threshold_high=10, cpu_threshold=80.0, activity_logs_dir='activity_logs'):
        if AdaptiveHeartbeat._instance is not None:
            return
        self.min_interval = min_interval
        self.max_interval = max_interval
        self.hyper_drive_interval = hyper_drive_interval
        self.activity_threshold_low = activity_threshold_low
        self.activity_threshold_high = activity_threshold_high
        self.cpu_threshold = cpu_threshold
        self.activity_logs_dir = Path(activity_logs_dir)
        self.current_interval = (min_interval + max_interval) / 2
        self.is_running = False
        self.is_hyper_drive_active = False
        self.hyper_drive_start_time = None
        self.max_hyper_drive_duration = 60
        self.lock = threading.Lock()
        self.heartbeat_thread = None
        self.defense_thread = None
        self.stats = {'heartbeats': 0, 'hyper_drive_activations': 0, 'last_heartbeat': None, 'current_mode': 'NORMAL', 'current_interval': self.current_interval, 'active_events': 0, 'cpu_usage': 0.0, 'memory_usage': 0.0}
        self.event_log = []
        self.active_events = []
        AdaptiveHeartbeat._instance = self
        if not self.activity_logs_dir.exists():
            self.activity_logs_dir.mkdir(parents=True, exist_ok=True)
        self.heartbeat_log_dir = self.activity_logs_dir / 'heartbeat'
        if not self.heartbeat_log_dir.exists():
            self.heartbeat_log_dir.mkdir(parents=True, exist_ok=True)
        self.activity_log_file = self.heartbeat_log_dir / 'activity.json'
        if not self.activity_log_file.exists():
            with open(self.activity_log_file, 'w') as f:
                json.dump([], f)
    def start(self):
        if self.is_running:
            logger.warning('Heartbeat system is already running')
            return
        self.is_running = True
        self.heartbeat_thread = threading.Thread(target=self._heartbeat_loop, daemon=True)
        self.heartbeat_thread.start()
        self.defense_thread = threading.Thread(target=self._defense_monitor, daemon=True)
        self.defense_thread.start()
        logger.info(f'Adaptive heartbeat system started with interval range: {self.max_interval}s to {self.min_interval}s')
        self._log_activity('Heartbeat system started', 'startup')
    def stop(self):
        if not self.is_running:
            return
        self.is_running = False
        if self.heartbeat_thread:
            self.heartbeat_thread.join(timeout=2.0)
        if self.defense_thread:
            self.defense_thread.join(timeout=2.0)
        logger.info('Heartbeat system stopped')
        self._log_activity('Heartbeat system stopped', 'shutdown')
    def _heartbeat_loop(self):
        while self.is_running:
            try:
                current_time = time.time()
                self._heartbeat()
                self._update_system_stats()
                if not self.is_hyper_drive_active:
                    self._adjust_heartbeat_rate()
                if self.is_hyper_drive_active:
                    elapsed = time.time() - self.hyper_drive_start_time
                    if elapsed > self.max_hyper_drive_duration:
                        self._exit_hyper_drive()
                interval = self._get_current_interval()
                sleep_time = max(0.1, interval - (time.time() - current_time))
                time.sleep(sleep_time)
            except Exception as e:
                logger.error(f'Error in heartbeat loop: {e}', exc_info=True)
                time.sleep(1.0)
    def _defense_monitor(self):
        consecutive_high_cpu = 0
        check_interval = 1.0
        cpu_alert_threshold = 90.0
        consecutive_threshold = 3
        while self.is_running:
            try:
                cpu_usage = psutil.cpu_percent(interval=None)
                suspicious_activity = self._check_suspicious_processes()
                if cpu_usage > cpu_alert_threshold or suspicious_activity:
                    consecutive_high_cpu += 1
                else:
                    consecutive_high_cpu = 0
                if consecutive_high_cpu >= consecutive_threshold and (not self.is_hyper_drive_active):
                    trigger_reason = 'High CPU' if cpu_usage > cpu_alert_threshold else 'Suspicious processes'
                    self._enter_hyper_drive(trigger_reason)
                time.sleep(check_interval)
            except Exception as e:
                logger.error(f'Error in defense monitor: {e}', exc_info=True)
                time.sleep(1.0)
    def _heartbeat(self):
        with self.lock:
            self.stats['heartbeats'] += 1
            self.stats['last_heartbeat'] = time.time()
        self._signal_heartbeat()
    def _signal_heartbeat(self):
        heartbeat_file = self.heartbeat_log_dir / 'last_heartbeat'
        try:
            with open(heartbeat_file, 'w') as f:
                f.write(f'{time.time()}')
        except Exception as e:
            logger.error(f'Error writing heartbeat signal: {e}')
    def _update_system_stats(self):
        with self.lock:
            self.stats['cpu_usage'] = psutil.cpu_percent(interval=None)
            self.stats['memory_usage'] = psutil.virtual_memory().percent
            self.stats['current_interval'] = self.current_interval
            self.stats['active_events'] = self._count_active_events()
            try:
                with open(self.heartbeat_log_dir / 'stats.json', 'w') as f:
                    json.dump(self.stats, f)
            except Exception as e:
                logger.error(f'Error writing stats: {e}')
    def _count_active_events(self):
        active_count = 0
        recent_window = 60
        try:
            for component_dir in self.activity_logs_dir.iterdir():
                if component_dir.is_dir():
                    activity_file = component_dir / 'activity.json'
                    if activity_file.exists():
                        with open(activity_file) as f:
                            try:
                                activities = json.load(f)
                                now = time.time()
                                for activity in activities:
                                    if now - activity.get('time', 0) < recent_window:
                                        active_count += 1
                            except json.JSONDecodeError:
                                continue
        except Exception as e:
            logger.error(f'Error counting active events: {e}')
        return active_count
    def _adjust_heartbeat_rate(self):
        active_events = self.stats['active_events']
        cpu_usage = self.stats['cpu_usage']
        with self.lock:
            if active_events <= self.activity_threshold_low:
                target_interval = self.min_interval
            elif active_events >= self.activity_threshold_high:
                target_interval = self.max_interval
            else:
                activity_range = self.activity_threshold_high - self.activity_threshold_low
                position = (active_events - self.activity_threshold_low) / activity_range
                interval_range = self.min_interval - self.max_interval
                target_interval = self.min_interval - position * interval_range
            if cpu_usage > self.cpu_threshold:
                cpu_factor = min(2.0, 1.0 + (cpu_usage - self.cpu_threshold) / 50)
                target_interval *= cpu_factor
            self.current_interval = 0.8 * self.current_interval + 0.2 * target_interval
            self.current_interval = min(self.min_interval, max(self.max_interval, self.current_interval))
    def _get_current_interval(self):
        if self.is_hyper_drive_active:
            return self.hyper_drive_interval
        return self.current_interval
    def _enter_hyper_drive(self, reason='manual'):
        if self.is_hyper_drive_active:
            return
        with self.lock:
            self.is_hyper_drive_active = True
            self.hyper_drive_start_time = time.time()
            self.stats['current_mode'] = 'HYPER DRIVE'
            self.stats['hyper_drive_activations'] += 1
        logger.warning(f'ENTERING HYPER DRIVE MODE: {reason}')
        self._log_activity(f'Hyper Drive mode activated: {reason}', 'defense', priority='high')
        try:
            p = psutil.Process(os.getpid())
            p.nice(psutil.HIGH_PRIORITY_CLASS if hasattr(psutil, 'HIGH_PRIORITY_CLASS') else -10)
        except Exception as e:
            logger.error(f'Failed to set process priority: {e}')
    def _exit_hyper_drive(self):
        if not self.is_hyper_drive_active:
            return
        with self.lock:
            self.is_hyper_drive_active = False
            self.stats['current_mode'] = 'NORMAL'
            elapsed = time.time() - self.hyper_drive_start_time
            self.hyper_drive_start_time = None
        logger.info(f'Exiting hyper drive mode after {elapsed:.1f} seconds')
        self._log_activity('Hyper Drive mode deactivated', 'defense')
        try:
            p = psutil.Process(os.getpid())
            p.nice(psutil.NORMAL_PRIORITY_CLASS if hasattr(psutil, 'NORMAL_PRIORITY_CLASS') else 0)
        except Exception as e:
            logger.error(f'Failed to reset process priority: {e}')
    def _check_suspicious_processes(self):
        suspicious_count = 0
        suspicious_names = ['stress', 'stress-ng', 'fork bomb', 'dd', 'cat /dev/zero']
        cpu_hog_threshold = 95.0
        for proc in psutil.process_iter(['pid', 'name', 'cmdline', 'cpu_percent']):
            try:
                if any((sus in proc.info['name'].lower() for sus in suspicious_names if proc.info['name'])):
                    suspicious_count += 1
                    logger.warning(f"Suspicious process found: {proc.info['name']} (PID: {proc.info['pid']})")
                    continue
                if proc.info['cpu_percent'] > cpu_hog_threshold:
                    cmdline = ' '.join(proc.cmdline()) if hasattr(proc, 'cmdline') else 'Unknown'
                    logger.warning(f"High CPU process: {proc.info['name']} ({cmdline}) CPU: {proc.info['cpu_percent']}% (PID: {proc.info['pid']})")
                    suspicious_count += 1
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
        return suspicious_count > 0
    def _log_activity(self, description, category, priority='normal'):
        activity = {'time': time.time(), 'description': description, 'category': category, 'priority': priority}
        try:
            activities = []
            if self.activity_log_file.exists():
                with open(self.activity_log_file) as f:
                    try:
                        activities = json.load(f)
                    except json.JSONDecodeError:
                        activities = []
            activities.append(activity)
            activities = activities[-1000:]
            with open(self.activity_log_file, 'w') as f:
                json.dump(activities, f)
        except Exception as e:
            logger.error(f'Error logging activity: {e}')
    def manual_trigger_hyper_drive(self, reason='manual trigger'):
        self._enter_hyper_drive(reason)
    def get_current_rate(self):
        interval = self._get_current_interval()
        if interval <= 0:
            return 0
        return 1.0 / interval
    def is_hyper_drive_active(self):
        return self.is_hyper_drive_active
    def get_active_events(self):
        active_events = []
        recent_window = 60
        try:
            for component_dir in self.activity_logs_dir.iterdir():
                if component_dir.is_dir():
                    activity_file = component_dir / 'activity.json'
                    if activity_file.exists():
                        with open(activity_file) as f:
                            try:
                                activities = json.load(f)
                                now = time.time()
                                for activity in activities:
                                    if now - activity.get('time', 0) < recent_window:
                                        active_events.append(activity)
                            except json.JSONDecodeError:
                                continue
        except Exception as e:
            logger.error(f'Error getting active events: {e}')
        return active_events
    def get_system_metrics(self):
        cpu_percent = psutil.cpu_percent(interval=None)
        memory_percent = psutil.virtual_memory().percent
        disk_usage = psutil.disk_usage('/').percent
        return {'cpu_percent': cpu_percent, 'memory_percent': memory_percent, 'disk_usage': disk_usage, 'heartbeats': self.stats['heartbeats'], 'current_interval': self.stats['current_interval']}
    def get_recent_log_entries(self, max_entries=10):
        entries = []
        try:
            if self.activity_log_file.exists():
                with open(self.activity_log_file) as f:
                    try:
                        activities = json.load(f)
                        for activity in activities[-max_entries:]:
                            timestamp = datetime.fromtimestamp(activity.get('time', 0)).strftime('%H:%M:%S')
                            priority = activity.get('priority', 'normal').upper()
                            category = activity.get('category', 'system').upper()
                            description = activity.get('description', '')
                            entry = f'[{timestamp}] [{priority}] [{category}] {description}'
                            entries.append(entry)
                    except json.JSONDecodeError:
                        entries.append('[ERROR] Could not parse activity log')
        except Exception as e:
            logger.error(f'Error getting recent log entries: {e}')
            entries.append(f'[ERROR] {str(e)}')
        return entries
    def activate_hyperdrive(self, reason='Manual activation from GUI'):
        self._enter_hyper_drive(reason)
    def deactivate_hyperdrive(self):
        self._exit_hyper_drive()
def signal_handler(sig, frame):
    if heartbeat_system:
        print('\nStopping heartbeat system...')
        heartbeat_system.stop()
    sys.exit(0)
heartbeat_system = None
def main():
    global heartbeat_system
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    try:
        print('Starting Adaptive Heartbeat System for DeepTreeEcho...')
        heartbeat_system = AdaptiveHeartbeat.get_instance()
        heartbeat_system.start()
        while True:
            if heartbeat_system.stats.get('current_mode') == 'HYPER DRIVE':
                elapsed = time.time() - heartbeat_system.hyper_drive_start_time if heartbeat_system.hyper_drive_start_time else 0
                print(f"\r[{datetime.now().strftime('%H:%M:%S')}] {heartbeat_system.stats['current_mode']} MODE ({elapsed:.1f}s) - CPU: {heartbeat_system.stats['cpu_usage']:.1f}% - Events: {heartbeat_system.stats['active_events']} - Heartbeats: {heartbeat_system.stats['heartbeats']}", end='', flush=True)
            else:
                print(f"\r[{datetime.now().strftime('%H:%M:%S')}] Current Rate: {1.0 / heartbeat_system.stats['current_interval']:.2f} Hz - CPU: {heartbeat_system.stats['cpu_usage']:.1f}% - Events: {heartbeat_system.stats['active_events']} - Heartbeats: {heartbeat_system.stats['heartbeats']}", end='', flush=True)
            time.sleep(1.0)
    except Exception as e:
        logger.error(f'Error in main: {e}', exc_info=True)
        print(f'Error: {e}')
        return 1
    return 0
if __name__ == '__main__':
    sys.exit(main())