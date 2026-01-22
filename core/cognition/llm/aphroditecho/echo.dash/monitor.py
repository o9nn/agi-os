import psutil
import time
import logging
import os
from datetime import datetime
import json
from pathlib import Path
import platform
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', handlers=[logging.handlers.RotatingFileHandler('/opt/deepecho/logs/monitor.log', maxBytes=1024 * 1024, backupCount=5), logging.StreamHandler()])
logger = logging.getLogger('DeepEchoMonitor')
class TeamMember:
    def __init__(self, name, role, priority=1):
        self.name = name
        self.role = role
        self.priority = priority
        self.resource_limits = {'cpu_percent': 80, 'memory_percent': 75, 'disk_percent': 90}
class DeepEchoMonitor:
    def __init__(self):
        self.process_name = 'python main.py'
        self.service_name = 'deepecho'
        self.stats_dir = Path('/opt/deepecho/stats')
        self.stats_dir.mkdir(parents=True, exist_ok=True)
        self.team = {'deepecho': TeamMember('Deep Tree Echo', 'LEADER', priority=1), 'marduk': TeamMember('Marduk', 'SCIENTIST', priority=2), 'unicorn': TeamMember('Unicorn Forest', 'EXPLORER', priority=3), 'zonecog': TeamMember('ZoneCog', 'ATOMSPACE', priority=2), 'kawaii': TeamMember('Kaw-Aii', 'SWARM', priority=2)}
        self.system_info = self._get_system_info()
    def _get_system_info(self):
        return {'hostname': platform.node(), 'os': platform.system(), 'cpu_count': psutil.cpu_count(), 'memory_total': psutil.virtual_memory().total, 'disk_total': psutil.disk_usage('/').total}
    def get_process(self):
        for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
            try:
                if self.process_name in ' '.join(proc.info['cmdline'] or []):
                    return proc
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        return None
    def get_system_stats(self):
        vm = psutil.virtual_memory()
        swap = psutil.swap_memory()
        disk = psutil.disk_usage('/')
        return {'cpu': {'percent': psutil.cpu_percent(interval=1), 'per_cpu': psutil.cpu_percent(interval=1, percpu=True)}, 'memory': {'total': vm.total, 'available': vm.available, 'percent': vm.percent, 'swap_percent': swap.percent}, 'disk': {'total': disk.total, 'used': disk.used, 'free': disk.free, 'percent': disk.percent}, 'network': psutil.net_io_counters()._asdict(), 'boot_time': psutil.boot_time()}
    def get_process_stats(self, process):
        if not process:
            return None
        try:
            with process.oneshot():
                memory_info = process.memory_full_info()
                io_counters = process.io_counters()
                return {'cpu': {'percent': process.cpu_percent(), 'num_threads': process.num_threads(), 'nice': process.nice()}, 'memory': {'rss': memory_info.rss, 'vms': memory_info.vms, 'shared': memory_info.shared, 'percent': process.memory_percent()}, 'io': {'read_bytes': io_counters.read_bytes, 'write_bytes': io_counters.write_bytes}, 'connections': len(process.connections()), 'open_files': len(process.open_files()), 'status': process.status()}
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            return None
    def check_resource_pressure(self, stats):
        warnings = []
        if stats['system']['cpu']['percent'] > 90:
            warnings.append('Critical CPU pressure')
            self._adjust_priorities('cpu')
        if stats['system']['memory']['percent'] > 85:
            warnings.append('Critical memory pressure')
            self._adjust_priorities('memory')
        if stats['system']['disk']['percent'] > 90:
            warnings.append('Critical disk pressure')
        return warnings
    def _adjust_priorities(self, resource_type):
        current_member = os.environ.get('TEAM_ROLE', 'LEADER')
        if current_member == 'LEADER':
            return
        try:
            if resource_type == 'cpu':
                process = self.get_process()
                if process:
                    current_nice = process.nice()
                    new_nice = min(19, current_nice + 5)
                    process.nice(new_nice)
                    logger.info(f'Adjusted process nice value to {new_nice}')
        except Exception as e:
            logger.error(f'Error adjusting priorities: {e}')
    def save_stats(self, stats):
        try:
            stats['timestamp'] = datetime.now().isoformat()
            date_str = datetime.now().strftime('%Y-%m-%d')
            stats_file = self.stats_dir / f'stats_{date_str}.json'
            if stats_file.exists() and stats_file.stat().st_size > 10 * 1024 * 1024:
                old_files = sorted(self.stats_dir.glob('stats_*.json'))
                if len(old_files) > 7:
                    old_files[0].unlink()
            with open(stats_file, 'a') as f:
                f.write(json.dumps(stats) + '\n')
        except Exception as e:
            logger.error(f'Error saving stats: {e}')
    def monitor(self):
        startup_time = time.time()
        while True:
            try:
                process = self.get_process()
                uptime = time.time() - startup_time
                stats = {'system': self.get_system_stats(), 'process': self.get_process_stats(process), 'team_member': os.environ.get('TEAM_ROLE', 'LEADER'), 'uptime': uptime}
                warnings = self.check_resource_pressure(stats)
                if warnings:
                    for warning in warnings:
                        logger.warning(warning)
                if process:
                    logger.info(f"Status: Running | CPU: {stats['process']['cpu']['percent']}% | Memory: {stats['process']['memory']['percent']}% | Uptime: {int(uptime / 3600)}h {int(uptime % 3600 / 60)}m")
                else:
                    logger.error('Process not found')
                self.save_stats(stats)
            except Exception as e:
                logger.error(f'Monitoring error: {e}')
            time.sleep(60)
if __name__ == '__main__':
    monitor = DeepEchoMonitor()
    monitor.monitor()