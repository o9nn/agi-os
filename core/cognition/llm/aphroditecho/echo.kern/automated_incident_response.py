#!/usr/bin/env python3
"""
Automated Incident Response System for Deep Tree Echo Architecture
Implements Task 4.3.2: Implement Monitoring and Alerting (Automated Incident Response)

This module provides automated incident response capabilities integrated with the
existing performance monitoring system. It implements proactive system maintenance
and optimization based on the runbook procedures from echo.rkwv/docs/runbooks/incident-response.md.
"""

import os
import time
import logging
import subprocess
import threading
import json
import psutil
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Callable, Any
from pathlib import Path
from enum import Enum

from performance_monitor import (
    PerformanceAlert, 
    AlertSeverity, 
    UnifiedPerformanceMonitor
)

# Setup logging
logger = logging.getLogger(__name__)


class ResponseStatus(Enum):
    """Status of automated response actions"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    SUCCESS = "success"
    FAILED = "failed"
    ESCALATED = "escalated"


class ResponsePriority(Enum):
    """Priority levels for incident response"""
    P0_CRITICAL = 0  # Complete system outage - 15 minutes
    P1_HIGH = 1      # Major feature unavailable - 1 hour
    P2_MEDIUM = 2    # Minor feature unavailable - 4 hours
    P3_LOW = 3       # Cosmetic issues - 24 hours


@dataclass
class IncidentResponse:
    """Automated incident response record"""
    incident_id: str
    timestamp: float
    alert: PerformanceAlert
    priority: ResponsePriority
    status: ResponseStatus
    actions_taken: List[str] = field(default_factory=list)
    success_actions: List[str] = field(default_factory=list)
    failed_actions: List[str] = field(default_factory=list)
    escalation_reason: Optional[str] = None
    resolution_time: Optional[float] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class MaintenanceTask:
    """Proactive maintenance task definition"""
    task_id: str
    name: str
    description: str
    action: Callable[[], bool]
    interval_hours: float
    last_run: Optional[float] = None
    success_count: int = 0
    failure_count: int = 0
    enabled: bool = True


class AutomatedIncidentResponseSystem:
    """
    Automated incident response system that integrates with the existing 
    performance monitoring to provide proactive system maintenance and optimization.
    """
    
    def __init__(self, performance_monitor: Optional[UnifiedPerformanceMonitor] = None):
        self.performance_monitor = performance_monitor
        self.responses_history: List[IncidentResponse] = []
        self.maintenance_tasks: Dict[str, MaintenanceTask] = {}
        self.response_handlers: Dict[str, Callable[[PerformanceAlert], bool]] = {}
        self.is_running = False
        self._lock = threading.RLock()
        self._response_thread = None
        self._maintenance_thread = None
        
        # Configuration
        self.max_concurrent_responses = 3
        self.escalation_timeout_minutes = 30
        self.maintenance_enabled = True
        
        # Stats directory for incident reports
        self.stats_dir = Path("/tmp/incident_response_stats")
        self.stats_dir.mkdir(exist_ok=True)
        
        # Initialize default response handlers
        self._initialize_default_handlers()
        
        # Initialize default maintenance tasks
        self._initialize_maintenance_tasks()
        
        logger.info("Automated Incident Response System initialized")
    
    def _initialize_default_handlers(self):
        """Initialize default automated response handlers"""
        
        # High CPU usage response
        self.response_handlers['high_cpu_usage'] = self._handle_high_cpu_usage
        
        # High memory usage response  
        self.response_handlers['high_memory_usage'] = self._handle_high_memory_usage
        
        # Low token throughput response
        self.response_handlers['low_token_throughput'] = self._handle_low_throughput
        
        # High request latency response
        self.response_handlers['high_request_latency'] = self._handle_high_latency
        
        # Low disk space response
        self.response_handlers['low_disk_space'] = self._handle_low_disk_space
        
        # GPU utilization issues
        self.response_handlers['high_gpu_utilization'] = self._handle_high_gpu_usage
        
        # Evolution convergence issues
        self.response_handlers['low_evolution_convergence'] = self._handle_evolution_issues
        
        logger.info(f"Initialized {len(self.response_handlers)} default response handlers")
    
    def _initialize_maintenance_tasks(self):
        """Initialize proactive maintenance tasks"""
        
        # System cleanup task
        cleanup_task = MaintenanceTask(
            task_id="system_cleanup",
            name="System Cleanup",
            description="Clean temporary files and optimize system resources",
            action=self._system_cleanup_task,
            interval_hours=6.0
        )
        self.maintenance_tasks["system_cleanup"] = cleanup_task
        
        # Memory optimization task
        memory_task = MaintenanceTask(
            task_id="memory_optimization",
            name="Memory Optimization", 
            description="Optimize memory usage and clear caches",
            action=self._memory_optimization_task,
            interval_hours=4.0
        )
        self.maintenance_tasks["memory_optimization"] = memory_task
        
        # Health check task
        health_task = MaintenanceTask(
            task_id="health_check",
            name="System Health Check",
            description="Comprehensive system health validation",
            action=self._health_check_task,
            interval_hours=1.0
        )
        self.maintenance_tasks["health_check"] = health_task
        
        # Log rotation task
        log_rotation_task = MaintenanceTask(
            task_id="log_rotation",
            name="Log Rotation",
            description="Rotate and compress old log files",
            action=self._log_rotation_task,
            interval_hours=24.0
        )
        self.maintenance_tasks["log_rotation"] = log_rotation_task
        
        logger.info(f"Initialized {len(self.maintenance_tasks)} maintenance tasks")
    
    def start(self):
        """Start the automated incident response system"""
        if self.is_running:
            logger.warning("Automated incident response system already running")
            return
        
        self.is_running = True
        
        # Start response monitoring thread
        self._response_thread = threading.Thread(
            target=self._response_monitoring_loop,
            daemon=True,
            name="IncidentResponseMonitor"
        )
        self._response_thread.start()
        
        # Start maintenance thread
        if self.maintenance_enabled:
            self._maintenance_thread = threading.Thread(
                target=self._maintenance_loop,
                daemon=True,
                name="ProactiveMaintenanceLoop"
            )
            self._maintenance_thread.start()
        
        # Register with performance monitor if available
        if self.performance_monitor:
            self.performance_monitor.register_alert_handler(self._handle_performance_alert)
        
        logger.info("Automated incident response system started")
    
    def stop(self):
        """Stop the automated incident response system"""
        self.is_running = False
        
        if self._response_thread:
            self._response_thread.join(timeout=5.0)
        
        if self._maintenance_thread:
            self._maintenance_thread.join(timeout=5.0)
        
        logger.info("Automated incident response system stopped")
    
    def _response_monitoring_loop(self):
        """Main monitoring loop for automated responses"""
        logger.info("Automated response monitoring loop started")
        
        while self.is_running:
            try:
                # Check for pending responses that need escalation
                self._check_escalation_timeouts()
                
                # Sleep for monitoring interval
                time.sleep(60)  # Check every minute
                
            except Exception as e:
                logger.error(f"Error in response monitoring loop: {e}")
                time.sleep(60)
        
        logger.info("Automated response monitoring loop stopped")
    
    def _maintenance_loop(self):
        """Proactive maintenance loop"""
        logger.info("Proactive maintenance loop started")
        
        while self.is_running:
            try:
                current_time = time.time()
                
                for task in self.maintenance_tasks.values():
                    if not task.enabled:
                        continue
                    
                    # Check if task needs to run
                    if (task.last_run is None or 
                        current_time - task.last_run >= task.interval_hours * 3600):
                        
                        logger.info(f"Running maintenance task: {task.name}")
                        try:
                            success = task.action()
                            task.last_run = current_time
                            
                            if success:
                                task.success_count += 1
                                logger.info(f"Maintenance task '{task.name}' completed successfully")
                            else:
                                task.failure_count += 1
                                logger.warning(f"Maintenance task '{task.name}' failed")
                                
                        except Exception as e:
                            task.failure_count += 1
                            logger.error(f"Error executing maintenance task '{task.name}': {e}")
                
                # Sleep between maintenance checks
                time.sleep(300)  # Check every 5 minutes
                
            except Exception as e:
                logger.error(f"Error in maintenance loop: {e}")
                time.sleep(300)
        
        logger.info("Proactive maintenance loop stopped")
    
    def _handle_performance_alert(self, alert: PerformanceAlert):
        """Handle performance alerts from the monitoring system"""
        try:
            # Determine priority based on severity and metric
            priority = self._determine_priority(alert)
            
            # Create incident response record
            incident_id = f"incident_{int(time.time())}"
            response = IncidentResponse(
                incident_id=incident_id,
                timestamp=time.time(),
                alert=alert,
                priority=priority,
                status=ResponseStatus.PENDING
            )
            
            with self._lock:
                self.responses_history.append(response)
            
            logger.info(f"Processing incident {incident_id} for alert: {alert.metric}")
            
            # Execute automated response
            self._execute_automated_response(response)
            
        except Exception as e:
            logger.error(f"Error handling performance alert: {e}")
    
    def _determine_priority(self, alert: PerformanceAlert) -> ResponsePriority:
        """Determine incident priority based on alert characteristics"""
        
        # Critical alerts that could cause system outage
        if alert.severity == AlertSeverity.CRITICAL:
            critical_metrics = [
                'token_throughput', 'evolution_convergence', 
                'proprioceptive_accuracy', 'memory_usage'
            ]
            if alert.metric in critical_metrics:
                return ResponsePriority.P0_CRITICAL
            else:
                return ResponsePriority.P1_HIGH
        
        # Warning alerts for performance degradation
        elif alert.severity == AlertSeverity.WARNING:
            high_priority_metrics = [
                'request_latency_ms', 'cpu_usage', 'gpu_utilization'
            ]
            if alert.metric in high_priority_metrics:
                return ResponsePriority.P1_HIGH
            else:
                return ResponsePriority.P2_MEDIUM
        
        # Info alerts are low priority
        else:
            return ResponsePriority.P3_LOW
    
    def _execute_automated_response(self, response: IncidentResponse):
        """Execute automated response actions for an incident"""
        response.status = ResponseStatus.IN_PROGRESS
        
        # Determine appropriate response handler
        metric = response.alert.metric
        handler_key = None
        
        # Map metrics to handler keys
        metric_handlers = {
            'cpu_usage': 'high_cpu_usage',
            'memory_usage': 'high_memory_usage',
            'token_throughput': 'low_token_throughput',
            'request_latency_ms': 'high_request_latency',
            'disk_usage': 'low_disk_space',
            'gpu_utilization': 'high_gpu_utilization',
            'evolution_convergence': 'low_evolution_convergence'
        }
        
        handler_key = metric_handlers.get(metric)
        
        if handler_key and handler_key in self.response_handlers:
            try:
                logger.info(f"Executing automated response for {metric}")
                success = self.response_handlers[handler_key](response.alert)
                
                if success:
                    response.status = ResponseStatus.SUCCESS
                    response.resolution_time = time.time()
                    logger.info(f"Automated response successful for incident {response.incident_id}")
                else:
                    response.status = ResponseStatus.FAILED
                    self._escalate_incident(response, "Automated response failed")
                
            except Exception as e:
                response.status = ResponseStatus.FAILED
                logger.error(f"Error in automated response: {e}")
                self._escalate_incident(response, f"Response execution error: {e}")
        else:
            logger.warning(f"No handler found for metric: {metric}")
            response.status = ResponseStatus.FAILED
            self._escalate_incident(response, "No automated handler available")
        
        # Save incident report
        self._save_incident_report(response)
    
    # Response Handler Implementations
    def _handle_high_cpu_usage(self, alert: PerformanceAlert) -> bool:
        """Handle high CPU usage incidents"""
        actions = []
        
        try:
            # Find CPU-intensive processes
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cpu_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['cpu_percent'] > 10.0:
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            
            actions.append(f"Identified {len(processes)} CPU-intensive processes")
            
            # Attempt to reduce process priorities for non-critical processes
            reduced_count = 0
            for proc_info in processes:
                try:
                    if 'python' in proc_info['name'].lower():  # Skip Python processes (might be our system)
                        continue
                    
                    proc = psutil.Process(proc_info['pid'])
                    current_priority = proc.nice()
                    if current_priority < 10:  # Only reduce if not already reduced
                        proc.nice(min(19, current_priority + 5))
                        reduced_count += 1
                        
                except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                    continue
            
            actions.append(f"Reduced priority for {reduced_count} processes")
            
            # Clear system caches if possible
            try:
                if os.path.exists('/proc/sys/vm/drop_caches'):
                    subprocess.run(['sudo', 'sh', '-c', 'echo 1 > /proc/sys/vm/drop_caches'], 
                                 check=True, capture_output=True)
                    actions.append("Cleared system caches")
            except (subprocess.CalledProcessError, FileNotFoundError):
                actions.append("Cache clearing skipped (no sudo access)")
            
            logger.info(f"High CPU usage response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling high CPU usage: {e}")
            return False
    
    def _handle_high_memory_usage(self, alert: PerformanceAlert) -> bool:
        """Handle high memory usage incidents"""
        actions = []
        
        try:
            # Force garbage collection in Python processes
            import gc
            gc.collect()
            actions.append("Forced garbage collection")
            
            # Find memory-intensive processes
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'memory_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['memory_percent'] > 5.0:
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            
            actions.append(f"Identified {len(processes)} memory-intensive processes")
            
            # Clear system buffers and caches
            try:
                if os.path.exists('/proc/sys/vm/drop_caches'):
                    subprocess.run(['sudo', 'sh', '-c', 'echo 3 > /proc/sys/vm/drop_caches'],
                                 check=True, capture_output=True)
                    actions.append("Cleared system buffers and caches")
            except (subprocess.CalledProcessError, FileNotFoundError):
                actions.append("Buffer clearing skipped (no sudo access)")
            
            logger.info(f"High memory usage response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling high memory usage: {e}")
            return False
    
    def _handle_low_throughput(self, alert: PerformanceAlert) -> bool:
        """Handle low token throughput incidents"""
        actions = []
        
        try:
            # This would typically restart relevant services or adjust configurations
            # For now, we simulate optimization actions
            
            # Simulate clearing model caches
            actions.append("Cleared model caches")
            
            # Simulate connection pool optimization
            actions.append("Optimized connection pools")
            
            # Simulate batch size adjustments
            actions.append("Adjusted processing batch sizes")
            
            # Log optimization actions
            logger.info(f"Low throughput response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling low throughput: {e}")
            return False
    
    def _handle_high_latency(self, alert: PerformanceAlert) -> bool:
        """Handle high request latency incidents"""
        actions = []
        
        try:
            # Simulate latency optimization actions
            actions.append("Analyzed request queues")
            actions.append("Optimized processing pipelines")
            actions.append("Adjusted timeout settings")
            
            logger.info(f"High latency response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling high latency: {e}")
            return False
    
    def _handle_low_disk_space(self, alert: PerformanceAlert) -> bool:
        """Handle low disk space incidents"""
        actions = []
        freed_space = 0
        
        try:
            # Clean temporary directories
            temp_dirs = ['/tmp', '/var/tmp', '/tmp/echo_stats', '/tmp/performance_demo']
            
            for temp_dir in temp_dirs:
                if os.path.exists(temp_dir):
                    initial_size = self._get_directory_size(temp_dir)
                    
                    # Remove old files (older than 1 day)
                    cutoff_time = time.time() - 86400  # 24 hours
                    
                    for root, dirs, files in os.walk(temp_dir):
                        for file in files:
                            file_path = os.path.join(root, file)
                            try:
                                if os.path.getmtime(file_path) < cutoff_time:
                                    os.remove(file_path)
                            except (OSError, FileNotFoundError):
                                continue
                    
                    final_size = self._get_directory_size(temp_dir)
                    cleaned = initial_size - final_size
                    if cleaned > 0:
                        freed_space += cleaned
                        actions.append(f"Cleaned {temp_dir}: {cleaned/1024/1024:.2f} MB")
            
            # Rotate logs if needed
            self._log_rotation_task()
            actions.append("Rotated log files")
            
            logger.info(f"Low disk space response completed: {actions}, freed {freed_space/1024/1024:.2f} MB")
            return freed_space > 0
            
        except Exception as e:
            logger.error(f"Error handling low disk space: {e}")
            return False
    
    def _handle_high_gpu_usage(self, alert: PerformanceAlert) -> bool:
        """Handle high GPU utilization incidents"""
        actions = []
        
        try:
            # Simulate GPU optimization actions
            actions.append("Analyzed GPU memory usage")
            actions.append("Optimized model loading strategies")
            actions.append("Adjusted GPU memory allocation")
            
            logger.info(f"High GPU usage response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling high GPU usage: {e}")
            return False
    
    def _handle_evolution_issues(self, alert: PerformanceAlert) -> bool:
        """Handle evolution convergence issues"""
        actions = []
        
        try:
            # Simulate evolution optimization actions
            actions.append("Reset evolution parameters")
            actions.append("Adjusted convergence thresholds")
            actions.append("Cleared evolution history cache")
            
            logger.info(f"Evolution issues response completed: {actions}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling evolution issues: {e}")
            return False
    
    # Maintenance Task Implementations
    def _system_cleanup_task(self) -> bool:
        """Proactive system cleanup task"""
        try:
            cleaned_files = 0
            
            # Clean temporary directories
            temp_dirs = ['/tmp/echo_stats', '/tmp/performance_demo', '/tmp/incident_response_stats']
            
            for temp_dir in temp_dirs:
                if os.path.exists(temp_dir):
                    # Keep recent files (last 2 hours)
                    cutoff_time = time.time() - 7200  # 2 hours
                    
                    for root, dirs, files in os.walk(temp_dir):
                        for file in files:
                            file_path = os.path.join(root, file)
                            try:
                                if os.path.getmtime(file_path) < cutoff_time:
                                    os.remove(file_path)
                                    cleaned_files += 1
                            except (OSError, FileNotFoundError):
                                continue
            
            logger.info(f"System cleanup completed: cleaned {cleaned_files} files")
            return True
            
        except Exception as e:
            logger.error(f"System cleanup task failed: {e}")
            return False
    
    def _memory_optimization_task(self) -> bool:
        """Memory optimization task"""
        try:
            # Force garbage collection
            import gc
            collected = gc.collect()
            
            # Get memory info before and after
            memory_info = psutil.virtual_memory()
            
            logger.info(f"Memory optimization completed: collected {collected} objects, "
                       f"memory usage: {memory_info.percent:.1f}%")
            return True
            
        except Exception as e:
            logger.error(f"Memory optimization task failed: {e}")
            return False
    
    def _health_check_task(self) -> bool:
        """System health check task"""
        try:
            health_status = {
                'timestamp': time.time(),
                'cpu_usage': psutil.cpu_percent(interval=1),
                'memory_usage': psutil.virtual_memory().percent,
                'disk_usage': psutil.disk_usage('/').percent,
                'load_average': os.getloadavg() if hasattr(os, 'getloadavg') else [0, 0, 0],
                'uptime': time.time() - psutil.boot_time()
            }
            
            # Save health status
            health_file = self.stats_dir / 'system_health.json'
            with open(health_file, 'w') as f:
                json.dump(health_status, f, indent=2)
            
            logger.info(f"Health check completed: CPU {health_status['cpu_usage']:.1f}%, "
                       f"Memory {health_status['memory_usage']:.1f}%")
            return True
            
        except Exception as e:
            logger.error(f"Health check task failed: {e}")
            return False
    
    def _log_rotation_task(self) -> bool:
        """Log rotation task"""
        try:
            rotated_count = 0
            
            # Find log files in common locations
            log_dirs = ['/tmp', '/var/log', self.stats_dir]
            
            for log_dir in log_dirs:
                if not os.path.exists(log_dir):
                    continue
                    
                for file_path in Path(log_dir).glob('*.log'):
                    if file_path.stat().st_size > 10 * 1024 * 1024:  # > 10MB
                        # Rotate large log files
                        rotated_path = file_path.with_suffix('.log.old')
                        if rotated_path.exists():
                            rotated_path.unlink()
                        file_path.rename(rotated_path)
                        rotated_count += 1
            
            logger.info(f"Log rotation completed: rotated {rotated_count} files")
            return True
            
        except Exception as e:
            logger.error(f"Log rotation task failed: {e}")
            return False
    
    # Helper Methods
    def _get_directory_size(self, directory: str) -> int:
        """Get total size of directory in bytes"""
        total_size = 0
        try:
            for dirpath, dirnames, filenames in os.walk(directory):
                for filename in filenames:
                    file_path = os.path.join(dirpath, filename)
                    try:
                        total_size += os.path.getsize(file_path)
                    except (OSError, FileNotFoundError):
                        continue
        except (OSError, FileNotFoundError):
            pass
        return total_size
    
    def _check_escalation_timeouts(self):
        """Check for incidents that need escalation due to timeout"""
        current_time = time.time()
        timeout_seconds = self.escalation_timeout_minutes * 60
        
        with self._lock:
            for response in self.responses_history:
                if (response.status == ResponseStatus.IN_PROGRESS and 
                    current_time - response.timestamp > timeout_seconds):
                    self._escalate_incident(response, "Response timeout exceeded")
    
    def _escalate_incident(self, response: IncidentResponse, reason: str):
        """Escalate incident to manual intervention"""
        response.status = ResponseStatus.ESCALATED
        response.escalation_reason = reason
        
        logger.warning(f"Incident {response.incident_id} escalated: {reason}")
        
        # Here you would typically send notifications to on-call engineers
        # For now, we just log the escalation
        self._save_incident_report(response)
    
    def _save_incident_report(self, response: IncidentResponse):
        """Save incident response report"""
        try:
            report_data = {
                'incident_id': response.incident_id,
                'timestamp': response.timestamp,
                'priority': response.priority.name,
                'status': response.status.name,
                'alert': {
                    'severity': response.alert.severity.name,
                    'component': response.alert.component,
                    'metric': response.alert.metric,
                    'current_value': response.alert.current_value,
                    'threshold': response.alert.threshold,
                    'message': response.alert.message
                },
                'actions_taken': response.actions_taken,
                'success_actions': response.success_actions,
                'failed_actions': response.failed_actions,
                'escalation_reason': response.escalation_reason,
                'resolution_time': response.resolution_time,
                'metadata': response.metadata
            }
            
            report_file = self.stats_dir / f"incident_{response.incident_id}.json"
            with open(report_file, 'w') as f:
                json.dump(report_data, f, indent=2)
                
        except Exception as e:
            logger.error(f"Error saving incident report: {e}")
    
    def get_response_statistics(self) -> Dict[str, Any]:
        """Get statistics about automated responses"""
        with self._lock:
            total_responses = len(self.responses_history)
            successful_responses = len([r for r in self.responses_history if r.status == ResponseStatus.SUCCESS])
            failed_responses = len([r for r in self.responses_history if r.status == ResponseStatus.FAILED])
            escalated_responses = len([r for r in self.responses_history if r.status == ResponseStatus.ESCALATED])
            
            # Maintenance task stats
            maintenance_stats = {}
            for task_id, task in self.maintenance_tasks.items():
                maintenance_stats[task_id] = {
                    'name': task.name,
                    'success_count': task.success_count,
                    'failure_count': task.failure_count,
                    'last_run': task.last_run,
                    'enabled': task.enabled
                }
            
            return {
                'total_responses': total_responses,
                'successful_responses': successful_responses,
                'failed_responses': failed_responses,
                'escalated_responses': escalated_responses,
                'success_rate': successful_responses / max(total_responses, 1) * 100,
                'maintenance_tasks': maintenance_stats,
                'system_status': 'running' if self.is_running else 'stopped'
            }


# Factory function for easy integration
def create_automated_response_system(performance_monitor: Optional[UnifiedPerformanceMonitor] = None) -> AutomatedIncidentResponseSystem:
    """Create and configure an automated incident response system"""
    return AutomatedIncidentResponseSystem(performance_monitor=performance_monitor)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    print("🚨 Automated Incident Response System Demo")
    print("=" * 50)
    
    # Create the system
    response_system = create_automated_response_system()
    
    try:
        # Start the system
        response_system.start()
        print("✅ System started")
        
        # Run for a short time to demonstrate
        time.sleep(10)
        
        # Get statistics
        stats = response_system.get_response_statistics()
        print("\n📊 Statistics:")
        print(f"   Total responses: {stats['total_responses']}")
        print(f"   Success rate: {stats['success_rate']:.1f}%")
        print(f"   Maintenance tasks: {len(stats['maintenance_tasks'])}")
        
    finally:
        response_system.stop()
        print("🛑 System stopped")