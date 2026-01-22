import time
import logging
import threading
import heapq
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field
from enum import IntEnum
from queue import Queue, Empty
from collections import defaultdict
import uuid
import json
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('AGI_Scheduler')
class TaskPriority(IntEnum):
    REALTIME = 0
    HIGH = 1
    NORMAL = 2
    LOW = 3
    IDLE = 4
class TaskState(IntEnum):
    CREATED = 0
    QUEUED = 1
    RUNNING = 2
    BLOCKED = 3
    COMPLETED = 4
    CANCELLED = 5
    FAILED = 6
class TaskType(IntEnum):
    INFERENCE = 1
    ATTENTION = 2
    LEARNING = 3
    PERCEPTION = 4
    ACTION = 5
    MEMORY = 6
    COMMUNICATION = 7
    SYSTEM = 8
@dataclass(order=True)
class CognitiveTask:
    sort_priority: float = field(compare=True)
    task_id: str = field(compare=False, default_factory=lambda: str(uuid.uuid4())[:8])
    name: str = field(compare=False, default='')
    task_type: TaskType = field(compare=False, default=TaskType.SYSTEM)
    base_priority: TaskPriority = field(compare=False, default=TaskPriority.NORMAL)
    state: TaskState = field(compare=False, default=TaskState.CREATED)
    sti: float = field(compare=False, default=0.0)
    lti: float = field(compare=False, default=0.0)
    deadline: Optional[float] = field(compare=False, default=None)
    created_at: float = field(compare=False, default_factory=time.time)
    started_at: Optional[float] = field(compare=False, default=None)
    completed_at: Optional[float] = field(compare=False, default=None)
    execute_fn: Optional[Callable] = field(compare=False, default=None)
    args: tuple = field(compare=False, default_factory=tuple)
    kwargs: dict = field(compare=False, default_factory=dict)
    result: Any = field(compare=False, default=None)
    error: Optional[str] = field(compare=False, default=None)
    cpu_weight: float = field(compare=False, default=1.0)
    memory_mb: int = field(compare=False, default=0)
    def __post_init__(self):
        if self.sort_priority == 0:
            self.sort_priority = self._calculate_effective_priority()
    def _calculate_effective_priority(self) -> float:
        priority = float(self.base_priority)
        sti_bonus = max(0, min(self.sti, 100)) / 100.0 * 0.5
        priority -= sti_bonus
        lti_bonus = max(0, min(self.lti, 100)) / 100.0 * 0.1
        priority -= lti_bonus
        if self.deadline:
            time_to_deadline = self.deadline - time.time()
            if time_to_deadline < 0:
                priority -= 1.0
            elif time_to_deadline < 60:
                priority -= 0.8
            elif time_to_deadline < 300:
                priority -= 0.5
            elif time_to_deadline < 3600:
                priority -= 0.2
        return max(0, priority)
    def update_priority(self):
        self.sort_priority = self._calculate_effective_priority()
class SchedulerMetrics:
    def __init__(self):
        self.tasks_created = 0
        self.tasks_completed = 0
        self.tasks_failed = 0
        self.tasks_cancelled = 0
        self.total_wait_time = 0.0
        self.total_exec_time = 0.0
        self.priority_distributions = defaultdict(int)
        self.task_type_counts = defaultdict(int)
    def record_task_created(self, task: CognitiveTask):
        self.tasks_created += 1
        self.priority_distributions[task.base_priority.name] += 1
        self.task_type_counts[task.task_type.name] += 1
    def record_task_completed(self, task: CognitiveTask):
        self.tasks_completed += 1
        if task.started_at and task.completed_at:
            self.total_exec_time += task.completed_at - task.started_at
        if task.started_at:
            self.total_wait_time += task.started_at - task.created_at
    def record_task_failed(self, task: CognitiveTask):
        self.tasks_failed += 1
    def get_statistics(self) -> Dict[str, Any]:
        completed = self.tasks_completed or 1
        return {'total_created': self.tasks_created, 'total_completed': self.tasks_completed, 'total_failed': self.tasks_failed, 'total_cancelled': self.tasks_cancelled, 'avg_wait_time': self.total_wait_time / completed, 'avg_exec_time': self.total_exec_time / completed, 'priority_distribution': dict(self.priority_distributions), 'task_type_distribution': dict(self.task_type_counts), 'success_rate': self.tasks_completed / max(self.tasks_created, 1)}
class AGIScheduler:
    def __init__(self, num_workers: int=4, attention_update_interval: float=1.0, starvation_threshold: float=30.0):
        self.num_workers = num_workers
        self.attention_update_interval = attention_update_interval
        self.starvation_threshold = starvation_threshold
        self.task_queue: List[CognitiveTask] = []
        self.tasks: Dict[str, CognitiveTask] = {}
        self.running_tasks: Dict[str, CognitiveTask] = {}
        self._lock = threading.RLock()
        self._queue_not_empty = threading.Condition(self._lock)
        self.workers: List[threading.Thread] = []
        self.running = False
        self.metrics = SchedulerMetrics()
        self._attention_updater: Optional[threading.Thread] = None
        logger.info(f'AGI Scheduler initialized with {num_workers} workers')
    def submit(self, name: str, execute_fn: Callable, priority: TaskPriority=TaskPriority.NORMAL, task_type: TaskType=TaskType.SYSTEM, deadline: Optional[float]=None, sti: float=0.0, lti: float=0.0, args: tuple=(), kwargs: dict=None) -> str:
        task = CognitiveTask(sort_priority=0, name=name, task_type=task_type, base_priority=priority, deadline=deadline, sti=sti, lti=lti, execute_fn=execute_fn, args=args, kwargs=kwargs or {})
        task.update_priority()
        with self._lock:
            self.tasks[task.task_id] = task
            task.state = TaskState.QUEUED
            heapq.heappush(self.task_queue, task)
            self.metrics.record_task_created(task)
            self._queue_not_empty.notify()
        logger.debug(f'Task submitted: {task.name} (id={task.task_id}, priority={task.sort_priority:.2f})')
        return task.task_id
    def submit_inference(self, name: str, execute_fn: Callable, sti: float=50.0, **kwargs) -> str:
        return self.submit(name=name, execute_fn=execute_fn, priority=TaskPriority.HIGH, task_type=TaskType.INFERENCE, sti=sti, **kwargs)
    def submit_learning(self, name: str, execute_fn: Callable, **kwargs) -> str:
        return self.submit(name=name, execute_fn=execute_fn, priority=TaskPriority.LOW, task_type=TaskType.LEARNING, **kwargs)
    def update_task_attention(self, task_id: str, sti: float, lti: float):
        with self._lock:
            if task_id in self.tasks:
                task = self.tasks[task_id]
                task.sti = sti
                task.lti = lti
                task.update_priority()
                heapq.heapify(self.task_queue)
    def cancel(self, task_id: str) -> bool:
        with self._lock:
            if task_id in self.tasks:
                task = self.tasks[task_id]
                if task.state == TaskState.QUEUED:
                    task.state = TaskState.CANCELLED
                    self.metrics.tasks_cancelled += 1
                    logger.info(f'Task cancelled: {task.name}')
                    return True
        return False
    def get_task(self, task_id: str) -> Optional[CognitiveTask]:
        return self.tasks.get(task_id)
    def _worker_loop(self, worker_id: int):
        logger.debug(f'Worker {worker_id} started')
        while self.running:
            task = None
            with self._queue_not_empty:
                while self.running and (not self.task_queue):
                    self._queue_not_empty.wait(timeout=1.0)
                if not self.running:
                    break
                for i, t in enumerate(self.task_queue):
                    if t.state == TaskState.QUEUED:
                        task = heapq.heappop(self.task_queue)
                        while self.task_queue and self.task_queue[0].state != TaskState.QUEUED:
                            heapq.heappop(self.task_queue)
                        break
            if task is None:
                continue
            self._execute_task(task, worker_id)
    def _execute_task(self, task: CognitiveTask, worker_id: int):
        task.state = TaskState.RUNNING
        task.started_at = time.time()
        with self._lock:
            self.running_tasks[task.task_id] = task
        logger.debug(f'Worker {worker_id} executing: {task.name}')
        try:
            if task.execute_fn:
                task.result = task.execute_fn(*task.args, **task.kwargs)
            task.state = TaskState.COMPLETED
            task.completed_at = time.time()
            self.metrics.record_task_completed(task)
            logger.debug(f'Task completed: {task.name} ({task.completed_at - task.started_at:.3f}s)')
        except Exception as e:
            task.state = TaskState.FAILED
            task.error = str(e)
            task.completed_at = time.time()
            self.metrics.record_task_failed(task)
            logger.error(f'Task failed: {task.name} - {e}')
        finally:
            with self._lock:
                self.running_tasks.pop(task.task_id, None)
    def _anti_starvation_loop(self):
        while self.running:
            time.sleep(self.attention_update_interval)
            with self._lock:
                current_time = time.time()
                for task in self.task_queue:
                    if task.state == TaskState.QUEUED:
                        wait_time = current_time - task.created_at
                        if wait_time > self.starvation_threshold:
                            task.sort_priority = max(0, task.sort_priority - 0.1)
                heapq.heapify(self.task_queue)
    def start(self):
        if self.running:
            logger.warning('Scheduler already running')
            return
        self.running = True
        for i in range(self.num_workers):
            worker = threading.Thread(target=self._worker_loop, args=(i,), daemon=True)
            worker.start()
            self.workers.append(worker)
        self._attention_updater = threading.Thread(target=self._anti_starvation_loop, daemon=True)
        self._attention_updater.start()
        logger.info(f'AGI Scheduler started with {self.num_workers} workers')
    def stop(self, wait: bool=True, timeout: float=10.0):
        self.running = False
        with self._queue_not_empty:
            self._queue_not_empty.notify_all()
        if wait:
            deadline = time.time() + timeout
            for worker in self.workers:
                remaining = max(0, deadline - time.time())
                worker.join(timeout=remaining)
        self.workers.clear()
        logger.info('AGI Scheduler stopped')
    def get_status(self) -> Dict[str, Any]:
        with self._lock:
            return {'running': self.running, 'num_workers': self.num_workers, 'queued_tasks': len([t for t in self.task_queue if t.state == TaskState.QUEUED]), 'running_tasks': len(self.running_tasks), 'total_tasks': len(self.tasks)}
    def get_metrics(self) -> Dict[str, Any]:
        return self.metrics.get_statistics()
    def get_queue_snapshot(self) -> List[Dict[str, Any]]:
        with self._lock:
            return [{'task_id': t.task_id, 'name': t.name, 'priority': t.sort_priority, 'state': t.state.name, 'type': t.task_type.name, 'sti': t.sti, 'deadline': t.deadline} for t in sorted(self.task_queue, key=lambda x: x.sort_priority)[:20]]
_scheduler_instance: Optional[AGIScheduler] = None
def get_scheduler(num_workers: int=4) -> AGIScheduler:
    global _scheduler_instance
    if _scheduler_instance is None:
        _scheduler_instance = AGIScheduler(num_workers=num_workers)
    return _scheduler_instance
if __name__ == '__main__':
    import random
    scheduler = get_scheduler(num_workers=2)
    scheduler.start()
    def sample_task(task_name: str, duration: float):
        time.sleep(duration)
        return f'{task_name} completed'
    task_ids = []
    for i in range(3):
        tid = scheduler.submit_inference(name=f'PLN_Inference_{i}', execute_fn=sample_task, args=(f'inference_{i}', random.uniform(0.1, 0.5)), sti=random.uniform(50, 100))
        task_ids.append(tid)
    for i in range(5):
        tid = scheduler.submit(name=f'Normal_Task_{i}', execute_fn=sample_task, priority=TaskPriority.NORMAL, args=(f'normal_{i}', random.uniform(0.1, 0.3)))
        task_ids.append(tid)
    for i in range(3):
        tid = scheduler.submit_learning(name=f'Learning_Task_{i}', execute_fn=sample_task, args=(f'learning_{i}', random.uniform(0.2, 0.5)))
        task_ids.append(tid)
    tid = scheduler.submit(name='Urgent_Deadline_Task', execute_fn=sample_task, priority=TaskPriority.NORMAL, deadline=time.time() + 2, args=('deadline', 0.1))
    task_ids.append(tid)
    print('\nWaiting for tasks to complete...')
    time.sleep(5)
    print('\n' + '=' * 50)
    print('Scheduler Metrics:')
    print('=' * 50)
    metrics = scheduler.get_metrics()
    print(json.dumps(metrics, indent=2))
    print('\n' + '=' * 50)
    print('Scheduler Status:')
    print('=' * 50)
    status = scheduler.get_status()
    print(json.dumps(status, indent=2))
    scheduler.stop()