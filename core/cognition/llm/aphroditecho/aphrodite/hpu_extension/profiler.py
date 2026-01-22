import gc
import json
import os
import queue
import threading
import time
import uuid
from contextlib import contextmanager
from typing import Any, List
import psutil
import torch
from habana_frameworks.torch import torch
from loguru import logger
from aphrodite.hpu_extension.utils import is_fake_hpu
class FileWriter(threading.Thread):
    def __init__(self, filename, event_queue):
        super().__init__()
        self.filename = filename
        self.event_queue = event_queue
        self.daemon = True
        self.timer_event = threading.Event()
    def _drain_event_queue(self):
        content = ''
        while True:
            try:
                element = self.event_queue.get_nowait()
                content += element
            except queue.Empty:
                break
        return content
    def run(self):
        while not self.timer_event.wait(1):
            content = self.event_queue.get()
            content += self._drain_event_queue()
            with open(self.filename, 'a') as outfile:
                outfile.write(content)
class HabanaHighLevelProfiler:
    profiling_trace_events: queue.Queue = queue.Queue()
    event_tid = {'counter': 1, 'external': 2, 'internal': 3}
    event_cache: List[Any] = []
    def __init__(self, aphrodite_instance_id=None):
        self.enabled = os.getenv('APHRODITE_PROFILER_ENABLED', 'false').lower() == 'true' and int(os.getenv('RANK', '0')) == 0
        self.pid = os.getpid()
        if self.enabled:
            self.aphrodite_instance_id = aphrodite_instance_id if aphrodite_instance_id is not None else f'aphrodite-instance-{self.pid}-{str(uuid.uuid4().hex)}'
            msg = f'Profiler enabled for: {self.aphrodite_instance_id}'
            logger.info(msg)
            self.filename = f'server_events_{self.aphrodite_instance_id}.json'
            with open(self.filename, 'w') as outfile:
                outfile.write('[')
            file_writer = FileWriter(self.filename, self.profiling_trace_events)
            file_writer.start()
        if os.getenv('APHRODITE_PROFILER_ENABLED') == 'full':
            self.enabled = True
    def _dump_with_sep(self, entry):
        entry = json.dumps(entry) + ','
        self.profiling_trace_events.put(entry)
    def get_timestamp_us(self):
        return time.time() * 1000000.0
    def record_counter(self, ts, counter):
        if self.enabled:
            self._dump_with_sep({'pid': self.pid, 'tid': self.event_tid['counter'], 'ph': 'C', 'name': 'utils', 'ts': ts, 'args': counter})
    def start(self, type, name, args=None):
        if self.enabled:
            ts = self.get_timestamp_us()
            if args is not None and 'counter' in args:
                self.record_counter(ts, args['counter'])
                del args['counter']
            event = {'pid': self.pid, 'tid': self.event_tid[type], 'ph': 'X', 'name': name, 'ts': ts, 'dur': None, 'args': args}
            self.event_cache.append(event)
    def end(self):
        if self.enabled:
            ts = self.get_timestamp_us()
            if not self.event_cache:
                logger.warning('Profiler: end() call does not have matching start() call. Disabling profiler.')
                self.enabled = False
                return
            event = self.event_cache.pop()
            event['dur'] = ts - event['ts']
            self._dump_with_sep(event)
    @contextmanager
    def record_event(self, type, name, args=None):
        if self.enabled:
            self.start(type, name, args)
            yield
            self.end()
        else:
            yield
def format_bytes(size):
    power = 2 ** 10
    n = 0
    power_labels = {0: '', 1: 'Ki', 2: 'Mi', 3: 'Gi', 4: 'Ti'}
    while abs(size) > power:
        size /= power
        n += 1
    return f"{size:.4g} {power_labels[n] + 'B'}"
class HabanaMemoryProfiler:
    def __init__(self, device=None):
        self.device = device
    @staticmethod
    def current_device_memory_usage() -> float:
        if is_fake_hpu():
            return 0
        free_hpu_memory, total_hpu_memory = torch.hpu.mem_get_info()
        return total_hpu_memory - free_hpu_memory
    @staticmethod
    def current_free_device_memory() -> float:
        if is_fake_hpu():
            return 0
        free_hpu_memory, _ = torch.hpu.mem_get_info()
        return free_hpu_memory
    @staticmethod
    def total_device_memory() -> float:
        if is_fake_hpu():
            return 0
        _, total_hpu_memory = torch.hpu.mem_get_info()
        return total_hpu_memory
    @staticmethod
    def current_host_memory_usage() -> float:
        return HabanaMemoryProfiler.total_host_memory() - HabanaMemoryProfiler.current_free_host_memory()
    @staticmethod
    def current_free_host_memory() -> float:
        return psutil.virtual_memory().available
    @staticmethod
    def total_host_memory() -> float:
        return psutil.virtual_memory().total
    def get_summary_string(self):
        if getattr(self, 'final_device_memory', None) is None or getattr(self, 'final_host_memory', None) is None:
            raise RuntimeError('HabanaMemoryProfiler.get_summary_string() can only be called after closing context manager')
        return f'{format_bytes(self.consumed_device_memory)} of device memory ({format_bytes(self.final_device_memory)}/{format_bytes(HabanaMemoryProfiler.total_device_memory())} used) and {format_bytes(self.consumed_host_memory)} of host memory ({format_bytes(self.final_host_memory)}/{format_bytes(HabanaMemoryProfiler.total_host_memory())} used)'
    def __enter__(self):
        gc.collect()
        self.initial_device_memory = HabanaMemoryProfiler.current_device_memory_usage()
        self.initial_host_memory = HabanaMemoryProfiler.current_host_memory_usage()
        return self
    def __exit__(self, exc_type, exc_val, exc_tb):
        gc.collect()
        self.final_device_memory = HabanaMemoryProfiler.current_device_memory_usage()
        self.final_host_memory = HabanaMemoryProfiler.current_host_memory_usage()
        self.consumed_device_memory = self.final_device_memory - self.initial_device_memory
        self.consumed_host_memory = self.final_host_memory - self.initial_host_memory