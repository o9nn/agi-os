import logging
import time
from typing import Any, Dict, Optional, Callable
from dataclasses import dataclass
from collections import deque
logger = logging.getLogger(__name__)
@dataclass
class LoadMetrics:
    current_load: float = 0.0
    cpu_utilization: float = 0.0
    memory_utilization: float = 0.0
    gpu_utilization: float = 0.0
    active_requests: int = 0
    queue_depth: int = 0
    avg_response_time: float = 0.0
    last_updated: float = 0.0
class ServerLoadTracker:
    def __init__(self, update_interval: float=1.0, history_window: int=60, enable_system_metrics: bool=True):
        self.update_interval = update_interval
        self.history_window = history_window
        self.enable_system_metrics = enable_system_metrics
        self._current_metrics = LoadMetrics()
        self._load_history = deque(maxlen=history_window)
        self._last_update = 0.0
        self._app_state = None
        self._custom_load_providers = []
        logger.info(f'ServerLoadTracker initialized with {update_interval}s update interval')
    def set_app_state(self, app_state):
        self._app_state = app_state
        logger.info('Connected to FastAPI app state for load tracking')
    def add_load_provider(self, provider: Callable[[], float], weight: float=1.0):
        self._custom_load_providers.append((provider, weight))
        logger.info(f'Added custom load provider with weight {weight}')
    def get_current_load(self) -> float:
        current_time = time.time()
        if current_time - self._last_update > self.update_interval:
            self._update_metrics()
            self._last_update = current_time
        return self._current_metrics.current_load
    def get_load_metrics(self) -> LoadMetrics:
        return self._current_metrics
    def get_load_trend(self, window_size: int=10) -> float:
        if len(self._load_history) < window_size:
            return 0.0
        recent_samples = list(self._load_history)[-window_size:]
        if len(recent_samples) < 2:
            return 0.0
        x_vals = list(range(len(recent_samples)))
        y_vals = recent_samples
        n = len(recent_samples)
        sum_x = sum(x_vals)
        sum_y = sum(y_vals)
        sum_xy = sum((x * y for x, y in zip(x_vals, y_vals)))
        sum_x2 = sum((x * x for x in x_vals))
        if n * sum_x2 - sum_x * sum_x == 0:
            return 0.0
        slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        return slope
    def _update_metrics(self):
        try:
            metrics = LoadMetrics(last_updated=time.time())
            aphrodite_load = self._get_aphrodite_load()
            if aphrodite_load is not None:
                metrics.active_requests = int(aphrodite_load)
                expected_capacity = 50.0
                metrics.current_load = min(1.0, aphrodite_load / expected_capacity)
            if self.enable_system_metrics:
                system_metrics = self._get_system_metrics()
                metrics.cpu_utilization = system_metrics.get('cpu', 0.0)
                metrics.memory_utilization = system_metrics.get('memory', 0.0)
                metrics.gpu_utilization = system_metrics.get('gpu', 0.0)
                system_load = max(metrics.cpu_utilization, metrics.memory_utilization, metrics.gpu_utilization)
                if metrics.current_load == 0.0:
                    metrics.current_load = system_load
                else:
                    metrics.current_load = 0.7 * metrics.current_load + 0.3 * system_load
            if self._custom_load_providers:
                custom_loads = []
                total_weight = 0.0
                for provider, weight in self._custom_load_providers:
                    try:
                        load_value = provider()
                        if 0.0 <= load_value <= 1.0:
                            custom_loads.append(load_value * weight)
                            total_weight += weight
                    except Exception as e:
                        logger.warning(f'Custom load provider failed: {e}')
                if custom_loads and total_weight > 0:
                    weighted_custom_load = sum(custom_loads) / total_weight
                    if metrics.current_load == 0.0:
                        metrics.current_load = weighted_custom_load
                    else:
                        metrics.current_load = 0.8 * metrics.current_load + 0.2 * weighted_custom_load
            metrics.current_load = max(0.0, min(1.0, metrics.current_load))
            self._current_metrics = metrics
            self._load_history.append(metrics.current_load)
            logger.debug(f'Load metrics updated: load={metrics.current_load:.3f}, cpu={metrics.cpu_utilization:.2f}, mem={metrics.memory_utilization:.2f}, active_reqs={metrics.active_requests}')
        except Exception as e:
            logger.error(f'Failed to update load metrics: {e}')
    def _get_aphrodite_load(self) -> Optional[float]:
        try:
            if self._app_state and hasattr(self._app_state, 'server_load_metrics'):
                return float(self._app_state.server_load_metrics)
        except Exception as e:
            logger.debug(f'Could not get Aphrodite load metrics: {e}')
        return None
    def _get_system_metrics(self) -> Dict[str, float]:
        metrics = {'cpu': 0.0, 'memory': 0.0, 'gpu': 0.0}
        try:
            import psutil
            metrics['cpu'] = psutil.cpu_percent(interval=None) / 100.0
            memory = psutil.virtual_memory()
            metrics['memory'] = memory.percent / 100.0
        except ImportError:
            logger.debug('psutil not available for system metrics')
        except Exception as e:
            logger.warning(f'Failed to get system metrics: {e}')
        try:
            import pynvml
            pynvml.nvmlInit()
            handle = pynvml.nvmlDeviceGetHandleByIndex(0)
            utilization = pynvml.nvmlDeviceGetUtilizationRates(handle)
            metrics['gpu'] = utilization.gpu / 100.0
        except (ImportError, Exception):
            pass
        return metrics
def create_load_tracker_for_app(app_state) -> ServerLoadTracker:
    tracker = ServerLoadTracker(update_interval=0.5, history_window=120, enable_system_metrics=True)
    tracker.set_app_state(app_state)
    return tracker
def get_batch_load_function(app_state) -> Callable[[], float]:
    tracker = create_load_tracker_for_app(app_state)
    return tracker.get_current_load