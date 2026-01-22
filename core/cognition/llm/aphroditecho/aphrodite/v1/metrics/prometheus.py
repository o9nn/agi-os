import os
import tempfile
from typing import Optional
from loguru import logger
from prometheus_client import REGISTRY, CollectorRegistry, multiprocess
_prometheus_multiproc_dir: Optional[tempfile.TemporaryDirectory] = None
def setup_multiprocess_prometheus():
    global _prometheus_multiproc_dir
    if 'PROMETHEUS_MULTIPROC_DIR' not in os.environ:
        _prometheus_multiproc_dir = tempfile.TemporaryDirectory()
        os.environ['PROMETHEUS_MULTIPROC_DIR'] = _prometheus_multiproc_dir.name
        logger.debug('Created PROMETHEUS_MULTIPROC_DIR at {}', _prometheus_multiproc_dir.name)
    else:
        logger.warning('Found PROMETHEUS_MULTIPROC_DIR was set by user. This directory must be wiped between Aphrodite runs or you will find inaccurate metrics. Unset the variable and Aphrodite will properly handle cleanup.')
def get_prometheus_registry():
    if os.getenv('PROMETHEUS_MULTIPROC_DIR') is not None:
        logger.debug('Using multiprocess registry for prometheus metrics')
        registry = CollectorRegistry()
        multiprocess.MultiProcessCollector(registry)
        return registry
    return REGISTRY
def unregister_aphrodite_metrics():
    registry = REGISTRY
    for collector in list(registry._collector_to_names):
        if hasattr(collector, '_name') and 'aphrodite' in collector._name:
            registry.unregister(collector)
def shutdown_prometheus():
    path = _prometheus_multiproc_dir
    if path is None:
        return
    try:
        pid = os.getpid()
        multiprocess.mark_process_dead(pid, path)
        logger.debug('Marked Prometheus metrics for process {} as dead', pid)
    except Exception as e:
        logger.error('Error during metrics cleanup: {}', str(e))