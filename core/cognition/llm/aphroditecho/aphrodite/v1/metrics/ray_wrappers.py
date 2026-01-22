import time
from typing import Optional, Union
from aphrodite.v1.metrics.loggers import PrometheusStatLogger
from aphrodite.v1.spec_decode.metrics import SpecDecodingProm
try:
    from ray.util import metrics as ray_metrics
    from ray.util.metrics import Metric
except ImportError:
    ray_metrics = None
class RayPrometheusMetric:
    def __init__(self):
        if ray_metrics is None:
            raise ImportError('RayPrometheusMetric requires Ray to be installed.')
        self.metric: Metric = None
    def labels(self, *labels, **labelskwargs):
        if labelskwargs:
            for k, v in labelskwargs.items():
                if not isinstance(v, str):
                    labelskwargs[k] = str(v)
            self.metric.set_default_tags(labelskwargs)
        if labels:
            if len(labels) != len(self.metric._tag_keys):
                raise ValueError(f'Number of labels must match the number of tag keys. Expected {len(self.metric._tag_keys)}, got {len(labels)}')
            self.metric.set_default_tags(dict(zip(self.metric._tag_keys, labels)))
        return self
class RayGaugeWrapper(RayPrometheusMetric):
    def __init__(self, name: str, documentation: Optional[str]='', labelnames: Optional[list[str]]=None, multiprocess_mode: Optional[str]=''):
        del multiprocess_mode
        labelnames_tuple = tuple(labelnames) if labelnames else None
        self.metric = ray_metrics.Gauge(name=name, description=documentation, tag_keys=labelnames_tuple)
    def set(self, value: Union[int, float]):
        return self.metric.set(value)
    def set_to_current_time(self):
        return self.metric.set(time.time())
class RayCounterWrapper(RayPrometheusMetric):
    def __init__(self, name: str, documentation: Optional[str]='', labelnames: Optional[list[str]]=None):
        labelnames_tuple = tuple(labelnames) if labelnames else None
        self.metric = ray_metrics.Counter(name=name, description=documentation, tag_keys=labelnames_tuple)
    def inc(self, value: Union[int, float]=1.0):
        if value == 0:
            return
        return self.metric.inc(value)
class RayHistogramWrapper(RayPrometheusMetric):
    def __init__(self, name: str, documentation: Optional[str]='', labelnames: Optional[list[str]]=None, buckets: Optional[list[float]]=None):
        labelnames_tuple = tuple(labelnames) if labelnames else None
        boundaries = buckets if buckets else []
        self.metric = ray_metrics.Histogram(name=name, description=documentation, tag_keys=labelnames_tuple, boundaries=boundaries)
    def observe(self, value: Union[int, float]):
        return self.metric.observe(value)
class RaySpecDecodingProm(SpecDecodingProm):
    _counter_cls = RayCounterWrapper
class RayPrometheusStatLogger(PrometheusStatLogger):
    _gauge_cls = RayGaugeWrapper
    _counter_cls = RayCounterWrapper
    _histogram_cls = RayHistogramWrapper
    _spec_decoding_cls = RaySpecDecodingProm
    @staticmethod
    def _unregister_aphrodite_metrics():
        pass