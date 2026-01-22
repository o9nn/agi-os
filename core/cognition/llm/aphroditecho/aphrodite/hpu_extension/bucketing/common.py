import os
from typing import Dict
import inspect
from weakref import WeakValueDictionary
class WeakSingleton(type):
    _instances: WeakValueDictionary[type, object] = WeakValueDictionary()
    _instances_argspec: Dict[type, object] = {}
    def __call__(cls, *args, **kwargs):
        argspec = inspect.getcallargs(super().__call__, args, kwargs)
        if cls not in cls._instances:
            instance = super().__call__(*args, **kwargs)
            cls._instances[cls] = instance
            cls._instances_argspec[cls] = argspec
        assert cls._instances_argspec[cls] == argspec, 'Singleton instance already initialized with different arguments'
        return cls._instances[cls]
def get_bucketing_context():
    use_exponential_bucketing = os.environ.get('APHRODITE_EXPONENTIAL_BUCKETING', 'false').lower() == 'true'
    if use_exponential_bucketing:
        from aphrodite.hpu_extension.bucketing.exponential import HPUExponentialBucketingContext as HPUBucketingContext
    else:
        from aphrodite.hpu_extension.bucketing.linear import HPUBucketingContext
    return HPUBucketingContext