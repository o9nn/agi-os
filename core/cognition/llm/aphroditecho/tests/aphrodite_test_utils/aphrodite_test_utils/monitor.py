import contextlib
import dataclasses
import sys
import traceback
from collections.abc import Generator
from typing import Callable, Generic, TypeVar
_T = TypeVar('_T')
@dataclasses.dataclass
class MonitoredValues(Generic[_T]):
    values: list[_T] = dataclasses.field(default_factory=list)
    trace_stacks: list[str] = dataclasses.field(default_factory=list)
@contextlib.contextmanager
def monitor(measure_func: Callable[[], _T]) -> Generator[MonitoredValues[_T], None, None]:
    monitored_values = MonitoredValues[_T]()
    def _trace_calls(frame, event, arg=None):
        nonlocal monitored_values
        if event in ['line']:
            try:
                sys.settrace(None)
                current_value = measure_func()
                if len(monitored_values.values) == 0 or current_value != monitored_values.values[-1]:
                    monitored_values.values.append(current_value)
                    monitored_values.trace_stacks.append(''.join(traceback.format_stack()))
                sys.settrace(_trace_calls)
            except NameError:
                pass
        return _trace_calls
    try:
        sys.settrace(_trace_calls)
        yield monitored_values
    finally:
        sys.settrace(None)