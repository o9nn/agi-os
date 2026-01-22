import contextlib
import dataclasses
import sys
import traceback
from collections.abc import Generator
from typing import Callable
@dataclasses.dataclass
class BlameResult:
    found: bool = False
    trace_stack: str = ''
@contextlib.contextmanager
def blame(func: Callable) -> Generator[BlameResult, None, None]:
    result = BlameResult()
    def _trace_calls(frame, event, arg=None):
        nonlocal result
        if event in ['call', 'return']:
            try:
                sys.settrace(None)
                if not result.found and func():
                    result.found = True
                    result.trace_stack = ''.join(traceback.format_stack())
                sys.settrace(_trace_calls)
            except NameError:
                pass
        return _trace_calls
    try:
        sys.settrace(_trace_calls)
        yield result
    finally:
        sys.settrace(None)