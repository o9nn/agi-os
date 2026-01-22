from contextlib import contextmanager
from typing import Any
_compile_context: Any = None
def get_compile_context() -> Any:
    return _compile_context
@contextmanager
def set_compile_context(context: Any):
    global _compile_context
    prev_context = _compile_context
    _compile_context = context
    try:
        yield
    finally:
        _compile_context = prev_context