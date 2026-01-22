try:
    from ggml.cffi import ffi as ffi
except ImportError as e:
    raise ImportError(f"Couldn't find ggml bindings ({e}). Run `python regenerate.py` or check your PYTHONPATH.")
import os, platform
__exact_library = os.environ.get('GGML_LIBRARY')
if __exact_library:
    __candidates = [__exact_library]
elif platform.system() == 'Windows':
    __candidates = ['ggml_shared.dll', 'llama.dll']
else:
    __candidates = ['libggml_shared.so', 'libllama.so']
    if platform.system() == 'Darwin':
        __candidates += ['libggml_shared.dylib', 'libllama.dylib']
for i, name in enumerate(__candidates):
    try:
        lib = ffi.dlopen(name)
    except OSError:
        if i < len(__candidates) - 1:
            continue
        raise OSError(f"Couldn't find ggml's shared library (tried names: {__candidates}). Add its directory to DYLD_LIBRARY_PATH (on Mac) or LD_LIBRARY_PATH, or define GGML_LIBRARY.")
ffi = ffi