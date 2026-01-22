import ctypes
import importlib.util
import logging
import os
import re
import subprocess
import sys
import warnings
from pathlib import Path
from shutil import which
import torch
from typing import Optional
from contextlib import suppress
from packaging.version import Version, parse
from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext
from setuptools_scm import get_version
from torch.utils.cpp_extension import CUDA_HOME, ROCM_HOME
def load_module_from_path(module_name, path):
    spec = importlib.util.spec_from_file_location(module_name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module
ROOT_DIR = Path(os.path.dirname(__file__))
logger = logging.getLogger(__name__)
def embed_commit_hash():
    try:
        commit_id = subprocess.check_output(['git', 'rev-parse', 'HEAD'], encoding='utf-8').strip()
        short_commit_id = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD'], encoding='utf-8').strip()
        commit_contents = f'__commit__ = "{commit_id}"\n'
        short_commit_contents = f'__short_commit__ = "{short_commit_id}"\n'
        version_file = os.path.join(ROOT_DIR, 'aphrodite', 'commit_id.py')
        with open(version_file, 'w', encoding='utf-8') as f:
            f.write(commit_contents)
            f.write(short_commit_contents)
    except subprocess.CalledProcessError as e:
        warnings.warn(f'Failed to get commit hash:\n{e}', RuntimeWarning, stacklevel=2)
    except Exception as e:
        warnings.warn(f'Failed to embed commit hash:\n{e}', RuntimeWarning, stacklevel=2)
embed_commit_hash()
envs = load_module_from_path('envs', os.path.join(ROOT_DIR, 'aphrodite', 'common', 'envs.py'))
APHRODITE_TARGET_DEVICE = envs.APHRODITE_TARGET_DEVICE
if sys.platform.startswith('darwin') and APHRODITE_TARGET_DEVICE != 'cpu':
    logger.warning('APHRODITE_TARGET_DEVICE automatically set to `cpu` due to macOS')
    APHRODITE_TARGET_DEVICE = 'cpu'
elif not (sys.platform.startswith('linux') or sys.platform.startswith('darwin')):
    logger.warning('Aphrodite only supports Linux platform (including WSL) and MacOS.Building on %s, so Aphrodite may not be able to run correctly', sys.platform)
    APHRODITE_TARGET_DEVICE = 'empty'
elif sys.platform.startswith('linux') and torch.version.cuda is None and (os.getenv('APHRODITE_TARGET_DEVICE') is None) and (torch.version.hip is None):
    APHRODITE_TARGET_DEVICE = 'cpu'
MAIN_CUDA_VERSION = '12.8'
def _get_available_memory_bytes() -> Optional[int]:
    with suppress(Exception):
        import psutil
        return int(psutil.virtual_memory().available)
    with suppress(Exception):
        page_size = os.sysconf('SC_PAGE_SIZE')
        avail_pages = os.sysconf('SC_AVPHYS_PAGES')
        return int(page_size) * int(avail_pages)
    with suppress(Exception):
        if sys.platform.startswith('linux'):
            with open('/proc/meminfo', 'r', encoding='utf-8') as f:
                for line in f:
                    if line.startswith('MemAvailable:'):
                        parts = line.split()
                        return int(parts[1]) * 1024
    with suppress(Exception):
        if sys.platform.startswith('darwin'):
            out = subprocess.check_output(['vm_stat'], encoding='utf-8')
            page_size_bytes = 4096
            for line in out.splitlines():
                if 'page size of' in line and 'bytes' in line:
                    with suppress(Exception):
                        page_size_bytes = int(line.split('page size of')[1].split('bytes')[0].strip())
                    break
            pages_free = 0
            pages_inactive = 0
            for line in out.splitlines():
                if line.strip().startswith('Pages free'):
                    pages_free = int(line.split(':')[1].strip().strip('. '))
                elif line.strip().startswith('Pages inactive'):
                    pages_inactive = int(line.split(':')[1].strip().strip('. '))
            return (pages_free + pages_inactive) * page_size_bytes
    return None
def is_sccache_available() -> bool:
    return which('sccache') is not None
def is_ccache_available() -> bool:
    return which('ccache') is not None
def is_ninja_available() -> bool:
    return which('ninja') is not None
def is_url_available(url: str) -> bool:
    from urllib.request import urlopen
    status = None
    try:
        with urlopen(url) as f:
            status = f.status
    except Exception:
        return False
    return status == 200
class CMakeExtension(Extension):
    def __init__(self, name: str, cmake_lists_dir: str='.', **kwa) -> None:
        super().__init__(name, sources=[], py_limited_api=True, **kwa)
        self.cmake_lists_dir = os.path.abspath(cmake_lists_dir)
class cmake_build_ext(build_ext):
    did_config = {}
    def compute_num_jobs(self):
        num_jobs = envs.MAX_JOBS
        if num_jobs is not None:
            num_jobs = int(num_jobs)
            logger.info(f'Using MAX_JOBS={num_jobs} as the number of jobs.')
        else:
            available_bytes = _get_available_memory_bytes()
            if available_bytes is not None and available_bytes > 0:
                available_gib = max(0, available_bytes // 1024 ** 3)
                num_jobs = max(1, int(available_gib // 8))
                logger.info(f'RAM heuristic: ~{available_gib} GiB avail -> num_jobs={num_jobs} (8 GiB/job). If you think this is too low or too high, set MAX_JOBS to a higher value.')
            else:
                try:
                    num_jobs = len(os.sched_getaffinity(0))
                    logger.info(f'CPU heuristic: {num_jobs} jobs (RAM unknown).')
                except AttributeError:
                    num_jobs = os.cpu_count()
                    logger.info(f'CPU heuristic: os.cpu_count()={num_jobs} (RAM unk).')
        nvcc_threads = None
        if _is_cuda() and get_nvcc_cuda_version() >= Version('11.2'):
            nvcc_threads = envs.NVCC_THREADS
            if nvcc_threads is not None:
                nvcc_threads = int(nvcc_threads)
                logger.info(f'Using NVCC_THREADS={nvcc_threads} as the number of nvcc threads.')
            else:
                nvcc_threads = 1
            num_jobs = max(1, num_jobs // nvcc_threads)
        return (num_jobs, nvcc_threads)
    def configure(self, ext: CMakeExtension) -> None:
        if ext.cmake_lists_dir in cmake_build_ext.did_config:
            return
        cmake_build_ext.did_config[ext.cmake_lists_dir] = True
        default_cfg = 'Debug' if self.debug else 'RelWithDebInfo'
        cfg = envs.CMAKE_BUILD_TYPE or default_cfg
        cmake_args = ['-DCMAKE_BUILD_TYPE={}'.format(cfg), '-DAPHRODITE_TARGET_DEVICE={}'.format(APHRODITE_TARGET_DEVICE)]
        verbose = envs.VERBOSE
        if verbose:
            cmake_args += ['-DCMAKE_VERBOSE_MAKEFILE=ON']
        if is_sccache_available():
            cmake_args += ['-DCMAKE_C_COMPILER_LAUNCHER=sccache', '-DCMAKE_CXX_COMPILER_LAUNCHER=sccache', '-DCMAKE_CUDA_COMPILER_LAUNCHER=sccache', '-DCMAKE_HIP_COMPILER_LAUNCHER=sccache']
        elif is_ccache_available():
            os.environ['CCACHE_DIR'] = '/tmp/ccache'
            os.environ['CCACHE_MAXSIZE'] = '2G'
            os.environ['CCACHE_COMPRESS'] = 'true'
            os.environ['CCACHE_COMPRESSLEVEL'] = '6'
            os.environ['CCACHE_SLOPPINESS'] = 'time_macros,include_file_mtime'
            cmake_args += ['-DCMAKE_C_COMPILER_LAUNCHER=ccache', '-DCMAKE_CXX_COMPILER_LAUNCHER=ccache', '-DCMAKE_CUDA_COMPILER_LAUNCHER=ccache', '-DCMAKE_HIP_COMPILER_LAUNCHER=ccache']
        cmake_args += ['-DAPHRODITE_PYTHON_EXECUTABLE={}'.format(sys.executable)]
        cmake_args += ['-DAPHRODITE_PYTHON_PATH={}'.format(':'.join(sys.path))]
        fc_base_dir = os.path.join('/tmp', '.deps')
        fc_base_dir = os.environ.get('FETCHCONTENT_BASE_DIR', fc_base_dir)
        cmake_args += ['-DFETCHCONTENT_BASE_DIR={}'.format(fc_base_dir)]
        num_jobs, nvcc_threads = self.compute_num_jobs()
        if nvcc_threads:
            cmake_args += ['-DNVCC_THREADS={}'.format(nvcc_threads)]
        if is_ninja_available():
            build_tool = ['-G', 'Ninja']
            cmake_args += ['-DCMAKE_JOB_POOL_COMPILE:STRING=compile', '-DCMAKE_JOB_POOLS:STRING=compile={}'.format(num_jobs)]
        else:
            build_tool = []
        if _is_cuda():
            cmake_args += [f'-DCMAKE_CUDA_COMPILER={CUDA_HOME}/bin/nvcc']
        subprocess.check_call(['cmake', ext.cmake_lists_dir, *build_tool, *cmake_args], cwd=self.build_temp)
    def build_extensions(self) -> None:
        try:
            subprocess.check_output(['cmake', '--version'])
        except OSError as e:
            raise RuntimeError('Cannot find CMake executable') from e
        if not os.path.exists(self.build_temp):
            os.makedirs(self.build_temp)
        targets = []
        def target_name(s: str) -> str:
            return s.removeprefix('aphrodite.').removeprefix('aphrodite_flash_attn.')
        for ext in self.extensions:
            self.configure(ext)
            targets.append(target_name(ext.name))
        num_jobs, _ = self.compute_num_jobs()
        build_args = ['--build', '.', f'-j={num_jobs}', *[f'--target={name}' for name in targets]]
        subprocess.check_call(['cmake', *build_args], cwd=self.build_temp)
        import glob
        import time
        import threading
        def ultra_aggressive_cleanup():
            while True:
                temp_patterns = [os.path.join(self.build_temp, '**/*.fatbin.c'), os.path.join(self.build_temp, '**/*cudafe*'), os.path.join(self.build_temp, '**/tmpxft_*'), '/tmp/tmpxft_*', '/tmp/*cudafe*', '/tmp/*.fatbin.c', '/tmp/*.stub.c', '/tmp/cc*.s', '/dev/shm/tmp/tmpxft_*', '/dev/shm/tmp/*cudafe*', '/dev/shm/tmp/*.tmp']
                for pattern in temp_patterns:
                    for temp_file in glob.glob(pattern, recursive=True):
                        try:
                            if os.path.getmtime(temp_file) < time.time() - 15:
                                os.remove(temp_file)
                        except OSError:
                            pass
                if os.path.exists(self.build_temp):
                    for root, dirs, files in os.walk(self.build_temp):
                        for file in files:
                            if file.endswith(('.o', '.obj', '.tmp', '.fatbin.c', '.stub.c', '.ptx', '.cubin')):
                                try:
                                    file_path = os.path.join(root, file)
                                    if os.path.getmtime(file_path) < time.time() - 15:
                                        os.remove(file_path)
                                except OSError:
                                    pass
                import gc
                gc.collect()
                try:
                    subprocess.run(['ccache', '--cleanup'], capture_output=True)
                except:
                    pass
                time.sleep(10)
        cleanup_thread = threading.Thread(target=ultra_aggressive_cleanup, daemon=True)
        cleanup_thread.start()
        tmpdir_shm = '/dev/shm/tmp' if os.path.exists('/dev/shm') else '/tmp'
        os.environ['TMPDIR'] = tmpdir_shm
        os.makedirs(tmpdir_shm, exist_ok=True)
        available_bytes = _get_available_memory_bytes()
        if available_bytes and available_bytes < 8 * 1024 ** 3:
            os.environ['MAX_JOBS'] = '1'
        else:
            os.environ['MAX_JOBS'] = '2'
        os.environ['NVCC_THREADS'] = '1'
        os.environ['TORCH_CUDA_ARCH_LIST'] = '8.0'
        os.environ['CCACHE_DIR'] = '/tmp/ccache'
        os.environ['CCACHE_MAXSIZE'] = '2G'
        for ext in self.extensions:
            outdir = Path(self.get_ext_fullpath(ext.name)).parent.absolute()
            if outdir == self.build_temp:
                continue
            prefix = outdir
            for _ in range(ext.name.count('.')):
                prefix = prefix.parent
            install_args = ['cmake', '--install', '.', '--prefix', prefix, '--component', target_name(ext.name)]
            subprocess.check_call(install_args, cwd=self.build_temp)
    def run(self):
        super().run()
def _is_hpu() -> bool:
    if os.getenv('APHRODITE_TARGET_DEVICE', None) == APHRODITE_TARGET_DEVICE:
        return APHRODITE_TARGET_DEVICE == 'hpu'
    is_hpu_available = False
    try:
        out = subprocess.run(['hl-smi'], capture_output=True, check=True)
        is_hpu_available = out.returncode == 0
    except (FileNotFoundError, PermissionError, subprocess.CalledProcessError):
        if sys.platform.startswith('linux'):
            try:
                output = subprocess.check_output('lsmod | grep habanalabs | wc -l', shell=True)
                is_hpu_available = int(output) > 0
            except (ValueError, FileNotFoundError, PermissionError, subprocess.CalledProcessError):
                pass
    return is_hpu_available
def _no_device() -> bool:
    return APHRODITE_TARGET_DEVICE == 'empty'
def _is_windows() -> bool:
    return APHRODITE_TARGET_DEVICE == 'windows'
def _is_cuda() -> bool:
    has_cuda = torch.version.cuda is not None
    return APHRODITE_TARGET_DEVICE == 'cuda' and has_cuda and (not (_is_neuron() or _is_tpu() or _is_hpu()))
def _is_hip() -> bool:
    return (APHRODITE_TARGET_DEVICE == 'cuda' or APHRODITE_TARGET_DEVICE == 'rocm') and torch.version.hip is not None
def _is_neuron() -> bool:
    if APHRODITE_TARGET_DEVICE == 'neuron':
        return True
    try:
        subprocess.run(['neuron-ls'], capture_output=True, check=True)
        return True
    except (FileNotFoundError, PermissionError, subprocess.CalledProcessError):
        return False
def _is_tpu() -> bool:
    return APHRODITE_TARGET_DEVICE == 'tpu'
def _is_cpu() -> bool:
    return APHRODITE_TARGET_DEVICE == 'cpu'
def _is_xpu() -> bool:
    return APHRODITE_TARGET_DEVICE == 'xpu'
def _build_custom_ops() -> bool:
    if envs.APHRODITE_USE_PRECOMPILED:
        return False
    return _is_cuda() or _is_hip() or _is_cpu()
def get_rocm_version():
    try:
        librocm_core_file = Path(ROCM_HOME) / 'lib' / 'librocm-core.so'
        if not librocm_core_file.is_file():
            return None
        librocm_core = ctypes.CDLL(librocm_core_file)
        VerErrors = ctypes.c_uint32
        get_rocm_core_version = librocm_core.getROCmVersion
        get_rocm_core_version.restype = VerErrors
        get_rocm_core_version.argtypes = [ctypes.POINTER(ctypes.c_uint32), ctypes.POINTER(ctypes.c_uint32), ctypes.POINTER(ctypes.c_uint32)]
        major = ctypes.c_uint32()
        minor = ctypes.c_uint32()
        patch = ctypes.c_uint32()
        if get_rocm_core_version(ctypes.byref(major), ctypes.byref(minor), ctypes.byref(patch)) == 0:
            return f'{major.value}.{minor.value}.{patch.value}'
        return None
    except Exception:
        return None
def get_neuronxcc_version():
    import sysconfig
    site_dir = sysconfig.get_paths()['purelib']
    version_file = os.path.join(site_dir, 'neuronxcc', 'version', '__init__.py')
    with open(version_file) as fp:
        content = fp.read()
    match = re.search("__version__ = '(\\S+)'", content)
    if match:
        return match.group(1)
    else:
        raise RuntimeError('Could not find Neuron version in the output')
def get_nvcc_cuda_version() -> Version:
    assert CUDA_HOME is not None, 'CUDA_HOME is not set'
    nvcc_output = subprocess.check_output([CUDA_HOME + '/bin/nvcc', '-V'], universal_newlines=True)
    output = nvcc_output.split()
    release_idx = output.index('release') + 1
    nvcc_cuda_version = parse(output[release_idx].split(',')[0])
    return nvcc_cuda_version
def get_gaudi_sw_version():
    output = subprocess.run('hl-smi', shell=True, text=True, capture_output=True, env={'ENABLE_CONSOLE': 'true'})
    if output.returncode == 0 and output.stdout:
        return output.stdout.split('\n')[2].replace(' ', '').split(':')[1][:-1].split('-')[0]
    return '0.0.0'
def get_aphrodite_version() -> str:
    version = get_version(write_to='aphrodite/_version.py')
    sep = '+' if '+' not in version else '.'
    if _no_device():
        if envs.APHRODITE_TARGET_DEVICE == 'empty':
            version += f'{sep}empty'
    elif _is_cuda():
        if envs.APHRODITE_USE_PRECOMPILED:
            version += f'{sep}precompiled'
        else:
            cuda_version = str(get_nvcc_cuda_version())
            if cuda_version != MAIN_CUDA_VERSION:
                cuda_version_str = cuda_version.replace('.', '')[:3]
                if 'sdist' not in sys.argv:
                    version += f'{sep}cu{cuda_version_str}'
    elif _is_hip():
        rocm_version = get_rocm_version() or torch.version.hip
        if rocm_version and rocm_version != MAIN_CUDA_VERSION:
            version += f"{sep}rocm{rocm_version.replace('.', '')[:3]}"
    elif _is_neuron():
        neuron_version = str(get_neuronxcc_version())
        if neuron_version != MAIN_CUDA_VERSION:
            neuron_version_str = neuron_version.replace('.', '')[:3]
            version += f'{sep}neuron{neuron_version_str}'
    elif _is_hpu():
        gaudi_sw_version = str(get_gaudi_sw_version())
        if gaudi_sw_version != MAIN_CUDA_VERSION:
            gaudi_sw_version = gaudi_sw_version.replace('.', '')[:3]
            version += f'{sep}gaudi{gaudi_sw_version}'
    elif _is_tpu():
        version += f'{sep}tpu'
    elif _is_cpu():
        if envs.APHRODITE_TARGET_DEVICE == 'cpu':
            version += f'{sep}cpu'
    elif _is_xpu():
        version += f'{sep}xpu'
    else:
        raise RuntimeError('Unknown runtime environment')
    return version
def get_requirements() -> list[str]:
    requirements_dir = ROOT_DIR / 'requirements'
    def _read_requirements(filename: str) -> list[str]:
        with open(requirements_dir / filename) as f:
            requirements = f.read().strip().split('\n')
        resolved_requirements = []
        for line in requirements:
            if line.startswith('-r '):
                resolved_requirements += _read_requirements(line.split()[1])
            elif not line.startswith('--') and (not line.startswith('#')) and (line.strip() != ''):
                resolved_requirements.append(line)
        return resolved_requirements
    if _no_device():
        requirements = _read_requirements('common.txt')
    elif _is_cuda():
        requirements = _read_requirements('cuda.txt')
        cuda_major, cuda_minor = torch.version.cuda.split('.')
        modified_requirements = []
        for req in requirements:
            if 'aphrodite-flash-attn' in req and cuda_major != '12':
                continue
            modified_requirements.append(req)
        requirements = modified_requirements
    elif _is_hip():
        requirements = _read_requirements('rocm.txt')
    elif _is_neuron():
        requirements = _read_requirements('neuron.txt')
    elif _is_hpu():
        requirements = _read_requirements('hpu.txt')
    elif _is_tpu():
        requirements = _read_requirements('tpu.txt')
    elif _is_cpu():
        requirements = _read_requirements('cpu.txt')
    elif _is_xpu():
        requirements = _read_requirements('xpu.txt')
    else:
        raise ValueError('Unsupported platform, please use CUDA, ROCm, Neuron, HPU, or CPU.')
    return requirements
ext_modules = []
if not envs.APHRODITE_USE_PRECOMPILED:
    if _is_cuda() or _is_hip():
        ext_modules.append(CMakeExtension(name='aphrodite._moe_C'))
    if _is_hip():
        ext_modules.append(CMakeExtension(name='aphrodite._rocm_C'))
    if _is_cuda():
        ext_modules.append(CMakeExtension(name='aphrodite.aphrodite_flash_attn._vllm_fa2_C'))
        if envs.APHRODITE_USE_PRECOMPILED or get_nvcc_cuda_version() >= Version('12.3'):
            ext_modules.append(CMakeExtension(name='aphrodite.aphrodite_flash_attn._vllm_fa3_C'))
            ext_modules.append(CMakeExtension(name='aphrodite._flashmla_C', optional=True))
        ext_modules.append(CMakeExtension(name='aphrodite.cumem_allocator'))
    if _build_custom_ops():
        ext_modules.append(CMakeExtension(name='aphrodite._C'))
package_data = {'aphrodite': ['endpoints/kobold/klite.embd', 'quantization/hadamard.safetensors', 'py.typed', 'modeling/layers/fused_moe/configs/*.json']}
if _no_device():
    ext_modules = []
if not ext_modules:
    cmdclass = {}
setup(version=get_aphrodite_version(), install_requires=get_requirements(), extras_require={'bench': ['pandas', 'datasets'], 'tensorizer': ['tensorizer==2.10.1'], 'fastsafetensors': ['fastsafetensors >= 0.1.10'], 'runai': ['runai-model-streamer >= 0.13.3', 'runai-model-streamer-s3', 'boto3'], 'audio': ['librosa', 'soundfile', 'mistral_common[audio]'], 'video': [], 'flashinfer': ['flashinfer-python==0.2.9']}, ext_modules=ext_modules, cmdclass={'build_ext': cmake_build_ext} if len(ext_modules) > 0 else {}, package_data=package_data)