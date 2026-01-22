import subprocess
import sys
import shutil
from pathlib import Path
import pytest
import torch
class TestCMakeCPUBuild:
    def test_cpu_build_with_cuda_pytorch(self):
        if not torch.version.cuda:
            pytest.skip('PyTorch was not compiled with CUDA support')
        if torch.cuda.is_available():
            pytest.skip('CUDA is available, test is for CUDA-unavailable systems')
        repo_root = Path(__file__).parent.parent
        build_dir = repo_root / 'build_test_cpu'
        if build_dir.exists():
            shutil.rmtree(build_dir)
        try:
            cmake_cmd = ['cmake', '-DAPHRODITE_TARGET_DEVICE=cpu', f'-DAPHRODITE_PYTHON_EXECUTABLE={sys.executable}', '-S', str(repo_root), '-B', str(build_dir)]
            result = subprocess.run(cmake_cmd, capture_output=True, text=True)
            assert result.returncode == 0, f'CMake configuration failed: {result.stderr}'
            assert 'Configuring done' in result.stdout, 'CMake configuration did not complete'
            assert 'Your installed Caffe2 version uses CUDA but I cannot find the CUDA libraries' not in result.stderr, 'CMake CUDA error still occurs'
            assert 'Getting PyTorch library paths for CPU build' in result.stdout, 'CPU bypass logic was not used'
            build_cmd = ['cmake', '--build', str(build_dir), '--target', '_C']
            result = subprocess.run(build_cmd, capture_output=True, text=True)
            assert result.returncode == 0, f'CPU extension build failed: {result.stderr}'
            assert 'Built target _C' in result.stdout, 'CPU extension was not built successfully'
        finally:
            if build_dir.exists():
                shutil.rmtree(build_dir)