"""
Test for CMake CPU build fix (Issue #76).

This test ensures that CPU builds work correctly even when PyTorch has CUDA 
support but CUDA libraries are not available on the system.
"""

import subprocess
import sys
import shutil
from pathlib import Path
import pytest
import torch


class TestCMakeCPUBuild:
    """Test CMake CPU build functionality."""
    
    def test_cpu_build_with_cuda_pytorch(self):
        """
        Test that CPU builds work when PyTorch has CUDA support but CUDA is not available.
        
        This is a regression test for Issue #76 where CMake would fail with:
        "Your installed Caffe2 version uses CUDA but I cannot find the CUDA libraries"
        """
        
        # Skip if PyTorch doesn't have CUDA support (nothing to test)
        if not torch.version.cuda:
            pytest.skip("PyTorch was not compiled with CUDA support")
            
        # Skip if CUDA is actually available (test is for CUDA-unavailable systems)
        if torch.cuda.is_available():
            pytest.skip("CUDA is available, test is for CUDA-unavailable systems")
        
        repo_root = Path(__file__).parent.parent
        build_dir = repo_root / "build_test_cpu"
        
        # Clean any existing build directory
        if build_dir.exists():
            shutil.rmtree(build_dir)
            
        try:
            # Test CMake configuration for CPU build
            cmake_cmd = [
                "cmake",
                "-DAPHRODITE_TARGET_DEVICE=cpu", 
                f"-DAPHRODITE_PYTHON_EXECUTABLE={sys.executable}",
                "-S", str(repo_root),
                "-B", str(build_dir)
            ]
            
            result = subprocess.run(cmake_cmd, capture_output=True, text=True)
            
            # Should succeed without the CUDA error
            assert result.returncode == 0, f"CMake configuration failed: {result.stderr}"
            assert "Configuring done" in result.stdout, "CMake configuration did not complete"
            assert "Your installed Caffe2 version uses CUDA but I cannot find the CUDA libraries" not in result.stderr, "CMake CUDA error still occurs"
            
            # Should use our bypass logic
            assert "Getting PyTorch library paths for CPU build" in result.stdout, "CPU bypass logic was not used"
            
            # Test building the CPU extension
            build_cmd = ["cmake", "--build", str(build_dir), "--target", "_C"]
            result = subprocess.run(build_cmd, capture_output=True, text=True)
            
            assert result.returncode == 0, f"CPU extension build failed: {result.stderr}"
            assert "Built target _C" in result.stdout, "CPU extension was not built successfully"
            
        finally:
            # Clean up
            if build_dir.exists():
                shutil.rmtree(build_dir)