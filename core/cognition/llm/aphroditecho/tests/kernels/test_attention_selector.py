from unittest.mock import patch
import pytest
import torch
from aphrodite.attention.selector import which_attn_to_use
from aphrodite.common.utils import STR_FLASH_ATTN_VAL, STR_INVALID_VAL
from tests.kernels.utils import override_backend_env_variable
@pytest.mark.parametrize('name', ['TORCH_SDPA', 'TRITON_FLASH', 'XFORMERS', 'FLASHINFER', 'OPENVINO'])
@pytest.mark.parametrize('device', ['cpu', 'openvino', 'hip', 'cuda'])
def test_env(name: str, device: str, monkeypatch):
    override_backend_env_variable(monkeypatch, name)
    if device == 'cpu':
        with patch('aphrodite.attention.selector.is_cpu', return_value=True):
            backend = which_attn_to_use(8, 16, 8, None, torch.float16, torch.float16, 16)
        assert backend.name == 'TORCH_SDPA'
    elif device == 'hip':
        with patch('aphrodite.attention.selector.is_hip', return_value=True):
            backend = which_attn_to_use(8, 16, 8, None, torch.float16, torch.float16, 16)
        assert backend.name == 'TRITON_FLASH'
    elif device == 'openvino':
        with patch('aphrodite.attention.selector.is_openvino', return_value=True):
            backend = which_attn_to_use(8, 16, 8, None, torch.float16, torch.float16, 16)
        assert backend.name == 'OPENVINO'
    else:
        backend = which_attn_to_use(8, 16, 8, None, torch.float16, torch.float16, 16)
        assert backend.name == name
def test_flash_attn(monkeypatch):
    override_backend_env_variable(monkeypatch, STR_FLASH_ATTN_VAL)
    with patch('torch.cuda.get_device_capability', return_value=[7, 5]):
        backend = which_attn_to_use(8, 16, 8, None, torch.float16, None, 16)
        assert backend.name != STR_FLASH_ATTN_VAL
    backend = which_attn_to_use(8, 16, 8, None, torch.float8_e4m3fn, None, 16)
    assert backend.name != STR_FLASH_ATTN_VAL
    backend = which_attn_to_use(8, 16, 8, None, torch.float16, 'fp8', 16)
    assert backend.name != STR_FLASH_ATTN_VAL
    backend = which_attn_to_use(8, 16, 8, None, torch.float16, None, 8)
    assert backend.name != STR_FLASH_ATTN_VAL
    backend = which_attn_to_use(8, 16, 8, 1, torch.float16, None, 16)
    assert backend.name != STR_FLASH_ATTN_VAL
    with patch.dict('sys.modules', {'aphrodite_flash_attn': None}):
        backend = which_attn_to_use(8, 16, 8, None, torch.float16, None, 16)
        assert backend.name != STR_FLASH_ATTN_VAL
    backend = which_attn_to_use(8, 17, 8, None, torch.float16, None, 16)
    assert backend.name != STR_FLASH_ATTN_VAL
def test_invalid_env(monkeypatch):
    override_backend_env_variable(monkeypatch, STR_INVALID_VAL)
    with pytest.raises(ValueError):
        which_attn_to_use(8, 16, 8, None, torch.float16, None, 16)