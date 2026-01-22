from unittest.mock import patch
import pytest
import torch
from aphrodite.attention.selector import _cached_get_attn_backend, get_attn_backend
from aphrodite.platforms.cpu import CpuPlatform
from aphrodite.platforms.cuda import CudaPlatform
from aphrodite.platforms.rocm import RocmPlatform
from aphrodite.common.utils import STR_BACKEND_ENV_VAR, STR_FLASH_ATTN_VAL, STR_INVALID_VAL
@pytest.fixture(autouse=True)
def clear_cache():
    _cached_get_attn_backend.cache_clear()
DEVICE_MLA_BACKENDS = {'cuda': ['TRITON_MLA', 'FLASHMLA'], 'hip': ['TRITON_MLA', 'ROCM_AITER_MLA'], 'cpu': []}
DEVICE_REGULAR_ATTN_BACKENDS = {'cuda': ['XFORMERS', 'FLASHINFER'], 'hip': ['ROCM_FLASH'], 'cpu': ['TORCH_SDPA']}
DEVICE_MLA_BLOCK_SIZES = {'cuda': [16, 64], 'hip': [16, 1], 'cpu': [16]}
def generate_params():
    params = []
    for use_mla in [True, False]:
        for device in ['cuda', 'hip', 'cpu']:
            backends = DEVICE_MLA_BACKENDS[device] if use_mla else DEVICE_REGULAR_ATTN_BACKENDS[device]
            for name in backends:
                block_sizes = DEVICE_MLA_BLOCK_SIZES[device] if use_mla else [16]
                for block_size in block_sizes:
                    params.append(pytest.param(device, name, use_mla, block_size, id=f'{device}_{name}_mla_{str(use_mla)[0]}_blks{block_size}'))
    return params
@pytest.mark.parametrize('device, name, use_mla, block_size', generate_params())
@pytest.mark.parametrize('use_v1', [True, False])
def test_env(device: str, name: str, use_mla: bool, block_size: int, use_v1: bool, monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1' if use_v1 else '0')
        m.setenv(STR_BACKEND_ENV_VAR, name)
        m.setenv('APHRODITE_MLA_DISABLE', '1' if use_mla else '0')
        if device == 'cpu':
            with patch('aphrodite.attention.selector.current_platform', CpuPlatform()):
                backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False)
            assert backend.get_name() == 'TORCH_SDPA'
        elif device == 'hip':
            with patch('aphrodite.attention.selector.current_platform', RocmPlatform()):
                if use_mla:
                    valid_combination = name == 'TRITON_MLA' and block_size != 1 or (name == 'ROCM_AITER_MLA' and block_size == 1)
                    if valid_combination:
                        backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                        assert backend.get_name() == name
                    else:
                        with pytest.raises(ValueError) as exc_info:
                            get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                        assert f'The selected backend, {name}' in str(exc_info.value)
                else:
                    backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                    expected = 'TRITON_ATTN_APHRODITE_V1' if use_v1 else 'ROCM_FLASH'
                    assert backend.get_name() == expected
        elif device == 'cuda':
            with patch('aphrodite.attention.selector.current_platform', CudaPlatform()):
                if use_mla:
                    if name == 'FLASHMLA' and block_size == 64:
                        from aphrodite.attention.backends.flashmla import is_flashmla_supported
                        is_supported, _ = is_flashmla_supported()
                        if not is_supported:
                            pytest.skip()
                        else:
                            backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                            expected = f'{name}_APHRODITE_V1' if use_v1 else name
                            assert backend.get_name() == expected
                    else:
                        backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                        expected = 'TRITON_MLA_APHRODITE_V1' if use_v1 else 'TRITON_MLA'
                        assert backend.get_name() == expected
                elif name == 'FLASHINFER':
                    backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                    expected = 'FLASHINFER_APHRODITE_V1' if use_v1 else name
                    assert backend.get_name() == expected
                else:
                    backend = get_attn_backend(16, torch.float16, torch.float16, block_size, False, use_mla=use_mla)
                    expected = 'FLASH_ATTN_APHRODITE_V1' if use_v1 else name
                    assert backend.get_name() == expected
def test_flash_attn(monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, STR_FLASH_ATTN_VAL)
        monkeypatch.setattr(torch.cuda, 'get_device_capability', lambda: (7, 5))
        backend = get_attn_backend(16, torch.float16, None, 16, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        monkeypatch.undo()
        backend = get_attn_backend(16, torch.float8_e4m3fn, None, 16, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        backend = get_attn_backend(16, torch.float16, 'fp8', 16, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        backend = get_attn_backend(16, torch.float16, None, 8, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        import sys
        original_module = sys.modules.get('aphrodite_flash_attn')
        monkeypatch.setitem(sys.modules, 'aphrodite_flash_attn', None)
        backend = get_attn_backend(16, torch.float16, None, 16, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        if original_module is not None:
            monkeypatch.setitem(sys.modules, 'aphrodite_flash_attn', original_module)
        else:
            monkeypatch.delitem(sys.modules, 'aphrodite_flash_attn', raising=False)
        backend = get_attn_backend(17, torch.float16, None, 16, False)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
        backend = get_attn_backend(16, torch.float16, torch.float16, 16, True)
        assert backend.get_name() != STR_FLASH_ATTN_VAL
@pytest.mark.parametrize('use_v1', [True, False])
def test_invalid_env(use_v1: bool, monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m, patch('aphrodite.attention.selector.current_platform', CudaPlatform()):
        m.setenv('APHRODITE_USE_V1', '1' if use_v1 else '0')
        m.setenv(STR_BACKEND_ENV_VAR, STR_INVALID_VAL)
        backend = get_attn_backend(32, torch.float16, None, 16, False)
        EXPECTED = 'FLASH_ATTN_APHRODITE_V1' if use_v1 else 'FLASH_ATTN'
        assert backend.get_name() == EXPECTED
        if use_v1:
            pass
        else:
            backend = get_attn_backend(16, torch.float16, None, 16, False)
            assert backend.get_name() == 'XFORMERS'