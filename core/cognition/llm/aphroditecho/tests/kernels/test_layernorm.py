import pytest
import torch
from aphrodite.modeling.layers.layernorm import RMSNorm
from tests.kernels.utils import opcheck
DTYPES = [torch.half, torch.bfloat16, torch.float]
NUM_TOKENS = [7, 83, 4096]
HIDDEN_SIZES = [768, 769, 770, 771, 5120, 5124, 5125, 5126, 8192, 8199]
ADD_RESIDUAL = [False, True]
SEEDS = [0]
CUDA_DEVICES = [f'cuda:{i}' for i in range(1 if torch.cuda.device_count() == 1 else 2)]
@pytest.mark.parametrize('num_tokens', NUM_TOKENS)
@pytest.mark.parametrize('hidden_size', HIDDEN_SIZES)
@pytest.mark.parametrize('add_residual', ADD_RESIDUAL)
@pytest.mark.parametrize('dtype', DTYPES)
@pytest.mark.parametrize('seed', SEEDS)
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_rms_norm(num_tokens: int, hidden_size: int, add_residual: bool, dtype: torch.dtype, seed: int, device: str) -> None:
    torch.random.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
    torch.set_default_device(device)
    layer = RMSNorm(hidden_size).to(dtype=dtype)
    layer.weight.data.normal_(mean=1.0, std=0.1)
    scale = 1 / (2 * hidden_size)
    x = torch.randn(num_tokens, hidden_size, dtype=dtype)
    x *= scale
    residual = torch.randn_like(x) * scale if add_residual else None
    ref_out = layer.forward_native(x, residual)
    out = layer(x, residual)
    if add_residual:
        torch.testing.assert_close(out[0], ref_out[0], atol=0.01, rtol=0.01)
        torch.testing.assert_close(out[1], ref_out[1], atol=0.01, rtol=0.01)
    else:
        torch.testing.assert_close(out, ref_out, atol=0.01, rtol=0.01)
    if residual is not None:
        opcheck(torch.ops._C.fused_add_rms_norm, (x, residual, layer.weight.data, layer.variance_epsilon))
    else:
        opcheck(torch.ops._C.rms_norm, (out, x, layer.weight.data, layer.variance_epsilon))
@pytest.mark.parametrize('num_tokens', NUM_TOKENS)
@pytest.mark.parametrize('hidden_size', HIDDEN_SIZES)
@pytest.mark.parametrize('add_residual', ADD_RESIDUAL)
@pytest.mark.parametrize('dtype', DTYPES)
@pytest.mark.parametrize('seed', SEEDS)
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_rms_norm_triton(num_tokens: int, hidden_size: int, add_residual: bool, dtype: torch.dtype, seed: int, device: str) -> None:
    torch.random.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
    layer = RMSNorm(hidden_size).to(device=device, dtype=dtype)
    layer.weight.data.normal_(mean=1.0, std=0.1)
    scale = 1 / (2 * hidden_size)
    x = torch.randn(num_tokens, hidden_size, dtype=dtype, device=device) * scale
    residual = torch.randn_like(x) * scale if add_residual else None
    ref_out = layer.forward_native(x, residual)
    triton_out = layer.forward_triton(x, residual)
    if add_residual:
        torch.testing.assert_close(triton_out[0], ref_out[0], atol=0.01, rtol=0.01)
        torch.testing.assert_close(triton_out[1], ref_out[1], atol=0.01, rtol=0.01)
    else:
        torch.testing.assert_close(triton_out, ref_out, atol=0.01, rtol=0.01)