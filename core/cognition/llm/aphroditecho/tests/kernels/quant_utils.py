from typing import Optional, Union
import torch
from aphrodite.platforms import current_platform
ROCM_FP8_MAX = 224.0
FP8_DTYPE = current_platform.fp8_dtype()
def as_float32_tensor(x: Union[float, torch.tensor]) -> torch.tensor:
    return torch.as_tensor(x, dtype=torch.float32, device='cuda')
def ref_dynamic_per_token_quant(x: torch.tensor, quant_dtype: torch.dtype, scale_ub: Optional[torch.tensor]=None) -> tuple[torch.tensor, torch.tensor]:
    assert quant_dtype in [torch.int8, FP8_DTYPE]
    if scale_ub is not None:
        assert quant_dtype == FP8_DTYPE
    qtype_traits = torch.iinfo(quant_dtype) if quant_dtype == torch.int8 else torch.finfo(quant_dtype)
    qtype_traits_max = ROCM_FP8_MAX if current_platform.is_rocm() else qtype_traits.max
    qtype_traits_min = -ROCM_FP8_MAX if current_platform.is_rocm() else qtype_traits.min
    qtype_max = as_float32_tensor(qtype_traits_max)
    s_1 = as_float32_tensor(1.0)
    s_512 = as_float32_tensor(512.0)
    x_token_max, _ = x.abs().max(dim=-1)
    x_token_max = as_float32_tensor(x_token_max)
    if scale_ub is not None:
        x_token_max = x_token_max.clamp(max=scale_ub)
    scales = (x_token_max / qtype_max)[:, None]
    if quant_dtype == torch.int8:
        iscales = as_float32_tensor(s_1 / scales)
        torch_out = as_float32_tensor(x) * iscales
        torch_out = torch_out.round()
        torch_out = torch_out.clamp(qtype_traits_min, qtype_traits_max).to(quant_dtype)
    else:
        assert quant_dtype == FP8_DTYPE
        min_scaling_factor = s_1 / (qtype_max * s_512)
        scales = scales.clamp(min=min_scaling_factor)
        torch_out = as_float32_tensor(x) / scales
        torch_out = torch_out.clamp(qtype_traits_min, qtype_traits_max).to(quant_dtype)
    return (torch_out, scales)
def ref_dynamic_per_tensor_fp8_quant(x: torch.tensor) -> tuple[torch.tensor, torch.tensor]:
    fp8_traits = torch.finfo(FP8_DTYPE)
    fp8_traits_max = ROCM_FP8_MAX if current_platform.is_rocm() else fp8_traits.max
    fp8_traits_min = -ROCM_FP8_MAX if current_platform.is_rocm() else fp8_traits.min
    fp8_max = as_float32_tensor(fp8_traits_max)
    one = as_float32_tensor(1.0)
    x_max = as_float32_tensor(x.abs().max())
    ref_scale = x_max / fp8_max
    ref_iscale = one / ref_scale
    ref_out = (as_float32_tensor(x) * ref_iscale).clamp(fp8_traits_min, fp8_traits_max).to(FP8_DTYPE)
    return (ref_out, ref_scale.view((1,)))
def native_w8a8_block_matmul(A: torch.Tensor, B: torch.Tensor, As: torch.Tensor, Bs: torch.Tensor, block_size, output_dtype):
    A = A.to(torch.float32)
    B = B.to(torch.float32)
    assert A.shape[-1] == B.shape[-1]
    assert B.ndim == 2 and B.is_contiguous() and (Bs.ndim == 2)
    assert len(block_size) == 2
    block_n, block_k = (block_size[0], block_size[1])
    assert (A.shape[-1] + block_k - 1) // block_k == As.shape[-1]
    assert A.shape[:-1] == As.shape[:-1]
    M = A.numel() // A.shape[-1]
    N, K = B.shape
    origin_C_shape = A.shape[:-1] + (N,)
    A = A.reshape(M, A.shape[-1])
    As = As.reshape(M, As.shape[-1])
    n_tiles = (N + block_n - 1) // block_n
    k_tiles = (K + block_k - 1) // block_k
    assert n_tiles == Bs.shape[0]
    assert k_tiles == Bs.shape[1]
    C_shape = (M, N)
    C = torch.zeros(C_shape, dtype=torch.float32, device=A.device)
    A_tiles = [A[:, i * block_k:min((i + 1) * block_k, K)] for i in range(k_tiles)]
    B_tiles = [[B[j * block_n:min((j + 1) * block_n, N), i * block_k:min((i + 1) * block_k, K)] for i in range(k_tiles)] for j in range(n_tiles)]
    C_tiles = [C[:, j * block_n:min((j + 1) * block_n, N)] for j in range(n_tiles)]
    As_tiles = [As[:, i:i + 1] for i in range(k_tiles)]
    for i in range(k_tiles):
        for j in range(n_tiles):
            a = A_tiles[i]
            b = B_tiles[j][i]
            c = C_tiles[j]
            s = As_tiles[i] * Bs[j][i]
            c[:, :] += torch.matmul(a, b.t()) * s
    C = C.reshape(origin_C_shape).to(output_dtype)
    return C