from functools import reduce
from typing import Tuple
import torch
from torch import Tensor
def _n_ones(n: int) -> int:
    return (1 << n) - 1
EBITS_F32, MBITS_F32 = (8, 23)
F32_EXP_BIAS = _n_ones(EBITS_F32 - 1)
_SPLIT_K_MAP = [{3072: 18, 4096: 13, 5120: 10, 6144: 9, 8192: 6, 10240: 5, 14336: 7, 28672: 7, 57344: 7}, {3072: 9, 4096: 6, 5120: 5, 6144: 9, 8192: 3, 10240: 5, 14336: 7, 28672: 7, 57344: 6}, {3072: 6, 4096: 4, 5120: 7, 6144: 3, 8192: 2, 10240: 5, 14336: 5, 28672: 5, 57344: 4}, {3072: 9, 4096: 3, 5120: 5, 6144: 2, 8192: 5, 10240: 4, 14336: 8, 28672: 6, 57344: 4}, {3072: 7, 4096: 5, 5120: 2, 6144: 5, 8192: 4, 10240: 1, 14336: 3, 28672: 3, 57344: 4}, {3072: 3, 4096: 2, 5120: 5, 6144: 3, 8192: 1, 10240: 8, 14336: 3, 28672: 4, 57344: 3}, {3072: 5, 4096: 7, 5120: 3, 6144: 5, 8192: 7, 10240: 3, 14336: 1, 28672: 1, 57344: 3}, {3072: 2, 4096: 5, 5120: 4, 6144: 1, 8192: 5, 10240: 2, 14336: 6, 28672: 4, 57344: 1}, {3072: 2, 4096: 3, 5120: 1, 6144: 1, 8192: 3, 10240: 3, 14336: 3, 28672: 1, 57344: 1}, {3072: 5, 4096: 4, 5120: 1, 6144: 4, 8192: 2, 10240: 1, 14336: 1, 28672: 1, 57344: 1}, {3072: 3, 4096: 1, 5120: 2, 6144: 2, 8192: 1, 10240: 2, 14336: 1, 28672: 1, 57344: 1}, {3072: 3, 4096: 1, 5120: 3, 6144: 2, 8192: 1, 10240: 1, 14336: 1, 28672: 1, 57344: 1}]
def _f32_to_fpx_unpacked(x: Tensor, ebits: int, mbits: int) -> Tensor:
    assert x.dtype == torch.float
    assert 1 + ebits + mbits <= 8
    exp_bias = _n_ones(ebits - 1)
    max_int = _n_ones(ebits + mbits)
    sign_mask = 1 << ebits + mbits
    magic_adder = _n_ones(MBITS_F32 - mbits - 1)
    max_normal = 2 ** (_n_ones(ebits) - exp_bias) * (_n_ones(mbits + 1) / 2 ** mbits)
    min_normal = 2 ** (1 - exp_bias)
    denorm_exp = F32_EXP_BIAS - exp_bias + (MBITS_F32 - mbits) + 1
    denorm_mask_int = denorm_exp << MBITS_F32
    denorm_mask_float = torch.tensor(denorm_mask_int, dtype=torch.int32).view(torch.float32)
    x = x.view(torch.int32)
    sign = x & 2147483648
    x = x ^ sign
    x = x.view(torch.float)
    saturate_mask = x >= max_normal
    denormal_mask = torch.logical_and(torch.logical_not(saturate_mask), x < min_normal)
    normal_mask = torch.logical_not(torch.logical_or(saturate_mask, denormal_mask))
    denormal_x = x + denorm_mask_float
    denormal_x = denormal_x.view(torch.int32)
    denormal_x -= denorm_mask_int
    denormal_x = denormal_x.to(torch.uint8)
    normal_x = x.view(torch.int32)
    mant_odd = normal_x >> MBITS_F32 - mbits & 1
    val_to_add = (exp_bias - F32_EXP_BIAS << MBITS_F32) + magic_adder
    normal_x += val_to_add
    normal_x += mant_odd
    normal_x = normal_x >> MBITS_F32 - mbits
    normal_x = normal_x.to(torch.uint8)
    x = torch.full_like(x, max_int, dtype=torch.uint8)
    x = torch.where(denormal_mask, denormal_x, x)
    x = torch.where(normal_mask, normal_x, x)
    sign_lp = sign >> MBITS_F32 + EBITS_F32 - mbits - ebits
    sign_lp = sign_lp.to(torch.uint8)
    sign_lp = sign_lp & sign_mask
    x = x | sign_lp
    return x.to(torch.uint8)
def _fpx_unpacked_to_f32(x: Tensor, ebits: int, mbits: int) -> Tensor:
    assert x.dtype == torch.uint8
    assert 1 + ebits + mbits <= 8
    sign_mask = 1 << ebits + mbits
    exp_bias = _n_ones(ebits - 1)
    mantissa_mask = _n_ones(mbits)
    sign_lp = x & sign_mask
    x_pos = x ^ sign_lp
    zero_mask = x_pos == 0
    denormal_mask = torch.logical_and(x_pos > 0, x_pos >> mbits == 0)
    exp_biased_lp = x_pos >> mbits
    exp_biased_f32 = exp_biased_lp - exp_bias + F32_EXP_BIAS
    exp_biased_f32 = exp_biased_f32.to(torch.int32) << MBITS_F32
    mantissa_lp_int32 = (x_pos & mantissa_mask).to(torch.int32)
    mantissa_f32 = mantissa_lp_int32 << MBITS_F32 - mbits
    result = exp_biased_f32 | mantissa_f32
    result[zero_mask] = 0
    denormal_exp_biased = 1 - exp_bias + F32_EXP_BIAS
    if mbits == 1:
        result[denormal_mask] = denormal_exp_biased - mbits << MBITS_F32
    else:
        for i in range(mbits):
            for mantissa_cmp in range(1 << i, 1 << i + 1):
                left_shift = mbits - i
                mantissa_f32 = mantissa_cmp - (1 << i) << left_shift + MBITS_F32 - mbits
                exp_biased_f32 = denormal_exp_biased - left_shift << MBITS_F32
                mantissa_lp_int32[mantissa_lp_int32 == mantissa_cmp] = exp_biased_f32 + mantissa_f32
        result = torch.where(denormal_mask, mantissa_lp_int32, result)
    sign_f32 = sign_lp.to(torch.int32) << MBITS_F32 - mbits + EBITS_F32 - ebits
    result = result | sign_f32
    return result.view(torch.float)
def quant_llm_linear(EXPONENT: int, MANTISSA: int, _in_feats: Tensor, _weights: Tensor, _scales: Tensor, splitK: int=1) -> Tensor:
    return torch.ops.torchao.quant_llm_linear.default(EXPONENT, MANTISSA, _in_feats, _weights, _scales, splitK)
_ONES_TABLE = [_n_ones(i) for i in range(8)]
def _pack(x: Tensor, n_bits: int) -> Tensor:
    return reduce(torch.bitwise_or, [x[..., i::8 // n_bits] << 8 - (i + 1) * n_bits for i in range(8 // n_bits)])
def _unpack(x: Tensor, n_bits: int) -> Tensor:
    return torch.stack([x >> 8 - (i + 1) * n_bits & (1 << n_bits) - 1 for i in range(8 // n_bits)], dim=-1).flatten(-2)
def _bit_interleave(x: Tensor, n_bits: int, undo: bool=False) -> Tensor:
    x = x.reshape(-1, 4).flip(1)
    x = _unpack(x, n_bits)
    x = x.view(-1, 4 * (8 // n_bits))
    if not undo:
        bit_order = {1: [1, 5, 9, 13, 17, 21, 25, 29, 3, 7, 11, 15, 19, 23, 27, 31, 0, 4, 8, 12, 16, 20, 24, 28, 2, 6, 10, 14, 18, 22, 26, 30], 2: [1, 5, 9, 13, 3, 7, 11, 15, 0, 4, 8, 12, 2, 6, 10, 14], 4: [1, 5, 3, 7, 0, 4, 2, 6]}[n_bits]
    else:
        bit_order = {1: [16, 0, 24, 8, 17, 1, 25, 9, 18, 2, 26, 10, 19, 3, 27, 11, 20, 4, 28, 12, 21, 5, 29, 13, 22, 6, 30, 14, 23, 7, 31, 15], 2: [8, 0, 12, 4, 9, 1, 13, 5, 10, 2, 14, 6, 11, 3, 15, 7], 4: [4, 0, 6, 2, 5, 1, 7, 3]}[n_bits]
    x = x[:, bit_order]
    x = _pack(x, n_bits)
    x = x.reshape(-1, 4).flip(1)
    return x.flatten()
def _pack_tc_fpx(tensor: Tensor, nbits: int) -> Tensor:
    assert tensor.ndim == 2, tensor.dtype == torch.uint8
    M, N = tensor.shape
    assert M % 64 == 0 and N % 64 == 0
    tensor = tensor.view(M // 64, 4, 2, 8, N // 16, 2, 8)
    tensor = tensor.permute(0, 4, 1, 5, 2, 3, 6)
    tensor = tensor.reshape(-1, 32, 2)
    tensor = tensor.permute(1, 0, 2)
    tensor = tensor.flatten()
    used_bits = 0
    fragments = []
    for y in [1, 2, 4]:
        if nbits & y:
            mask = (1 << y) - 1
            tensor_ybit = tensor >> nbits - used_bits - y & mask
            tensor_ybit = _pack(tensor_ybit, y)
            tensor_ybit = tensor_ybit.view(32, -1, 4).permute(1, 0, 2).flip(2)
            tensor_ybit = _bit_interleave(tensor_ybit.flatten(), y)
            fragments.append(tensor_ybit)
            used_bits += y
    return torch.cat(fragments, dim=0).view(M, -1)
def _pack_tc_fp6(tensor: Tensor) -> Tensor:
    assert tensor.ndim == 2, tensor.dtype == torch.uint8
    M, N = tensor.shape
    assert M % 64 == 0 and N % 64 == 0
    tensor = tensor.view(M // 64, 2, 2, 2, 8, N // 16, 2, 8)
    tensor = tensor.flip(3)
    tensor_2bit = tensor >> 4 & 3
    tensor_2bit = tensor_2bit.permute(0, 5, 1, 4, 7, 3, 2, 6)
    tensor_2bit = _pack(tensor_2bit.flatten(), 2)
    tensor_4bit = tensor & 15
    tensor_4bit = tensor_4bit.permute(0, 5, 1, 2, 4, 7, 3, 6)
    tensor_4bit = _pack(tensor_4bit.flatten(), 4)
    return torch.cat([tensor_2bit, tensor_4bit], dim=0).view(M, -1)
def pack_tc_fpx(tensor: Tensor, nbits: int) -> Tensor:
    if nbits == 6:
        return _pack_tc_fp6(tensor)
    return _pack_tc_fpx(tensor, nbits)
def to_scaled_tc_fpx(tensor: Tensor, ebits: int, mbits: int) -> Tuple[Tensor, Tensor]:
    exp_bias = _ONES_TABLE[ebits - 1]
    max_normal = 2 ** (_ONES_TABLE[ebits] - exp_bias) * (_ONES_TABLE[mbits + 1] / 2 ** mbits)
    tensor = tensor.float()
    scale = tensor.abs().amax(1).clamp(min=1e-12) / max_normal
    tensor_fpx = _f32_to_fpx_unpacked(tensor / scale.view(-1, 1), ebits, mbits)
    tensor_tc_fpx = pack_tc_fpx(tensor_fpx, 1 + ebits + mbits)
    return (tensor_tc_fpx, scale.half())
def _unpack_tc_fpx(tensor: Tensor, nbits: int) -> Tensor:
    assert tensor.ndim == 2 and tensor.dtype == torch.uint8
    M = tensor.shape[0]
    size = tensor.numel()
    tensor = tensor.flatten()
    offset = 0
    used_bits = 0
    tensor_fpx = None
    for y in [1, 2, 4]:
        if nbits & y:
            size_ybit = size // nbits * y
            tensor_ybit = tensor[offset:offset + size_ybit]
            offset += size_ybit
            tensor_ybit = _bit_interleave(tensor_ybit, y, undo=True)
            tensor_ybit = tensor_ybit.view(-1, 32, 4).flip(2).permute(1, 0, 2)
            tensor_ybit = _unpack(tensor_ybit.flatten(), y)
            tensor_ybit = tensor_ybit << nbits - used_bits - y
            used_bits += y
            if tensor_fpx is None:
                tensor_fpx = tensor_ybit
            else:
                tensor_fpx |= tensor_ybit
    tensor_fpx = tensor_fpx.view(32, -1, 2).permute(1, 0, 2)
    tensor_fpx = tensor_fpx.reshape(M // 64, -1, 4, 2, 2, 8, 8)
    tensor_fpx = tensor_fpx.permute(0, 2, 4, 5, 1, 3, 6)
    tensor_fpx = tensor_fpx.reshape(M, -1)
    return tensor_fpx
def _unpack_tc_fp6(tensor: Tensor) -> Tensor:
    assert tensor.ndim == 2 and tensor.dtype == torch.uint8
    M = tensor.shape[0]
    N = tensor.shape[1] // 3 * 4
    assert M % 64 == 0 and N % 64 == 0
    size_2bit = M * N // 4
    size_4bit = M * N // 2
    tensor = tensor.view(-1)
    assert tensor.numel() == size_2bit + size_4bit
    tensor_2bit, tensor_4bit = tensor.split([size_2bit, size_4bit])
    tensor_2bit = _unpack(tensor_2bit, 2)
    tensor_2bit = tensor_2bit.view(M // 64, N // 16, 2, 8, 8, 2, 2, 2)
    tensor_2bit = tensor_2bit.permute(0, 2, 6, 5, 3, 1, 7, 4)
    tensor_4bit = _unpack(tensor_4bit, 4)
    tensor_4bit = tensor_4bit.view(M // 64, N // 16, 2, 2, 8, 8, 2, 2)
    tensor_4bit = tensor_4bit.permute(0, 2, 3, 6, 4, 1, 7, 5)
    tensor_fp6 = tensor_2bit << 4 | tensor_4bit
    tensor_fp6 = tensor_fp6.flip(3).reshape(M, N)
    return tensor_fp6
def unpack_tc_fpx(tensor: Tensor, nbits: int) -> Tensor:
    if nbits == 6:
        return _unpack_tc_fp6(tensor)
    return _unpack_tc_fpx(tensor, nbits)
def from_scaled_tc_fpx(tensor: Tensor, ebits: int, mbits: int, scale=None) -> Tensor:
    fpx_unpacked = unpack_tc_fpx(tensor, 1 + ebits + mbits)
    tensor = _fpx_unpacked_to_f32(fpx_unpacked, ebits, mbits)
    if scale is not None:
        tensor = tensor * scale.float().view(-1, 1)
    return tensor