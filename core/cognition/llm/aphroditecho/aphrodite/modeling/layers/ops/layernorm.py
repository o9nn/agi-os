import torch
import triton
import triton.language as tl
from aphrodite.modeling.layers.ops.utils import calculate_settings
@triton.jit
def _rms_layernorm_forward(Y, Y_row_stride, X, X_row_stride, W, W_row_stride, r, r_row_stride, n_cols, eps, BLOCK_SIZE: tl.constexpr):
    row_idx = tl.program_id(0)
    col_offsets = tl.arange(0, BLOCK_SIZE)
    mask = col_offsets < n_cols
    Y += row_idx * Y_row_stride
    X += row_idx * X_row_stride
    r += row_idx * r_row_stride
    X_row = tl.load(X + col_offsets, mask=mask, other=0).to(tl.float32)
    W_row = tl.load(W + col_offsets, mask=mask, other=0)
    row_var = tl.sum(X_row * X_row, axis=0) / n_cols
    inv_var = tl.math.rsqrt(row_var + eps)
    tl.store(r, inv_var)
    normed = X_row * inv_var
    normed = normed.to(W_row.dtype)
    output = normed * W_row
    tl.store(Y + col_offsets, output, mask=mask)
pass
@triton.jit
def _gemma_rms_layernorm_forward(Y, Y_row_stride, X, X_row_stride, W, W_row_stride, r, r_row_stride, n_cols, eps, BLOCK_SIZE: tl.constexpr):
    row_idx = tl.program_id(0)
    col_offsets = tl.arange(0, BLOCK_SIZE)
    mask = col_offsets < n_cols
    Y += row_idx * Y_row_stride
    X += row_idx * X_row_stride
    r += row_idx * r_row_stride
    X_row = tl.load(X + col_offsets, mask=mask, other=0).to(tl.float32)
    W_row = tl.load(W + col_offsets, mask=mask, other=0).to(tl.float32)
    row_var = tl.sum(X_row * X_row, axis=0) / n_cols
    inv_var = tl.math.rsqrt(row_var + eps)
    tl.store(r, inv_var)
    normed = X_row * inv_var
    output = normed * (W_row + 1.0)
    tl.store(Y + col_offsets, output, mask=mask)
pass
class Fast_RMS_Layernorm(torch.autograd.Function):
    @staticmethod
    def forward(ctx, X: torch.Tensor, W: torch.Tensor, eps: float, gemma: bool=False):
        shape = X.shape
        dim: int = shape[-1]
        X = X.view(-1, dim)
        n_rows: int
        n_cols: int
        n_rows, n_cols = X.shape
        BLOCK_SIZE: int
        num_warps: int
        BLOCK_SIZE, num_warps = calculate_settings(n_cols)
        Y = torch.empty((n_rows, n_cols), dtype=X.dtype, device=X.device)
        r = torch.empty(n_rows, dtype=torch.float32, device=X.device)
        fx = _gemma_rms_layernorm_forward if gemma else _rms_layernorm_forward
        with torch.cuda.device(X.device):
            fx[n_rows,](Y, Y.stride(0), X, X.stride(0), W, W.stride(0), r, r.stride(0), n_cols, eps, BLOCK_SIZE=BLOCK_SIZE, num_warps=num_warps)
        ctx.eps = eps
        ctx.BLOCK_SIZE = BLOCK_SIZE
        ctx.num_warps = num_warps
        ctx.GEMMA = gemma
        ctx.save_for_backward(X, W, r)
        return Y.view(*shape)
    pass
pass
@torch.compiler.disable
def fast_rms_layernorm(layernorm, X: torch.Tensor, gemma: bool=False):
    W: torch.Tensor = layernorm.weight
    eps: float = layernorm.variance_epsilon if hasattr(layernorm, 'variance_epsilon') else layernorm.eps
    out = Fast_RMS_Layernorm.apply(X, W, eps, gemma)
    return out
pass