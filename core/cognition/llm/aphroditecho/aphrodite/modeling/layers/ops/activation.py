import torch
import triton
import triton.language as tl
from packaging.version import Version
if Version(triton.__version__) >= Version('3.0.0'):
    from triton.language.extra import libdevice
    triton_tanh = libdevice.tanh
    triton_erf = libdevice.erf
    triton_sqrt = libdevice.sqrt
else:
    triton_tanh = tl.math.tanh
    triton_erf = tl.math.erf
    triton_sqrt = tl.math.sqrt
@triton.jit
def _fg_kernel(e, g, h, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    e_row = tl.load(e + offsets, mask=mask, other=0).to(tl.float32)
    g_row = tl.load(g + offsets, mask=mask, other=0)
    f_row = e_row * tl.sigmoid(e_row)
    f_row = f_row.to(g_row.dtype)
    output = f_row * g_row
    tl.store(h + offsets, output, mask=mask)
def swiglu_fg_kernel(e, g):
    squeeze = False
    if e.dim() == 2:
        e = e.unsqueeze(0)
        g = g.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = e.shape
    n_elements = batch * num_tokens * d
    h = torch.empty((batch, num_tokens, d), dtype=e.dtype, device=e.device)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(e.device):
        _fg_kernel[grid](e.reshape(-1), g.reshape(-1), h.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return h.squeeze(0)
    return h
@triton.jit
def _exact_gelu_kernel(e, g, h, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    e_row = tl.load(e + offsets, mask=mask, other=0).to(tl.float32)
    g_row = tl.load(g + offsets, mask=mask, other=0)
    f_row = 0.5 * e_row * (triton_erf(triton_sqrt(0.5) * e_row) + 1.0)
    f_row = f_row.to(g_row.dtype)
    output = f_row * g_row
    tl.store(h + offsets, output, mask=mask)
@triton.jit
def _approx_gelu_kernel(e, g, h, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    e_row = tl.load(e + offsets, mask=mask, other=0).to(tl.float32)
    g_row = tl.load(g + offsets, mask=mask, other=0)
    s = 0.7978845608028654
    f_row = 0.5 * e_row * (triton_tanh(s * e_row * (1.0 + 0.044715 * e_row * e_row)) + 1.0)
    f_row = f_row.to(g_row.dtype)
    output = f_row * g_row
    tl.store(h + offsets, output, mask=mask)
def geglu_exact_forward_kernel(e, g):
    squeeze = False
    if e.dim() == 2:
        e = e.unsqueeze(0)
        g = g.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = e.shape
    n_elements = batch * num_tokens * d
    h = torch.empty((batch, num_tokens, d), dtype=e.dtype, device=e.device)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(e.device):
        _exact_gelu_kernel[grid](e.reshape(-1), g.reshape(-1), h.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return h.squeeze(0)
    return h
def geglu_approx_forward_kernel(e, g):
    squeeze = False
    if e.dim() == 2:
        e = e.unsqueeze(0)
        g = g.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = e.shape
    n_elements = batch * num_tokens * d
    h = torch.empty((batch, num_tokens, d), dtype=e.dtype, device=e.device)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(e.device):
        _approx_gelu_kernel[grid](e.reshape(-1), g.reshape(-1), h.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return h.squeeze(0)
    return h
@triton.jit
def _gelu_new_kernel(x_ptr, output_ptr, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    x = tl.load(x_ptr + offsets, mask=mask).to(tl.float32)
    x3 = x * x * x
    c = 0.79788456
    t = triton_tanh(c * (x + 0.044715 * x3))
    output = 0.5 * x * (1.0 + t)
    tl.store(output_ptr + offsets, output, mask=mask)
def gelu_new_kernel(x: torch.Tensor) -> torch.Tensor:
    squeeze = False
    if x.dim() == 2:
        x = x.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = x.shape
    n_elements = batch * num_tokens * d
    output = torch.empty_like(x)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(x.device):
        _gelu_new_kernel[grid](x.reshape(-1), output.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return output.squeeze(0)
    return output
@triton.jit
def _fast_gelu_kernel(x_ptr, output_ptr, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    x = tl.load(x_ptr + offsets, mask=mask).to(tl.float32)
    c = 0.79788456
    inner = x * (1.0 + 0.044715 * x * x)
    t = triton_tanh(c * inner)
    output = 0.5 * x * (1.0 + t)
    tl.store(output_ptr + offsets, output, mask=mask)
@triton.jit
def _quick_gelu_kernel(x_ptr, output_ptr, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    x = tl.load(x_ptr + offsets, mask=mask).to(tl.float32)
    output = x * (1.0 / (1.0 + tl.exp(-1.702 * x)))
    tl.store(output_ptr + offsets, output, mask=mask)
def fast_gelu_kernel(x: torch.Tensor) -> torch.Tensor:
    squeeze = False
    if x.dim() == 2:
        x = x.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = x.shape
    n_elements = batch * num_tokens * d
    output = torch.empty_like(x)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(x.device):
        _fast_gelu_kernel[grid](x.reshape(-1), output.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return output.squeeze(0)
    return output
def quick_gelu_kernel(x: torch.Tensor) -> torch.Tensor:
    squeeze = False
    if x.dim() == 2:
        x = x.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = x.shape
    n_elements = batch * num_tokens * d
    output = torch.empty_like(x)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(x.device):
        _quick_gelu_kernel[grid](x.reshape(-1), output.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return output.squeeze(0)
    return output
@triton.jit
def _relu_squared_kernel(x_ptr, output_ptr, n_elements, BLOCK_SIZE: tl.constexpr):
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    x = tl.load(x_ptr + offsets, mask=mask).to(tl.float32)
    is_positive = x >= 0
    output = tl.where(is_positive, x * x, 0.0)
    tl.store(output_ptr + offsets, output, mask=mask)
def relu_squared_kernel(x: torch.Tensor) -> torch.Tensor:
    squeeze = False
    if x.dim() == 2:
        x = x.unsqueeze(0)
        squeeze = True
    batch, num_tokens, d = x.shape
    n_elements = batch * num_tokens * d
    output = torch.empty_like(x)
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    with torch.cuda.device(x.device):
        _relu_squared_kernel[grid](x.reshape(-1), output.reshape(-1), n_elements, BLOCK_SIZE=1024)
    if squeeze:
        return output.squeeze(0)
    return output