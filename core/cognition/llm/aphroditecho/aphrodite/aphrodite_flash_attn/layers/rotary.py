from typing import Optional, Tuple, Union
import torch
from einops import rearrange, repeat
from aphrodite.aphrodite_flash_attn.ops.triton.rotary import apply_rotary
def rotate_half(x, interleaved=False):
    if not interleaved:
        x1, x2 = x.chunk(2, dim=-1)
        return torch.cat((-x2, x1), dim=-1)
    else:
        x1, x2 = (x[..., ::2], x[..., 1::2])
        return rearrange(torch.stack((-x2, x1), dim=-1), '... d two -> ... (d two)', two=2)
def apply_rotary_emb_torch(x, cos, sin, interleaved=False):
    ro_dim = cos.shape[-1] * 2
    assert ro_dim <= x.shape[-1]
    cos = repeat(cos, '... d -> ... 1 (2 d)' if not interleaved else '... d -> ... 1 (d 2)')
    sin = repeat(sin, '... d -> ... 1 (2 d)' if not interleaved else '... d -> ... 1 (d 2)')
    return torch.cat([x[..., :ro_dim] * cos + rotate_half(x[..., :ro_dim], interleaved) * sin, x[..., ro_dim:]], dim=-1)
class ApplyRotaryEmb(torch.autograd.Function):
    @staticmethod
    def forward(ctx, x, cos, sin, interleaved=False, inplace=False, seqlen_offsets: Union[int, torch.Tensor]=0, cu_seqlens: Optional[torch.Tensor]=None, max_seqlen: Optional[int]=None):
        out = apply_rotary(x, cos, sin, seqlen_offsets=seqlen_offsets, cu_seqlens=cu_seqlens, max_seqlen=max_seqlen, interleaved=interleaved, inplace=inplace)
        if isinstance(seqlen_offsets, int):
            ctx.save_for_backward(cos, sin, cu_seqlens)
            ctx.seqlen_offsets = seqlen_offsets
        else:
            ctx.save_for_backward(cos, sin, cu_seqlens, seqlen_offsets)
            ctx.seqlen_offsets = None
        ctx.interleaved = interleaved
        ctx.inplace = inplace
        ctx.max_seqlen = max_seqlen
        return out if not inplace else x
    @staticmethod
    def backward(ctx, do):
        seqlen_offsets = ctx.seqlen_offsets
        if seqlen_offsets is None:
            cos, sin, cu_seqlens, seqlen_offsets = ctx.saved_tensors
        else:
            cos, sin, cu_seqlens = ctx.saved_tensors
        if not ctx.interleaved and (not ctx.inplace):
            do = do.clone()
        dx = apply_rotary(do, cos, sin, seqlen_offsets=seqlen_offsets, cu_seqlens=cu_seqlens, max_seqlen=ctx.max_seqlen, interleaved=ctx.interleaved, inplace=ctx.inplace, conjugate=True)
        return (dx, None, None, None, None, None, None, None)
def apply_rotary_emb(x, cos, sin, interleaved=False, inplace=False, seqlen_offsets: Union[int, torch.Tensor]=0, cu_seqlens: Optional[torch.Tensor]=None, max_seqlen: Optional[int]=None):
    return ApplyRotaryEmb.apply(x, cos, sin, interleaved, inplace, seqlen_offsets, cu_seqlens, max_seqlen)
apply_rotary_emb_func = apply_rotary_emb
class ApplyRotaryEmbQKV_(torch.autograd.Function):
    @staticmethod
    def forward(ctx, qkv, cos, sin, cos_k=None, sin_k=None, interleaved=False, seqlen_offsets: Union[int, torch.Tensor]=0, num_heads_q: Union[int]=None):
        if cos_k is None and sin_k is None and qkv.is_contiguous():
            if qkv.dim() == 5:
                batch, seqlen, three, nheads, headdim = qkv.shape
                assert three == 3
                qk = qkv[:, :, :2].reshape(batch, seqlen, -1, headdim)
            else:
                assert qkv.dim() == 4
                assert num_heads_q is not None
                num_heads_k = (qkv.shape[2] - num_heads_q) // 2
                assert qkv.shape[2] == num_heads_q + 2 * num_heads_k
                qk = qkv[:, :, :num_heads_q + num_heads_k]
            apply_rotary(qk, cos, sin, seqlen_offsets=seqlen_offsets, interleaved=interleaved, inplace=True)
        else:
            cos_k = cos if cos_k is None else cos_k
            sin_k = sin if sin_k is None else sin_k
            if qkv.dim() == 5:
                q, k = (qkv[:, :, 0], qkv[:, :, 1])
            else:
                assert qkv.dim() == 4
                assert num_heads_q is not None
                num_heads_k = (qkv.shape[2] - num_heads_q) // 2
                assert qkv.shape[2] == num_heads_q + 2 * num_heads_k
                q, k = (qkv[:, :, :num_heads_q], qkv[:, :, num_heads_q:num_heads_q + num_heads_k])
            apply_rotary(q, cos, sin, seqlen_offsets, interleaved=interleaved, inplace=True)
            apply_rotary(k, cos_k, sin_k, seqlen_offsets, interleaved=interleaved, inplace=True)
            ctx.save_for_backward(cos, sin, cos_k, sin_k)
        if isinstance(seqlen_offsets, int):
            ctx.save_for_backward(cos, sin, cos_k, sin_k)
            ctx.seqlen_offsets = seqlen_offsets
        else:
            ctx.save_for_backward(cos, sin, cos_k, sin_k, seqlen_offsets)
            ctx.seqlen_offsets = None
        ctx.interleaved = interleaved
        ctx.num_heads_q = num_heads_q
        return qkv
    @staticmethod
    def backward(ctx, dqkv):
        seqlen_offsets = ctx.seqlen_offsets
        if seqlen_offsets is None:
            cos, sin, cos_k, sin_k, seqlen_offsets = ctx.saved_tensors
        else:
            cos, sin, cos_k, sin_k = ctx.saved_tensors
        if cos_k is None and sin_k is None and dqkv.is_contiguous():
            if dqkv.dim() == 5:
                dqk = rearrange(dqkv[:, :, :2], 'b s t h d -> b s (t h) d')
            else:
                assert dqkv.dim() == 4
                assert ctx.num_heads_q is not None
                num_heads_k = (dqkv.shape[2] - ctx.num_heads_q) // 2
                assert dqkv.shape[2] == ctx.num_heads_q + 2 * num_heads_k
                dqk = dqkv[:, :, :ctx.num_heads_q + num_heads_k]
            apply_rotary(dqk, cos, sin, seqlen_offsets=seqlen_offsets, interleaved=ctx.interleaved, inplace=True, conjugate=True)
        else:
            cos_k = cos if cos_k is None else cos_k
            sin_k = sin if sin_k is None else sin_k
            if dqkv.dim() == 5:
                dq, dk = (dqkv[:, :, 0], dqkv[:, :, 1])
            else:
                assert dqkv.dim() == 4
                assert ctx.num_heads_q is not None
                num_heads_k = (dqkv.shape[2] - ctx.num_heads_q) // 2
                assert dqkv.shape[2] == ctx.num_heads_q + 2 * num_heads_k
                dq = dqkv[:, :, :ctx.num_heads_q]
                dk = dqkv[:, :, ctx.num_heads_q:ctx.num_heads_q + num_heads_k]
            apply_rotary(dq, cos, sin, seqlen_offsets, interleaved=ctx.interleaved, inplace=True, conjugate=True)
            apply_rotary(dk, cos_k, sin_k, seqlen_offsets, interleaved=ctx.interleaved, inplace=True, conjugate=True)
        return (dqkv, None, None, None, None, None, None, None)
def apply_rotary_emb_qkv_(qkv, cos, sin, cos_k=None, sin_k=None, interleaved=False, seqlen_offsets: Union[int, torch.Tensor]=0, num_heads_q: Optional[int]=None):
    return ApplyRotaryEmbQKV_.apply(qkv, cos, sin, cos_k, sin_k, interleaved, seqlen_offsets, num_heads_q)
class ApplyRotaryEmbKV_(torch.autograd.Function):
    @staticmethod
    def forward(ctx, kv, cos, sin, interleaved=False, seqlen_offsets: Union[int, torch.Tensor]=0):
        batch, seqlen, two, nheads, headdim = kv.shape
        assert two == 2
        k = kv[:, :, 0]
        apply_rotary(k, cos, sin, seqlen_offsets=seqlen_offsets, interleaved=interleaved, inplace=True)
        if isinstance(seqlen_offsets, int):
            ctx.save_for_backward(cos, sin)
            ctx.seqlen_offsets = seqlen_offsets
        else:
            ctx.save_for_backward(cos, sin, seqlen_offsets)
            ctx.seqlen_offsets = None
        ctx.interleaved = interleaved
        return kv
    @staticmethod
    def backward(ctx, dkv):
        seqlen_offsets = ctx.seqlen_offsets
        if seqlen_offsets is None:
            cos, sin, seqlen_offsets = ctx.saved_tensors
        else:
            cos, sin = ctx.saved_tensors
        apply_rotary(dkv[:, :, 0], cos, sin, seqlen_offsets=seqlen_offsets, interleaved=ctx.interleaved, inplace=True, conjugate=True)
        return (dkv, None, None, None, None)
apply_rotary_emb_kv_ = ApplyRotaryEmbKV_.apply
def apply_rotary_emb_kv_(kv, cos, sin, interleaved=False, seqlen_offsets: Union[int, torch.Tensor]=0):
    return ApplyRotaryEmbKV_.apply(kv, cos, sin, interleaved, seqlen_offsets)
class RotaryEmbedding(torch.nn.Module):
    def __init__(self, dim: int, base=10000.0, interleaved=False, scale_base=None, pos_idx_in_fp32=True, device=None):
        super().__init__()
        self.dim = dim
        self.base = float(base)
        self.pos_idx_in_fp32 = pos_idx_in_fp32
        inv_freq = self._compute_inv_freq(device)
        self.register_buffer('inv_freq', inv_freq, persistent=False)
        self.interleaved = interleaved
        self.scale_base = scale_base
        scale = (torch.arange(0, dim, 2, device=device, dtype=torch.float32) + 0.4 * dim) / (1.4 * dim) if scale_base is not None else None
        self.register_buffer('scale', scale, persistent=False)
        self._seq_len_cached = 0
        self._cos_cached = None
        self._sin_cached = None
        self._cos_k_cached = None
        self._sin_k_cached = None
    def _compute_inv_freq(self, device=None):
        return 1.0 / self.base ** (torch.arange(0, self.dim, 2, device=device, dtype=torch.float32) / self.dim)
    def _update_cos_sin_cache(self, seqlen, device=None, dtype=None):
        if seqlen > self._seq_len_cached or self._cos_cached is None or self._cos_cached.device != device or (self._cos_cached.dtype != dtype) or (self.training and self._cos_cached.is_inference()):
            self._seq_len_cached = seqlen
            if self.pos_idx_in_fp32:
                t = torch.arange(seqlen, device=device, dtype=torch.float32)
                if self.inv_freq.dtype != torch.float32:
                    inv_freq = self._compute_inv_freq(device=device)
                else:
                    inv_freq = self.inv_freq
            else:
                t = torch.arange(seqlen, device=device, dtype=self.inv_freq.dtype)
                inv_freq = self.inv_freq
            freqs = torch.outer(t, inv_freq)
            if self.scale is None:
                self._cos_cached = torch.cos(freqs).to(dtype)
                self._sin_cached = torch.sin(freqs).to(dtype)
            else:
                power = (torch.arange(seqlen, dtype=self.scale.dtype, device=self.scale.device) - seqlen // 2) / self.scale_base
                scale = self.scale.to(device=power.device) ** rearrange(power, 's -> s 1')
                self._cos_cached = (torch.cos(freqs) * scale).to(dtype)
                self._sin_cached = (torch.sin(freqs) * scale).to(dtype)
                self._cos_k_cached = (torch.cos(freqs) / scale).to(dtype)
                self._sin_k_cached = (torch.sin(freqs) / scale).to(dtype)
    def forward(self, qkv: torch.Tensor, kv: Optional[torch.Tensor]=None, seqlen_offset: Union[int, torch.Tensor]=0, max_seqlen: Optional[int]=None, num_heads_q: Optional[int]=None) -> Union[torch.Tensor, Tuple[torch.Tensor, torch.Tensor]]:
        seqlen = qkv.shape[1]
        if max_seqlen is not None:
            self._update_cos_sin_cache(max_seqlen, device=qkv.device, dtype=qkv.dtype)
        elif isinstance(seqlen_offset, int):
            self._update_cos_sin_cache(seqlen + seqlen_offset, device=qkv.device, dtype=qkv.dtype)
        if kv is None:
            if self.scale is None:
                return apply_rotary_emb_qkv_(qkv, self._cos_cached, self._sin_cached, interleaved=self.interleaved, seqlen_offsets=seqlen_offset, num_heads_q=num_heads_q)
            else:
                return apply_rotary_emb_qkv_(qkv, self._cos_cached, self._sin_cached, self._cos_k_cached, self._sin_k_cached, interleaved=self.interleaved, seqlen_offsets=seqlen_offset, num_heads_q=num_heads_q)
        else:
            q = qkv
            q = apply_rotary_emb_func(q, self._cos_cached, self._sin_cached, interleaved=self.interleaved, inplace=True, seqlen_offsets=seqlen_offset)
            if self.scale is None:
                kv = apply_rotary_emb_kv_(kv, self._cos_cached, self._sin_cached, interleaved=self.interleaved, seqlen_offsets=seqlen_offset)
            else:
                kv = apply_rotary_emb_kv_(kv, self._cos_k_cached, self._sin_k_cached, interleaved=self.interleaved, seqlen_offsets=seqlen_offset)
            return (q, kv)