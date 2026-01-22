from typing import Optional
import torch
import torch.nn as nn
from loguru import logger
from aphrodite.common import envs
from aphrodite.common.logger import log_once
from aphrodite.platforms import current_platform
try:
    import flashinfer.sampling
    is_flashinfer_available = True
except ImportError:
    is_flashinfer_available = False
try:
    from aphrodite.distributed.parallel_state import get_tensor_model_parallel_rank
except Exception:
    get_tensor_model_parallel_rank = lambda: 0
class TopKTopPSampler(nn.Module):
    def __init__(self):
        super().__init__()
        if current_platform.is_cuda():
            if is_flashinfer_available:
                flashinfer_version = flashinfer.__version__
                if flashinfer_version < '0.2.3':
                    if get_tensor_model_parallel_rank() == 0:
                        logger.info('FlashInfer version >= 0.2.3 required. Falling back to default sampling implementation.')
                    self.forward = self.forward_native
                elif envs.APHRODITE_USE_SAMPLING_KERNELS is not False:
                    logger.info('Using FlashInfer for top-p & top-k sampling.')
                    self.forward = self.forward_cuda
                else:
                    if get_tensor_model_parallel_rank() == 0:
                        logger.warning('FlashInfer is available, but it is not enabled. Falling back to the PyTorch-native implementation of top-p & top-k sampling. For the best performance, please set APHRODITE_USE_SAMPLING_KERNELS=1.')
                    self.forward = self.forward_native
            else:
                if get_tensor_model_parallel_rank() == 0:
                    logger.warning('FlashInfer is not available. Falling back to the PyTorch-native implementation of top-p & top-k sampling. For the best performance, please install FlashInfer.')
                self.forward = self.forward_native
        elif current_platform.is_tpu():
            self.forward = self.forward_tpu
        else:
            self.forward = self.forward_native
    def forward_native(self, logits: torch.Tensor, generators: dict[int, torch.Generator], k: Optional[torch.Tensor], p: Optional[torch.Tensor]) -> torch.Tensor:
        logits = apply_top_k_top_p(logits, k, p)
        probs = logits.softmax(dim=-1, dtype=torch.float32)
        return random_sample(probs, generators)
    def forward_cuda(self, logits: torch.Tensor, generators: dict[int, torch.Generator], k: Optional[torch.Tensor], p: Optional[torch.Tensor]) -> torch.Tensor:
        if k is None and p is None:
            probs = logits.softmax(dim=-1, dtype=torch.float32)
            return random_sample(probs, generators)
        if generators:
            log_once('WARNING', 'FlashInfer 0.2.3+ does not support per-request generators. Falling back to PyTorch-native implementation.')
            return self.forward_native(logits, generators, k, p)
        return flashinfer_sample(logits.contiguous(), k, p, generators)
    def forward_tpu(self, logits: torch.Tensor, generators: dict[int, torch.Generator], k: Optional[torch.Tensor], p: Optional[torch.Tensor]) -> torch.Tensor:
        logits = apply_top_k_top_p_tpu(logits, k, p)
        probs = logits.softmax(dim=-1, dtype=torch.float32)
        return random_sample(probs, generators)
def apply_top_k_top_p_tpu(logits: torch.Tensor, k: torch.Tensor, p: torch.Tensor) -> torch.Tensor:
    probs = logits.softmax(dim=-1)
    probs_sort, _ = probs.sort(dim=-1, descending=False)
    if k is not None:
        top_k_count = probs_sort.size(1) - k.to(torch.long)
        top_k_count = top_k_count.unsqueeze(dim=1)
        top_k_cutoff = probs_sort.gather(-1, top_k_count)
        no_top_k_mask = (k == logits.shape[1]).unsqueeze(dim=1)
        top_k_cutoff.masked_fill_(no_top_k_mask, -float('inf'))
        elements_to_discard = probs < top_k_cutoff
        logits.masked_fill_(elements_to_discard, -float('inf'))
    if p is not None:
        cumprob = torch.cumsum(probs_sort, dim=-1)
        top_p_mask = cumprob <= 1 - p.unsqueeze(dim=1)
        top_p_mask[:, -1] = False
        top_p_count = top_p_mask.sum(dim=-1).unsqueeze(1)
        top_p_cutoff = probs_sort.gather(-1, top_p_count)
        elements_to_discard = probs < top_p_cutoff
        logits.masked_fill_(elements_to_discard, -float('inf'))
    return logits
def apply_top_k_top_p(logits: torch.Tensor, k: Optional[torch.Tensor], p: Optional[torch.Tensor]) -> torch.Tensor:
    if p is None:
        if k is None:
            return logits
        return apply_top_k_only(logits, k)
    logits_sort, logits_idx = logits.sort(dim=-1, descending=False)
    if k is not None:
        top_k_mask = logits_sort.size(1) - k.to(torch.long)
        top_k_mask = logits_sort.gather(1, top_k_mask.unsqueeze(dim=1))
        top_k_mask = logits_sort < top_k_mask
        logits_sort.masked_fill_(top_k_mask, -float('inf'))
    if p is not None:
        probs_sort = logits_sort.softmax(dim=-1)
        probs_sum = torch.cumsum(probs_sort, dim=-1, out=probs_sort)
        top_p_mask = probs_sum <= 1 - p.unsqueeze(dim=1)
        top_p_mask[:, -1] = False
        logits_sort.masked_fill_(top_p_mask, -float('inf'))
    logits = logits_sort.scatter(dim=-1, index=logits_idx, src=logits_sort)
    return logits
def apply_top_k_only(logits: torch.Tensor, k: torch.Tensor) -> torch.Tensor:
    no_top_k_mask = k == logits.shape[1]
    k = k.masked_fill(no_top_k_mask, 1)
    max_top_k = k.max()
    k_index = k.sub_(1).unsqueeze(1)
    top_k_mask = logits.topk(max_top_k, dim=1).values.gather(1, k_index.long())
    top_k_mask.masked_fill_(no_top_k_mask.unsqueeze(1), -float('inf'))
    logits.masked_fill_(logits < top_k_mask, -float('inf'))
    return logits
def random_sample(probs: torch.Tensor, generators: dict[int, torch.Generator]) -> torch.Tensor:
    q = torch.empty_like(probs)
    if len(generators) != probs.shape[0]:
        q.exponential_()
    if generators:
        for i, generator in generators.items():
            q[i].exponential_(generator=generator)
    return probs.div_(q).argmax(dim=-1).view(-1)
def flashinfer_sample(logits: torch.Tensor, k: Optional[torch.Tensor], p: Optional[torch.Tensor], generators: dict[int, torch.Generator]) -> torch.Tensor:
    assert not (k is None and p is None)
    if k is None:
        probs = logits.softmax(dim=-1, dtype=torch.float32)
        next_token_ids = flashinfer.sampling.top_p_sampling_from_probs(probs, p, deterministic=True)
    elif p is None:
        probs = logits.softmax(dim=-1, dtype=torch.float32)
        next_token_ids = flashinfer.sampling.top_k_sampling_from_probs(probs, k, deterministic=True)
    else:
        next_token_ids = flashinfer.sampling.top_k_top_p_sampling_from_logits(logits, k, p, deterministic=True)
    return next_token_ids.view(-1)