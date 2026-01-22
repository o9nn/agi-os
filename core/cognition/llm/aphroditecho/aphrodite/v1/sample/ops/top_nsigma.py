import torch
from aphrodite.v1.sample.metadata import SamplingMetadata
def top_nsigma(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    nsigma = sampling_metadata.top_nsigma
    if nsigma is None:
        return logits
    std = logits.std(dim=-1, keepdim=True)
    threshold = logits.max(dim=-1, keepdim=True).values - nsigma.unsqueeze(dim=1) * std
    logits[logits < threshold] = float('-inf')
    return logits