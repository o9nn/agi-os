import torch
from aphrodite.v1.sample.metadata import SamplingMetadata
def xtc(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    xtc_threshold = sampling_metadata.xtc_threshold
    xtc_probability = sampling_metadata.xtc_probability
    if xtc_threshold is None or xtc_probability is None:
        return logits
    apply_xtc = torch.rand_like(xtc_probability) < xtc_probability
    if not apply_xtc.any():
        return logits
    probs = torch.softmax(logits, dim=-1)
    sorted_probs, sorted_indices = torch.sort(probs, descending=True, dim=-1)
    above_threshold = sorted_probs[..., 1:] >= xtc_threshold.unsqueeze(-1)
    for i in range(logits.shape[0]):
        if apply_xtc[i]:
            indices_to_remove = above_threshold[i].count_nonzero(dim=-1).item()
            if indices_to_remove > 0:
                logits[i].scatter_(0, sorted_indices[i, :indices_to_remove], -float('inf'))
    return logits