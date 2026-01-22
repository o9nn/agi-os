import torch
from aphrodite.v1.sample.metadata import SamplingMetadata
def quadratic(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    smoothing_factor = sampling_metadata.quadratic_smoothing_factor
    smoothing_curve = sampling_metadata.quadratic_smoothing_curve
    if smoothing_factor is None or smoothing_curve is None:
        return logits
    mask = smoothing_factor != 0
    k = (smoothing_factor * (3 - smoothing_curve) / 2).unsqueeze_(dim=1)
    s = (smoothing_factor * (smoothing_curve - 1) / 2).unsqueeze_(dim=1)
    quadlogits = logits[mask]
    max_logits = quadlogits.max(dim=-1, keepdim=True).values
    diff = quadlogits - max_logits
    diff -= diff ** 2 * (s[mask] * diff - k[mask])
    diff[diff != diff] = 0
    logits[mask] -= diff
    return logits