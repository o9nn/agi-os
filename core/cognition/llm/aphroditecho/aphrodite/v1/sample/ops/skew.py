import torch
from aphrodite.v1.sample.metadata import SamplingMetadata
def skew(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    skews = sampling_metadata.skew
    if skews is None:
        return logits
    probs = torch.softmax(logits, dim=-1)
    cum_probs = torch.cumsum(probs, dim=-1)
    cum_probs = torch.pow(cum_probs, torch.exp(skews).unsqueeze(dim=1))
    probs = torch.diff(cum_probs, dim=-1, prepend=torch.zeros_like(cum_probs[..., :1]))
    logits = torch.log(probs)
    return logits