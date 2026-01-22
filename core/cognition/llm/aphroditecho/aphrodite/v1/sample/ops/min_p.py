import torch
from aphrodite.v1.sample.metadata import SamplingMetadata
def min_p(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    min_p = sampling_metadata.min_p
    if min_p is None:
        return logits
    probability_values = torch.nn.functional.softmax(logits, dim=-1)
    max_probabilities = torch.amax(probability_values, dim=-1, keepdim=True)
    adjusted_min_p = min_p.unsqueeze(1) * max_probabilities
    valid_token_mask = probability_values >= adjusted_min_p
    logits[~valid_token_mask] = -float('inf')
    return logits