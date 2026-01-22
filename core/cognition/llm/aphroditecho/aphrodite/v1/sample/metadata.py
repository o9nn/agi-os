from dataclasses import dataclass
from typing import List, Optional
import torch
from aphrodite.common.sampling_params import SamplerID
from aphrodite.v1.sample.logits_processor import LogitsProcessorManager
@dataclass
class SamplingMetadata:
    temperature: Optional[torch.Tensor]
    dynatemp_min: Optional[torch.Tensor]
    dynatemp_max: Optional[torch.Tensor]
    dynatemp_exp: Optional[torch.Tensor]
    all_greedy: bool
    all_random: bool
    top_p: Optional[torch.Tensor]
    top_k: Optional[torch.Tensor]
    top_a: Optional[torch.Tensor]
    dry_multiplier: Optional[torch.Tensor]
    dry_base: Optional[torch.Tensor]
    dry_allowed_length: Optional[torch.Tensor]
    dry_sequence_breaker_ids: Optional[torch.Tensor]
    dry_ranges: Optional[torch.Tensor]
    dry_max_ngram: Optional[torch.Tensor]
    dry_max_occurrences: Optional[torch.Tensor]
    dry_early_exit_match_len: Optional[torch.Tensor]
    no_repeat_ngram_size: Optional[torch.Tensor]
    tfs: Optional[torch.Tensor]
    eta_cutoff: Optional[torch.Tensor]
    epsilon_cutoff: Optional[torch.Tensor]
    typical_p: Optional[torch.Tensor]
    quadratic_smoothing_factor: Optional[torch.Tensor]
    quadratic_smoothing_curve: Optional[torch.Tensor]
    xtc_threshold: Optional[torch.Tensor]
    xtc_probability: Optional[torch.Tensor]
    top_nsigma: Optional[torch.Tensor]
    skew: Optional[torch.Tensor]
    generators: dict[int, torch.Generator]
    max_num_logprobs: Optional[int]
    no_penalties: bool
    prompt_token_ids: Optional[torch.Tensor]
    frequency_penalties: torch.Tensor
    presence_penalties: torch.Tensor
    repetition_penalties: torch.Tensor
    output_token_ids: list[list[int]]
    allowed_token_ids_mask: Optional[torch.Tensor]
    bad_words_token_ids: dict[int, list[list[int]]]
    logitsprocs: LogitsProcessorManager
    sampler_priority: Optional[List[SamplerID]] = None
    temperature_last: bool = False